//  written by Philippe Wechsler 2008
//
//  web: www.PhilippeWechsler.ch
//  mail: contact@PhilippeWechsler.ch
//
//  please see license.txt and documentation.txt
//
//  changes in 2.0 stable (2025, Baz Cuda - https://github.com/BazzaCuda)
//  - support for both the 32-bit and 64-bit versions of unrar.dll
//  - switched to using the ...Ex functions in unrar.dll to get added info
//  - each file's checksum (e.g. Blake2) is now accessible
//  - support for WideChar filenames in archives
//  - reformatted the code and added significant amounts of whitespace for enhanced readability
//  - completely refactored
//  - started a GoFundMe page to buy Philippe a keyboard with a space bar :D
//
//  changes in 1.2 stable
//   - support for delphi 2009
//   - support for unicode filenames (see TRARFileItem.FileNameW)
//   - dll name + path is custom
//   - fixed a memory leak (thanks to Claes Enskär)
//   - some small improvements in the demo
//  changes in 1.1 stable
//   - fixed problem with mySelf pointer - you can use now multiple TRAR instances
//   - "SFX" in archive informations
//   - code better commented
//   - bugfixing in reading multivolumes
//
//  known bugs:
//   - when extracting files that contains unicode characters there's no test if
//     the file exists already
//   - open archives that contains unicode characters in the archive name fails

unit RAR;

interface

uses
  classes, sysUtils, windows,
  vcl.forms,
  RAR_DLL;

type
  TRAROperation   = (roOpenArchive, roCloseArchive, roListFiles, roExtract, roTest);
  TRARHeaderType  = (htFile, htDirectory);
  TRAROpenMode    = (omRAR_OM_LIST, omRAR_OM_EXTRACT, omRAR_OM_LIST_INCSPLIT);
  TRARReplace     = (rrCancel, rrOverwrite, rrSkip);

  TRARProgressInfo = record
    fileName:           WideString;
    archiveBytesTotal:  LongInt;
    archiveBytesDone:   LongInt;
    fileBytesTotal:     LongInt;
    fileBytesDone:      LongInt;
  end;

  TRARFileItem = record
    // derived from processFileHeader(aHeaderDataEx: TRARHeaderDataEx)
    fileName:             AnsiString;
    fileNameW:            WideString;
    compressedSize:       cardinal;
    unCompressedSize:     cardinal;
    hostOS:               string;
    CRC32:                string;
    attributes:           cardinal;
    comment:              AnsiString;
    time:                 TDateTime;
    compressionStrength:  cardinal;
    archiverVersion:      cardinal;
    encrypted:            boolean;
    blake2:               string;
  end;

  TRARReplaceData = record
    fileName: Ansistring;
    size:     int64;
    time:     TDateTime;
  end;

  TRAROnErrorNotifyEvent    = procedure(Sender: TObject; const aErrorCode: integer; const aOperation: TRAROperation) of object;
  TRAROnListFile            = procedure(Sender: TObject; const aFileInformation: TRARFileItem) of object;
  TRAROnPasswordRequired    = procedure(Sender: TObject; const aHeaderPassword: boolean; const aFileName: Ansistring; out oNewPassword: Ansistring; out oCancel: boolean) of object;
  TRAROnNextVolumeRequired  = procedure(Sender: TObject; const aRequiredFileName: Ansistring; out oNewFileName: Ansistring; out oCancel: boolean) of object;
  TRAROnProgress            = procedure(Sender: TObject; const aProgressInfo: TRARProgressInfo) of object;
  TRAROnReplace             = procedure(Sender: TObject; const aExistingData:TRARReplaceData; aNewData:TRARReplaceData; out oAction: TRARReplace) of object;

  TRARArchiveInfo = record
    fileName:             Ansistring;
    fileNameW:            WideString;

    // derived from RAROpenArchiveEx: TRAROpenArchiveDataEx in openArchive()
    locked:               boolean;
    signed:               boolean;
    recovery:             boolean;
    solid:                boolean; // also set in processFileHeader(aHeaderDataEx: TRARHeaderDataEx)
    headerEncrypted:      boolean;
    SFX:                  boolean;
    archiveComment:       boolean;

    // derived from processFileHeader(aHeaderDataEx: TRARHeaderDataEx);
    packedSizeMVVolume:   cardinal; // from processFileHeader(aHeaderDataEx: TRARHeaderDataEx)
    archiverMajorVersion: cardinal;
    archiverMinorVersion: cardinal;
    hostOS:               string;
    totalFiles:           integer; // incremented when processing each file header
    dictionarySize:       int64;
    compressedSize:       int64;   // totalled from aHeaderDataEx.PackSize when processing each file header
    unCompressedSize:     int64;   // totalled from aHeaderDataEx.UnpSize  when processing eacah file header
    multiVolume:          boolean;
    fileComment:          boolean;
  end;

  TRARArchive = class(TObject)
    handle:               THandle;
    opened:               boolean;
    password:             AnsiString;
    info:                 TRARArchiveInfo;
    fileItem:             TRARFileItem;
    progressInfo:         TRARProgressInfo;
    hasComment:           boolean;
    comment:              AnsiString;
    commentBuf:           PAnsiChar;
    commentState:         cardinal;
    ReadMVToEnd:          boolean;
    abort:                boolean;
  public
    constructor create;
    destructor  destroy; override;
  end;

  ICallBackInfo = interface
  ['{9D062B91-12D3-47E2-821A-215938DAD3CE}']
    function  getOnPasswordRequired: TRAROnPasswordRequired;
    function  getOnProgress: TRAROnProgress;
    function  getRAR: TRARArchive;
    procedure setOnPasswordRequired(const Value: TRAROnPasswordRequired);
    procedure setOnProgress(const Value: TRAROnProgress);
    procedure setRAR(const Value: TRARArchive);
    property  RAR:                  TRARArchive             read getRAR                 write setRAR;
    property  onPasswordRequired:   TRAROnPasswordRequired  read getOnPasswordRequired  write setOnPasswordRequired;
    property  onProgress:           TRAROnProgress          read getOnProgress          write setOnProgress;
  end;

  TCallBackInfo = class(TInterfacedObject, ICallBackInfo)
  strict private
    FRAR:                 TRARArchive;
    FOnPasswordRequired:  TRAROnPasswordRequired;
    FOnProgress:          TRAROnProgress;
  private
    function  getOnPasswordRequired: TRAROnPasswordRequired;
    function  getOnProgress:         TRAROnProgress;
    function  getRAR:                TRARArchive;
    procedure setOnPasswordRequired(const Value: TRAROnPasswordRequired);
    procedure setOnProgress(const Value: TRAROnProgress);
    procedure setRAR(const Value: TRARArchive);
  public
    property  RAR:                  TRARArchive             read getRAR                 write setRAR;
    property  onPasswordRequired:   TRAROnPasswordRequired  read getOnPasswordRequired  write setOnPasswordRequired;
    property  onProgress:           TRAROnProgress          read getOnProgress          write setOnProgress;
  end;

  IRARResult = interface
  ['{8CBBE61B-C5F8-4FFD-96D0-32C29CC05AAB}']
    function  checkRARResult(const aResultCode: integer; const aOperation: TRAROperation): integer;
    function  getLastResult: integer;
    function  getOnError:    TRAROnErrorNotifyEvent;
    procedure setOnError(const Value: TRAROnErrorNotifyEvent);
    property  lastResult:    integer read getLastResult;
    property  onError:       TRAROnErrorNotifyEvent  read getOnError         write setOnError;
  end;

  TRARResult = class(TInterfacedObject, IRARResult)
  strict private
    FOnError:     TRAROnErrorNotifyEvent;
    FLastResult:  integer;
  public
    function  checkRARResult(const aResultCode: integer; const aOperation: TRAROperation): integer;
    function  getLastResult:  integer;
    function  getOnError:     TRAROnErrorNotifyEvent;
    procedure setOnError(const Value: TRAROnErrorNotifyEvent);

    property  lastResult:     integer                 read getLastResult;
    property  onError:        TRAROnErrorNotifyEvent  read getOnError         write setOnError;
  end;

type
  TRAR = class(TComponent)
  strict private
    FRAR:                   TRARArchive;

    FPassword:              AnsiString;

    FOnListFile:            TRAROnListFile;
    FOnPasswordRequired:    TRAROnPasswordRequired;
    FOnNextVolumeRequired:  TRAROnNextVolumeRequired;
    FOnProgress:            TRAROnProgress;
    FOnReplace:             TRAROnReplace;

    FReadMVToEnd:           boolean;

    function  getOnError:   TRAROnErrorNotifyEvent;
    procedure setOnError(const Value: TRAROnErrorNotifyEvent);

    function  getDLLName: string;
    function  getVersion:string;

    procedure onRARProgressTest(Sender: TObject; const aProgressInfo: TRARProgressInfo);

    function  getArchiveInfo: TRARArchiveInfo;
    function  getDLLVersion:  integer;
  public
    constructor create(AOwner: TComponent);
    destructor  destroy; override;

    procedure abort;
  private
    function  getReadMVToEnd: boolean;
    procedure setReadMVToEnd(const Value: boolean);
    function getPassword: AnsiString;
    procedure setPassword(const Value: AnsiString);

  public
    function  listArchive(const aFilePath:string):  boolean;
    function  testArchive(const aFilePath:string):  boolean;

  published
    property version: string read getVersion;
    property readMultiVolumeToEnd:  boolean                   read getReadMVToEnd         write setReadMVToEnd; //if true, mv's will be read until last part of the file
    //pro: display correct crc + display all files in all parts
    //con: all volumes required means to open you have to insert all disk if not all volumes in same folder
    property DLLName:               string                    read getDLLName;
    property DLLVersion:            integer                   read getDLLVersion;

    property onError:               TRAROnErrorNotifyEvent    read getOnError             write setOnError;
    property onListFile:            TRAROnListFile            read FOnListFile            write FOnListFile;
    property onPasswordRequired:    TRAROnPasswordRequired    read FOnPasswordRequired    write FOnPasswordRequired;
    property onNextVolumeRequired:  TRAROnNextVolumeRequired  read FOnNextVolumeRequired  write FOnNextVolumeRequired;
    property onProgress:            TRAROnProgress            read FOnProgress            write FOnProgress;
    property onReplace:             TRAROnReplace             read FOnReplace             write FOnReplace;

    property archiveInfo:           TRARArchiveInfo           read getArchiveInfo;
    property password:              AnsiString                read getPassword            write setPassword; // can be supplied by the user before calling an operation
  end;

procedure Register;

implementation

uses
  _debugWindow;

const
  GVersion='2.0';

var
  RR: IRARResult;

procedure Register;
begin
  RegisterComponents('Philippe Wechsler', [TRAR]);
end;

function checkRARResult(const aResultCode:integer; const aOperation: TRAROperation): integer;
begin
  result := RR.checkRARResult(aResultCode, aOperation);
end;

function UnRarCallBack(msg: cardinal; userData: LPARAM; P1: LPARAM; P2: LPARAM): integer; {$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};
var
  vCancel:        boolean;
  vPasswordFile:  AnsiString;
  vPassword:      AnsiString;
begin
  vCancel := FALSE;
  result  := RAR_CONTINUE;
  var vCBI := ICallbackInfo(userData);

  try

    case msg of
      UCM_NEEDPASSWORD: begin
                          case assigned(vCBI.onPasswordRequired) of TRUE: vCBI.onPasswordRequired(vCBI.RAR, NOT vCBI.RAR.opened, vPasswordFile, vPassword, vCancel); end;
                          case vCancel of TRUE: result := RAR_CANCEL; end;
                          strPCopy(Pointer(P1), copy(vPassword, 1, P2)); // P1 = pointer to the password buffer in unrar; P2 = maximum size of the buffer
                        end;

      UCM_PROCESSDATA:  begin
                          vCBI.RAR.progressInfo.ArchiveBytesDone  := vCBI.RAR.progressInfo.ArchiveBytesDone + P2;
                          vCBI.RAR.progressInfo.FileBytesDone     := vCBI.RAR.progressInfo.FileBytesDone    + P2;

                          case assigned(vCBI.onProgress) of TRUE: vCBI.onProgress(vCBI.RAR, vCBI.RAR.progressInfo); end;
                          case vCBI.RAR.abort of TRUE: result := RAR_CANCEL; end;
                        end;
    end;

  except
    MessageBox(0, 'It go Bang!', 'Bang!', MB_ICONEXCLAMATION or MB_OK);
  end;
end;


function closeArchive(const aArchiveHandle: THANDLE): boolean;
begin
  case aArchiveHandle = RAR_INVALID_HANDLE of FALSE: result := checkRARResult(RARCloseArchive(aArchiveHandle), roCloseArchive) = RAR_SUCCESS; end;
end;

function getOpenMode(bReadMVToEnd: boolean): TRAROpenMode;
begin
  case bReadMVToEnd of  TRUE: result := omRAR_OM_LIST_INCSPLIT;
                       FALSE: result := omRAR_OM_LIST; end;
end;

procedure initArchive(const aRAR: TRARArchive);
begin
  with aRAR do begin
    handle        := 0;
    opened        := FALSE;
    info          := default(TRARArchiveInfo);
    fileItem      := default(TRARFileItem);
    progressInfo  := default(TRARProgressInfo);
    hasComment    := FALSE;
    comment       := '';
    commentState  := 0;
    abort         := FALSE;
  end;
end;

function initCallBack(const aRAR: TRARArchive; aOnProgress: TRAROnProgress = NIL; aOnPasswordRequired: TRAROnPasswordRequired = NIL): ICallBackInfo;
begin
  result                    := TCallBackInfo.create;
  result.RAR                := aRAR;
  result.onProgress         := aOnProgress;
  result.onPasswordRequired := aOnPasswordRequired;
  RARSetCallback(aRAR.handle, UnRarCallBack, LPARAM(result));
end;

function processOpenArchive(const aOpenArchiveDataEx: TRAROpenArchiveDataEx; const aRAR: TRARArchive): boolean;
begin
  result := FALSE;

  //((ArchiveData.Flags and $00000100)=$00000100)=first volume
  //((ArchiveData.Flags and $00000001)=$00000001)=Volume attribute (archive volume)
  //((ArchiveData.Flags and $00000010)=$00000010)=New volume naming scheme ('volname.partN.rar')

  //set archive info
  if ((aOpenArchiveDataEx.Flags AND $00000002) = $00000002) then aRAR.info.archiveComment   := TRUE; // unrar doesn't seem to set this
  if ((aOpenArchiveDataEx.Flags AND $00000004) = $00000004) then aRAR.info.locked           := TRUE;
  if ((aOpenArchiveDataEx.Flags AND $00000008) = $00000008) then aRAR.info.solid            := TRUE;
  if ((aOpenArchiveDataEx.Flags AND $00000020) = $00000020) then aRAR.info.signed           := TRUE;
  if ((aOpenArchiveDataEx.Flags AND $00000040) = $00000040) then aRAR.info.recovery         := TRUE; // unrar doesn't seem to set this
  if ((aOpenArchiveDataEx.Flags AND $00000080) = $00000080) then aRAR.info.headerEncrypted  := TRUE; // it does set this

  aRAR.info.SFX := isSFX(aRAR.info.fileName);

  case aOpenArchiveDataEx.cmtState of // read archive comment - cmtState is actually aRAR.commentState
    RAR_COMMENT_EXISTS:   begin
                            aRAR.comment := strPas(aRAR.commentBuf);
                            aRAR.hasComment := TRUE;
                          end;
    RAR_NO_COMMENT:       begin
                            aRAR.comment := '';
                            aRAR.hasComment := FALSE;
                          end;
    ERAR_NO_MEMORY:       checkRARResult(ERAR_NO_MEMORY,       roOpenArchive);
    ERAR_BAD_DATA:        checkRARResult(ERAR_BAD_DATA,        roOpenArchive);
    ERAR_UNKNOWN_FORMAT:  checkRARResult(ERAR_UNKNOWN_FORMAT,  roOpenArchive);
    ERAR_SMALL_BUF:       checkRARResult(ERAR_SMALL_BUF,       roOpenArchive);
  end;

  case aOpenArchiveDataEx.cmtState in [RAR_NO_COMMENT, RAR_COMMENT_EXISTS] of FALSE: checkRARResult(RAR_COMMENT_UNKNOWN, roOpenArchive); end; // unknown comment condition

  result := TRUE;
end;

function openArchive(const aFilePath: string; const aOpenMode: TRAROpenMode; const aRAR: TRARArchive; bInitArchive: boolean): boolean;
begin
  case bInitArchive of TRUE: initArchive(aRAR); end;

  aRAR.info.fileName  := aFilePath;
  aRAR.info.fileNameW := aFilePath;

  result := FALSE;

  var vOpenArchiveDataEx := default(TRAROpenArchiveDataEx);
  with vOpenArchiveDataEx do begin

    arcName     := PAnsiChar(aRAR.info.fileName);
    arcNameW    := PWideChar(aRAR.info.fileNameW);
    openMode    := ord(aOpenMode);

    cmtBuf      := aRAR.commentBuf;
    cmtBufSize  := RAR_MAX_COMMENT_SIZE;
    cmtSize     := length(aRAR.commentBuf);
    cmtState    := aRAR.commentState; // feedback variable
  end;

  aRAR.handle   := RAROpenArchiveEx(@vOpenArchiveDataEx);
  aRAR.opened   := aRAR.handle <> RAR_INVALID_HANDLE;
  case aRAR.handle = RAR_INVALID_HANDLE of TRUE:  begin
                                                    checkRARResult(ERAR_EOPEN, roOpenArchive);
                                                    EXIT; end;end;

  result := processOpenArchive(vOpenArchiveDataEx, aRAR);
end;

function processFileHeader(const aFileHeaderDataEx: TRARHeaderDataEx; const aRAR: TRARArchive): TRARHeaderType; // populate FArchiveInfo: TRARArchiveInfo and vFileItem: TRARFileItem from aHeaderDataEx
var
  ft:         _FILETIME;
  st:         TSystemTime;
  OS:         string;

  function binToStr(const bin: array of byte): string;
  begin
    setLength(result, 2 * length(bin));
    for var i := low(bin) to high(bin) do begin
      result[i * 2 + 1] := lowerCase(intToHex(bin[i], 2))[1];
      result[i * 2 + 2] := lowerCase(intToHex(bin[i], 2))[2];
    end;
  end;

begin
  // first part of the file
  case (aRAR.readMVToEnd) and (NOT ((aFileHeaderDataEx.Flags AND $00000001) = $00000001)) and (((aFileHeaderDataEx.Flags AND $00000002) = $00000002))
    of TRUE: aRAR.info.packedSizeMVVolume := aFileHeaderDataEx.PackSize; end;

  // NOT last, NOT first part
  case (aRAR.readMVToEnd) and (((aFileHeaderDataEx.Flags AND $00000001) = $00000001))
                          and (((aFileHeaderDataEx.Flags AND $00000002) = $00000002))
    of TRUE:  begin
                inc(aRAR.info.packedSizeMVVolume, aFileHeaderDataEx.PackSize);
                EXIT; end;end;

  // last part
  case (aRAR.readMVToEnd) and     (((aFileHeaderDataEx.Flags AND $00000001) = $00000001))
                          and (NOT ((aFileHeaderDataEx.Flags AND $00000002) = $00000002))
    of TRUE: inc(aRAR.info.packedSizeMVVolume, aFileHeaderDataEx.PackSize); end;

  // NOT last part
  case (aRAR.readMVToEnd) and ((aFileHeaderDataEx.Flags AND $00000002) = $00000002)
    of TRUE: EXIT; end;

  case aRAR.info.archiverMajorVersion * 10 + aRAR.info.archiverMinorVersion < aFileHeaderDataEx.UnpVer
    of TRUE:  begin
                aRAR.info.archiverMinorVersion := aFileHeaderDataEx.UnpVer mod 10;
                aRAR.info.archiverMajorVersion :=(aFileHeaderDataEx.UnpVer - aRAR.info.archiverMinorVersion) div 10; end;end;

  case ((aFileHeaderDataEx.Flags AND $00000010) = $00000010)
    of TRUE: aRAR.info.solid := TRUE; end;

  OS:='unknown';
  case aFileHeaderDataEx.HostOS of
    0: OS:='DOS';
    1: OS:='IBM OS/2';
    2: OS:='Windows';
    3: OS:='Unix';
  end;
  aRAR.info.HostOS := OS;

  result := htDirectory;
  case (aFileHeaderDataEx.fileAttr AND faDirectory) = faDirectory of TRUE: EXIT; end;
  result := htFile;

  inc(aRAR.info.totalFiles);
  aRAR.info.dictionarySize := aFileHeaderDataEx.dictSize;

  inc(aRAR.info.compressedSize,   aFileHeaderDataEx.packSize);
  inc(aRAR.info.unCompressedSize, aFileHeaderDataEx.unpSize);

  case ((aFileHeaderDataEx.Flags AND $00000001) = $00000001) or ((aFileHeaderDataEx.Flags AND $00000002) = $00000002)
    of TRUE: aRAR.info.multiVolume := TRUE; end; // file continued in last or next part

  aRAR.fileItem := default(TRARFileItem);
  with aRAR.fileItem do begin
    fileName          := strPas(aFileHeaderDataEx.fileName);
    fileNameW         := aFileHeaderDataEx.fileNameW;
    compressedSize    := aFileHeaderDataEx.packSize;
    unCompressedSize  := aFileHeaderDataEx.unpSize;
    hostOS            := OS;
    CRC32             := format('%x',[aFileHeaderDataEx.fileCRC]);
    attributes        := aFileHeaderDataEx.fileAttr;
    comment           := aFileHeaderDataEx.cmtBuf;

    dosDateTimeToFileTime(hiWord(aFileHeaderDataEx.fileTime), loWord(aFileHeaderDataEx.fileTime), ft);
    fileTimeToSystemTime(ft, st);
    time := systemTimeToDateTime(st);

    compressionStrength := aFileHeaderDataEx.method;
    archiverVersion     := aFileHeaderDataEx.unpVer;
    encrypted           := (aFileHeaderDataEx.flags AND $00000004) = $00000004;
    blake2              := binToStr(aFileHeaderDataEx.hash);
  end;
end;

function testArchiveFiles(const aRAR: TRARArchive): boolean;
begin
  var vHeaderDataEx := default(TRARHeaderDataEx);
  aRAR.progressInfo := default(TRARProgressInfo);

  aRAR.progressInfo.ArchiveBytesTotal := aRAR.info.unCompressedSize;

  try
    repeat
      case checkRARResult(RARReadHeaderEx(aRAR.handle, @vHeaderDataEx), roTest)     = RAR_SUCCESS of FALSE: EXIT; end;  // get the next file header in the archive
      processFileHeader(vHeaderDataEx, aRAR);

      aRAR.progressInfo.FileBytesDone   := 0;
      aRAR.progressInfo.FileBytesTotal  := vHeaderDataEx.UnpSize; // aRAR.fileItem.unCompressedSize
      aRAR.progressInfo.FileName        := vHeaderDataEx.FileNameW; // aRAR.info.fileNameW

      case checkRARResult(RARProcessFile(aRAR.handle, RAR_TEST, NIL, NIL), roTest)  = RAR_SUCCESS of FALSE: EXIT; end;

      application.processMessages; // allow the user to actually press a cancel button
    until aRAR.abort;              // RARReadHeaderEx = ERAR_END_ARCHIVE will usually exit the loop

  finally
    result := RR.lastResult = ERAR_END_ARCHIVE; // not an error in this case
  end;
end;

function listArchiveFiles(const aRAR: TRARArchive; bNotify: boolean = TRUE; const aOnListFile: TRAROnListFile = NIL): boolean;
begin
  var vHeaderDataEx := default(TRARHeaderDataEx);

  try
    repeat
      case checkRARResult(RARReadHeaderEx(aRAR.handle, @vHeaderDataEx), roListFiles)     = RAR_SUCCESS  of FALSE: EXIT; end;  // get the next file header in the archive
      case (processFileHeader(vHeaderDataEx, aRAR) = htFile) and bNotify and assigned(aOnListFile)      of  TRUE: aOnListFile(aRAR, aRAR.fileItem); end;
      case checkRARResult(RARProcessFile(aRAR.handle, RAR_SKIP, NIL, NIL), roListFiles)  = RAR_SUCCESS  of FALSE: EXIT; end;  // do nothing - skip to next file header

      application.processMessages;  // allow the user to actually press a cancel button
    until aRAR.abort;               // RARReadHeaderEx = ERAR_END_ARCHIVE will usually exit the loop

  finally
    result := RR.lastResult = ERAR_END_ARCHIVE; // not an error in this case
  end;
end;

function listFiles(const aFilePath: string; aRAR: TRARArchive; aOnListFile: TRAROnListFile = NIL; aOnPasswordRequired: TRAROnPasswordRequired = NIL): boolean;
begin
  result := openArchive(aFilePath, omRAR_OM_LIST, aRAR, TRUE);
  case result of FALSE: EXIT; end;

  initCallBack(aRAR, NIL, aOnPasswordRequired);
  try
    case aRAR.password = '' of FALSE: RARSetPassword(aRAR.handle, PAnsiChar(aRAR.password)); end;
    result := listArchiveFiles(aRAR, TRUE, aOnListFile);
  finally
    closeArchive(aRAR.handle);
  end;
end;

function testRARArchive(const aFilePath: string; aRAR: TRARArchive; aOnRARProgress: TRAROnProgress = NIL; aOnPasswordRequired: TRAROnPasswordRequired = NIL): boolean;
begin
  begin
    result := openArchive(aFilePath, getOpenMode(aRAR.readMVToEnd), aRAR, TRUE);
    case result of FALSE: EXIT; end;

    initCallBack(aRAR, NIL, aOnPasswordRequired);
    try
      case aRAR.password = '' of FALSE: RARSetPassword(aRAR.handle, PAnsiChar(aRAR.password)); end;
      result := listArchiveFiles(aRAR, FALSE); // get archive total unCompressedSize
    finally
      closeArchive(aRAR.handle);
    end;
  end;

  begin
    result := openArchive(aFilePath, omRAR_OM_EXTRACT, aRAR, FALSE);
    case result of FALSE: EXIT; end;

    initCallBack(aRAR, aOnRARProgress, aOnPasswordRequired);
    try
      case aRAR.password = '' of FALSE: RARSetPassword(aRAR.handle, PAnsiChar(aRAR.password)); end;
      result := testArchiveFiles(aRAR);
    finally
      closeArchive(aRAR.handle);
    end;
  end;
end;

{ TRAR }

procedure TRAR.onRARProgressTest(Sender: TObject; const aProgressInfo: TRARProgressInfo);
begin
  debugString('FileName', aProgressInfo.FileName);
  debugFormat('Archive Bytes: %d, Archive Bytes Done: %d', [aProgressInfo.ArchiveBytesTotal, aProgressInfo.ArchiveBytesDone]);
  debugFormat('FileBytesTotal: %d, FileBytesDone: %d', [aProgressInfo.FileBytesTotal, aProgressInfo.FileBytesDone]);
end;

constructor TRAR.create(AOwner: TComponent);
begin
  inherited create(AOwner);

  FRAR          := TRARArchive.create;

  FOnProgress   := onRARProgressTest;
end;

destructor TRAR.Destroy;
begin
  case assigned(FRAR) of TRUE: FRAR.free; end;
  inherited destroy;
end;

function TRAR.listArchive(const aFilePath: string): boolean;
begin
  result := listFiles(aFilePath, FRAR, FOnListFile, FOnPasswordRequired);
end;

procedure TRAR.setOnError(const Value: TRAROnErrorNotifyEvent);
begin
  RR.onError := value;
end;

procedure TRAR.setPassword(const Value: AnsiString);
begin
  FPassword     := value;
  FRAR.password := value;
end;

procedure TRAR.setReadMVToEnd(const Value: boolean);
begin
  FReadMVToEnd      := value;
  FRAR.readMVToEnd  := value;
end;

function TRAR.testArchive(const aFilePath: string): boolean;
begin
  result := testRARArchive(aFilePath, FRAR, FOnProgress, FOnPasswordRequired);
end;

function TRAR.getArchiveInfo: TRARArchiveInfo;
begin
  result := FRAR.info;
end;

function TRAR.getDLLName: string;
begin
  result := RARDLLName;
end;

function TRAR.getDLLVersion: integer;
begin
  result := RARGetDLLVersion;
end;

function TRAR.getOnError: TRAROnErrorNotifyEvent;
begin
  result := RR.onError;
end;

function TRAR.getPassword: AnsiString;
begin
  result := FPassword;
end;

function TRAR.getReadMVToEnd: boolean;
begin
  result := FRAR.readMVToEnd;
end;

procedure TRAR.abort;
begin
  FRAR.abort := TRUE;
end;

function TRAR.getVersion: string;
begin
  result := GVersion;
end;

{ TRARArchive }

constructor TRARArchive.create;
begin
  inherited create;
  getMem(commentBuf, RAR_MAX_COMMENT_SIZE);
end;

destructor TRARArchive.destroy;
begin
  case assigned(commentBuf) of TRUE: freeMem(commentBuf); end;
  inherited destroy;
end;

{ TRARResult }

function TRARResult.checkRARResult(const aResultCode: integer; const aOperation: TRAROperation): integer;
//  (ResultCode=ERAR_END_ARCHIVE) = 10
begin
  result      := aResultCode;
  FLastResult := aResultCode;

  var vReport := (aResultCode = RAR_DLL_LOAD_ERROR)
              or (aResultCode = ERAR_NO_MEMORY)
              or (aResultCode = ERAR_BAD_DATA)
              or (aResultCode = ERAR_UNKNOWN_FORMAT)
              or (aResultCode = ERAR_EOPEN)
              or (aResultCode = ERAR_ECREATE)
              or (aResultCode = ERAR_ECLOSE)
              or (aResultCode = ERAR_EREAD)
              or (aResultCode = ERAR_EWRITE)
              or (aResultCode = ERAR_SMALL_BUF)
              or (aResultCode = ERAR_UNKNOWN)
              or (aResultCode = RAR_COMMENT_UNKNOWN);

  case vReport and assigned(FOnError) of TRUE: FOnError(SELF, aResultCode, aOperation); end;
end;

function TRARResult.getLastResult: integer;
begin
  result := FLastResult;
end;

function TRARResult.getOnError: TRAROnErrorNotifyEvent;
begin
  result := FOnError;
end;

procedure TRARResult.setOnError(const Value: TRAROnErrorNotifyEvent);
begin
  FOnError := value;
end;

{ TCallBackInfo }

function TCallBackInfo.getOnPasswordRequired: TRAROnPasswordRequired;
begin
  result := FOnPasswordRequired;
end;

function TCallBackInfo.getOnProgress: TRAROnProgress;
begin
  result := FOnProgress;
end;

function TCallBackInfo.getRAR: TRARArchive;
begin
  result := FRAR;
end;

procedure TCallBackInfo.setOnPasswordRequired(const Value: TRAROnPasswordRequired);
begin
  FOnPasswordRequired := value;
end;

procedure TCallBackInfo.setOnProgress(const Value: TRAROnProgress);
begin
  FOnProgress := value;
end;

procedure TCallBackInfo.setRAR(const Value: TRARArchive);
begin
  FRAR := value;
end;

initialization
  RR := TRARResult.create;

end.
