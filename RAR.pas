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
//   - fixed a memory leak (thanks to Claes Ensk�r)
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

type
  TRARProgressInfo = record
    fileName:           WideString;
    archiveBytesTotal:  LongInt;
    archiveBytesDone:   LongInt;
    fileBytesTotal:     LongInt;
    fileBytesDone:      LongInt;
  end;

type
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

type
  TRARReplaceData = record
    fileName: Ansistring;
    size:     int64;
    time:     TDateTime;
end;

TRARReplace = (rrCancel, rrOverwrite, rrSkip);

type
  TOnRARErrorNotifyEvent    = procedure(Sender: TObject; const aErrorCode: integer; const aOperation: TRAROperation) of object;
  TOnRARListFile            = procedure(Sender: TObject; const aFileInformation: TRARFileItem) of object;
  TOnRARPasswordRequired    = procedure(Sender: TObject; const aHeaderPassword: boolean; const aFileName: Ansistring; out oNewPassword: Ansistring; out oCancel: boolean) of object;
  TOnRARNextVolumeRequired  = procedure(Sender: TObject; const aRequiredFileName: Ansistring; out oNewFileName: Ansistring; out oCancel: boolean) of object;
  TOnRARProgress            = procedure(Sender: TObject; const aProgressInfo: TRARProgressInfo) of object;
  TOnRARReplace             = procedure(Sender: TObject; const aExistingData:TRARReplaceData; aNewData:TRARReplaceData; out oAction: TRARReplace) of object;

type
  TRARArchiveInfo = record
    fileName:              Ansistring;

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
    info:                 TRARArchiveInfo;
    fileItem:             TRARFileItem;
    progressInfo:         TRARProgressInfo;
    hasComment:           boolean;
    comment:              AnsiString;
    commentBuf:           PAnsiChar;
    commentResult:        cardinal;
    abort:                boolean;
  public
    constructor create;
    destructor  destroy; override;
  end;

  ICallBackInfo = interface
  ['{9D062B91-12D3-47E2-821A-215938DAD3CE}']
    function  getOnPasswordRequired: TOnRARPasswordRequired;
    function  getOnRARProgress: TOnRARProgress;
    function  getRAR: TRARArchive;
    procedure setOnPasswordRequired(const Value: TOnRARPasswordRequired);
    procedure setOnRARProgress(const Value: TOnRARProgress);
    procedure setRAR(const Value: TRARArchive);
    property  RAR:                  TRARArchive             read getRAR                 write setRAR;
    property  onPasswordRequired:   TOnRARPasswordRequired  read getOnPasswordRequired  write setOnPasswordRequired;
    property  onRARProgress:        TOnRARProgress          read getOnRARProgress       write setOnRARProgress;
  end;

  TCallBackInfo = class(TInterfacedObject, ICallBackInfo)
    FRAR:                 TRARArchive;
    FOnPasswordRequired:  TOnRARPasswordRequired;
    FOnRARProgress:       TOnRARProgress;
  private
    function  getOnPasswordRequired: TOnRARPasswordRequired;
    function  getOnRARProgress: TOnRARProgress;
    function  getRAR: TRARArchive;
    procedure setOnPasswordRequired(const Value: TOnRARPasswordRequired);
    procedure setOnRARProgress(const Value: TOnRARProgress);
    procedure setRAR(const Value: TRARArchive);
  public
    property  RAR:                  TRARArchive             read getRAR                 write setRAR;
    property  onPasswordRequired:   TOnRARPasswordRequired  read getOnPasswordRequired  write setOnPasswordRequired;
    property  onRARProgress:        TOnRARProgress          read getOnRARProgress       write setOnRARProgress;
  end;

  IRARResult = interface
  ['{8CBBE61B-C5F8-4FFD-96D0-32C29CC05AAB}']
    function  checkRARResult(const aResultCode: integer; const aOperation: TRAROperation): integer;
    function  getLastResult: integer;
    function  getOnError:    TOnRARErrorNotifyEvent;
    procedure setOnError(const Value: TOnRARErrorNotifyEvent);
    property  lastResult:    integer read getLastResult;
    property  onError:       TOnRARErrorNotifyEvent  read getOnError         write setOnError;
  end;

  TRARResult = class(TInterfacedObject, IRARResult)
  strict private
    FOnError:     TOnRARErrorNotifyEvent;
    FLastResult:  integer;
  public
    function  checkRARResult(const aResultCode: integer; const aOperation: TRAROperation): integer;
    function  getLastResult:  integer;
    function  getOnError:     TOnRARErrorNotifyEvent;
    procedure setOnError(const Value: TOnRARErrorNotifyEvent);

    property  lastResult:     integer                 read getLastResult;
    property  onError:        TOnRARErrorNotifyEvent  read getOnError         write setOnError;
  end;

type
  TRAR = class(TComponent)
  strict private
    FRAR:                   TRARArchive;

    FReadMVToEnd:           boolean;
    FPassword:              AnsiString;

    FOnListFile:            TOnRARListFile;
    FOnPasswordRequired:    TOnRARPasswordRequired;
    FOnNextVolumeRequired:  TOnRARNextVolumeRequired;
    FOnProgress:            TOnRARProgress;
    FOnReplace:             TOnRARReplace;

    function  getOnError:   TOnRARErrorNotifyEvent;
    procedure setOnError(const Value: TOnRARErrorNotifyEvent);

    function  getDLLName: string;
    function  getVersion:string;

    procedure onRARProgressTest(Sender: TObject; const aProgressInfo: TRARProgressInfo);

    function  openArchive(const aFilePath: string; bExtract: boolean; const aRAR: TRARArchive): boolean;
    function  closeArchive(const aArchiveHandle: THANDLE): boolean;
    procedure initCallBack(const aRAR: TRARArchive);
    function  listArchiveFiles(const aRAR: TRARArchive): boolean;
    function  listFiles(const aFilePath: string): boolean;
    function  processFileHeader(const aFileHeaderDataEx: TRARHeaderDataEx; const aRAR: TRARArchive): TRARHeaderType;
    function  testArchiveFiles(const aRAR: TRARArchive): boolean;
    function  getArchiveInfo: TRARArchiveInfo;
    function  getDLLVersion:  integer;
  public
    constructor create(AOwner: TComponent);
    destructor  destroy; override;

    procedure abort;

  public
    function  listArchive(const aFilePath:string):  boolean;
    function  testArchive(const aFilePath:string):  boolean;

  published
    property version: string read getVersion;
    property readMultiVolumeToEnd:  boolean                   read FReadMVToEnd           write FReadMVToEnd; //if true, mv's will be read until last part of the file
    //pro: display correct crc + display all files in all parts
    //con: all volumes required means to open you have to insert all disk if not all volumes in same folder
    property DLLName:               string                    read getDLLName;
    property DLLVersion:            integer                   read getDLLVersion;

    property onError:               TOnRARErrorNotifyEvent    read getOnError             write setOnError;
    property onListFile:            TOnRARListFile            read FOnListFile            write FOnListFile;
    property onPasswordRequired:    TOnRARPasswordRequired    read FOnPasswordRequired    write FOnPasswordRequired;
    property onNextVolumeRequired:  TOnRARNextVolumeRequired  read FOnNextVolumeRequired  write FOnNextVolumeRequired;
    property onProgress:            TOnRARProgress            read FOnProgress            write FOnProgress;
    property onReplace:             TOnRARReplace             read FOnReplace             write FOnReplace;

    property archiveInfo:           TRARArchiveInfo           read getArchiveInfo;
    property password:              AnsiString                read FPassword              write FPassword; // can be supplied by the user before calling an operation
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
  vCBI:           ICallBackInfo;
  vCancel:        boolean;
  vPasswordFile:  AnsiString;
  vPassword:      AnsiString;
begin
  result  := RAR_CONTINUE;
  vCBI    := ICallbackInfo(userData);

  try
  case msg of
    UCM_NEEDPASSWORD: begin
                        case assigned(vCBI.onPasswordRequired) of TRUE: vCBI.onPasswordRequired(vCBI.RAR, NOT vCBI.RAR.opened, vPasswordFile, vPassword, vCancel); end;
                        case vCancel of TRUE: result := RAR_CANCEL; end;
                        strPCopy(Pointer(P1), copy(vPassword, 1, P2)); // P2 = the maximum size of the password buffer in unrar
                      end;end;
  except
    MessageBox(0, 'It go Bang!', 'Bang!', MB_ICONEXCLAMATION or MB_OK);
  end;
end;

procedure TRAR.onRARProgressTest(Sender: TObject; const aProgressInfo: TRARProgressInfo);
begin
  debugString('FileName', aProgressInfo.FileName);
  debugFormat('Archive Bytes: %d, Archive Bytes Done: %d', [aProgressInfo.ArchiveBytesTotal, aProgressInfo.ArchiveBytesDone]);
  debugFormat('FileBytesTotal: %d, FileBytesDone: %d', [aProgressInfo.FileBytesTotal, aProgressInfo.FileBytesDone]);
end;

constructor TRAR.create(AOwner: TComponent);
begin
  inherited create(AOwner);

  FReadMVToEnd  := FALSE;
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
  result := listFiles(aFilePath);
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

  case aOpenArchiveDataEx.cmtState of // read archive comment - cmtState is actually aRAR.commentResult
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

function TRAR.openArchive(const aFilePath: string; bExtract: boolean; const aRAR: TRARArchive): boolean;

  procedure initArchive;
  begin
    with aRAR do begin
      handle        := 0;
      opened        := FALSE;
      info          := default(TRARArchiveInfo);
      fileItem      := default(TRARFileItem);
      progressInfo  := default(TRARProgressInfo);
      hasComment    := FALSE;
      comment       := '';
      commentResult := 0;
      abort         := FALSE;
    end;
  end;

begin
  initArchive;

  aRAR.info.fileName  := aFilePath;

  result := FALSE;

  var vOpenArchiveDataEx := default(TRAROpenArchiveDataEx);
  with vOpenArchiveDataEx do begin
    OpenResult := RAR_SUCCESS;
    if bExtract
    then
      OpenMode := RAR_OM_EXTRACT
    else
      if FReadMVToEnd then
        OpenMode := RAR_OM_LIST_INCSPLIT
      else
        OpenMode := RAR_OM_LIST;

    arcName := PAnsiChar(aRAR.info.fileName);

    cmtBuf      := aRAR.commentBuf;
    cmtBufSize  := RAR_MAX_COMMENT_SIZE;
    cmtSize     := length(aRAR.commentBuf);
    cmtState    := aRAR.commentResult; // feedback variable
  end;

  aRAR.handle   := RAROpenArchiveEx(@vOpenArchiveDataEx);
  aRAR.opened   := aRAR.handle <> RAR_INVALID_HANDLE;
  case aRAR.handle = RAR_INVALID_HANDLE of TRUE:  begin
                                                    checkRARResult(ERAR_EOPEN, roOpenArchive);
                                                    EXIT; end;end;

  result := processOpenArchive(vOpenArchiveDataEx, aRAR);
end;

function TRAR.closeArchive(const aArchiveHandle: THANDLE): boolean;
begin
  case aArchiveHandle = RAR_INVALID_HANDLE of FALSE: result := checkRARResult(RARCloseArchive(aArchiveHandle), roCloseArchive) = RAR_SUCCESS; end;
end;

function TRAR.processFileHeader(const aFileHeaderDataEx: TRARHeaderDataEx; const aRAR: TRARArchive): TRARHeaderType; // populate FArchiveInfo: TRARArchiveInfo and vFileItem: TRARFileItem from aHeaderDataEx
var
  ft:         _FILETIME;
  st:         TSystemTime;
  OS:         string;
  i:          integer;

  function binToStr(const bin: array of byte): string;
  begin
    setLength(result, 2 * length(bin));
    for var i := low(bin) to high(bin) do begin
      result[i * 2 + 1] := lowerCase(intToHex(bin[i], 2))[1];
      result[i * 2 + 2] := lowerCase(intToHex(bin[i], 2))[2];
    end;
//    {$IFDEF Win64} setLength(result, length(result) - 8); {$ENDIF}
  end;

begin
//  debugFormat('processHeader, Archive: %s, File: %s', [aHeaderDataEx.ArcNameW, aHeaderDataEx.FileNameW]);
  // first part of the file
  if (FReadMVToEnd) and (NOT ((aFileHeaderDataEx.Flags AND $00000001) = $00000001)) and (((aFileHeaderDataEx.Flags AND $00000002) = $00000002)) then aRAR.info.packedSizeMVVolume := aFileHeaderDataEx.PackSize;

  // NOT last, NOT first part
  if (FReadMVToEnd) and (((aFileHeaderDataEx.Flags AND $00000001) = $00000001))
                    and (((aFileHeaderDataEx.Flags AND $00000002) = $00000002)) then  begin
                                                                                    inc(aRAR.info.packedSizeMVVolume, aFileHeaderDataEx.PackSize);
                                                                                    EXIT;
                                                                                  end;
  // last part
  if (FReadMVToEnd) and     (((aFileHeaderDataEx.Flags AND $00000001) = $00000001))
                    and (NOT ((aFileHeaderDataEx.Flags AND $00000002) = $00000002))
                    then inc(aRAR.info.packedSizeMVVolume, aFileHeaderDataEx.PackSize);

  // NOT last part
  if (FReadMVToEnd) and ((aFileHeaderDataEx.Flags AND $00000002) = $00000002) then EXIT;

  if aRAR.info.archiverMajorVersion * 10 + aRAR.info.archiverMinorVersion < aFileHeaderDataEx.UnpVer then begin
     aRAR.info.archiverMinorVersion := aFileHeaderDataEx.UnpVer mod 10;
     aRAR.info.archiverMajorVersion :=(aFileHeaderDataEx.UnpVer - aRAR.info.archiverMinorVersion) div 10;
  end;

  if ((aFileHeaderDataEx.Flags AND $00000010) = $00000010) then aRAR.info.solid := TRUE;

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

  if ((aFileHeaderDataEx.Flags AND $00000001) = $00000001) or ((aFileHeaderDataEx.Flags AND $00000002) = $00000002) then aRAR.info.multiVolume := TRUE; // file continued in last or next part

//  if aFileHeaderDataEx.cmtSize > 0 then aRAR.info.fileComment := TRUE;

  aRAR.fileItem := default(TRARFileItem);
  with aRAR.fileItem do begin
    fileName          := strPas(aFileHeaderDataEx.fileName);
    fileNameW         := aFileHeaderDataEx.fileNameW;
    compressedSize    := aFileHeaderDataEx.packSize;
    unCompressedSize  := aFileHeaderDataEx.unpSize;
    hostOS            := OS;
    CRC32             := format('%x',[aFileHeaderDataEx.fileCRC]);
    attributes        := aFileHeaderDataEx.fileAttr;
//    comment           := aFileHeaderDataEx.cmtBuf;

    dosDateTimeToFileTime(hiWord(aFileHeaderDataEx.fileTime), loWord(aFileHeaderDataEx.fileTime), ft);
    fileTimeToSystemTime(ft, st);
    time := systemTimeToDateTime(st);

    compressionStrength := aFileHeaderDataEx.method;
    archiverVersion     := aFileHeaderDataEx.unpVer;
    encrypted           := (aFileHeaderDataEx.flags AND $00000004) = $00000004;
    blake2              := binToStr(aFileHeaderDataEx.blake2);
  end;
end;

procedure TRAR.setOnError(const Value: TOnRARErrorNotifyEvent);
begin
  RR.onError := value;
end;

function TRAR.listArchiveFiles(const aRAR: TRARArchive): boolean;
begin
  var vHeaderDataEx: TRARHeaderDataEx := default(TRARHeaderDataEx);

  try
    repeat

      case checkRARResult(RARReadHeaderEx(aRAR.handle, @vHeaderDataEx), roListFiles)     = RAR_SUCCESS  of FALSE: EXIT; end;  // get the next file header in the archive
      case (processFileHeader(vHeaderDataEx, aRAR) = htFile) and assigned(FOnListFile)                  of TRUE:  FOnListFile(SELF, aRAR.fileItem); end;
      case checkRARResult(RARProcessFile(aRAR.handle, RAR_SKIP, NIL, NIL), roListFiles)  = RAR_SUCCESS  of FALSE: EXIT; end;  // do nothing - skip to next file header

      application.processMessages;  // allow the user to actually press a cancel button
    until aRAR.abort;                   // RARReadHeaderEx = ERAR_END_ARCHIVE will usually exit the loop

  finally
    result := RR.lastResult = ERAR_END_ARCHIVE; // not an error in this case
  end;
end;

function TRAR.listFiles(const aFilePath: string): boolean;
begin
  result := openArchive(aFilePath, FALSE, FRAR);
  case result of FALSE: EXIT; end;

  case FPassword = '' of FALSE: RARSetPassword(FRAR.handle, PAnsiChar(FPassword)); end;
  initCallBack(FRAR);
  result := listArchiveFiles(FRAR);

  closeArchive(FRAR.handle);
end;

function TRAR.testArchiveFiles(const aRAR: TRARArchive): boolean;
begin
  var vHeaderDataEx: TRARHeaderDataEx;

  aRAR.progressInfo := default(TRARProgressInfo);

  try
    repeat

      case checkRARResult(RARReadHeaderEx(aRAR.handle, @vHeaderDataEx), roTest)     = RAR_SUCCESS of FALSE: EXIT; end;  // get the next file header in the archive
      // processFileHeader(vHeaderDataEx, aRAR);

      aRAR.progressInfo.FileBytesDone   := 0;
      aRAR.progressInfo.FileBytesTotal  := vHeaderDataEx.UnpSize;
      aRAR.progressInfo.FileName        := vHeaderDataEx.FileNameW;

      case checkRARResult(RARProcessFile(aRAR.handle, RAR_TEST, NIL, NIL), roTest)  = RAR_SUCCESS of FALSE: EXIT; end;

      application.processMessages; // allow the user to actually press a cancel button
    until aRAR.abort; // RARReadHeaderEx = ERAR_END_ARCHIVE will usually exit the loop

  finally
    result := RR.lastResult = ERAR_END_ARCHIVE; // not an error in this case
  end;
end;

function TRAR.testArchive(const aFilePath: string): boolean;
begin
  result := openArchive(aFilePath, TRUE, FRAR);
  case result of FALSE: EXIT; end;

  case FPassword = '' of FALSE: RARSetPassword(FRAR.handle, PAnsiChar(FPassword)); end;
  initCallBack(FRAR);
  result := testArchiveFiles(FRAR);

  closeArchive(FRAR.handle);
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

function TRAR.getOnError: TOnRARErrorNotifyEvent;
begin
  result := RR.onError;
end;

procedure TRAR.abort;
begin
  FRAR.abort := TRUE;
end;

function TRAR.getVersion: string;
begin
  result := GVersion;
end;

procedure TRAR.initCallBack(const aRAR: TRARArchive);
begin
  var vCBI:ICallBackInfo  := TCallBackInfo.create;
  vCBI.RAR                := aRAR;
  vCBI.onRARProgress      := FOnProgress;
  vCBI.onPasswordRequired := FOnPasswordRequired;
  RARSetCallback(aRAR.handle, UnRarCallBack, LPARAM(pointer(vCBI)));
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
begin
  result      := aResultCode;
  FLastResult := aResultCode;

//  FAbort := (aResultCode = RAR_DLL_LOAD_ERROR)
////  or (ResultCode=ERAR_END_ARCHIVE) = 10
//  or (aResultCode = ERAR_NO_MEMORY)
//  or (aResultCode = ERAR_BAD_DATA)
//  or (aResultCode = ERAR_UNKNOWN_FORMAT)
//  or (aResultCode = ERAR_EOPEN)
//  or (aResultCode = ERAR_ECREATE)
//  or (aResultCode = ERAR_ECLOSE)
//  or (aResultCode = ERAR_EREAD)
//  or (aResultCode = ERAR_EWRITE)
//  or (aResultCode = ERAR_SMALL_BUF)
//  or (aResultCode = ERAR_UNKNOWN);

  case {(FAbort or (aResultCode = RAR_COMMENT_UNKNOWN)) and} assigned(FOnError) of TRUE: FOnError(SELF, aResultCode, aOperation); end;
end;

function TRARResult.getLastResult: integer;
begin
  result := FLastResult;
end;

function TRARResult.getOnError: TOnRARErrorNotifyEvent;
begin
  result := FOnError;
end;

procedure TRARResult.setOnError(const Value: TOnRARErrorNotifyEvent);
begin
  FOnError := value;
end;

{ TCallBackInfo }

function TCallBackInfo.getOnPasswordRequired: TOnRARPasswordRequired;
begin
  result := FOnPasswordRequired;
end;

function TCallBackInfo.getOnRARProgress: TOnRARProgress;
begin
  result := FOnRARProgress;
end;

function TCallBackInfo.getRAR: TRARArchive;
begin
  result := FRAR;
end;

procedure TCallBackInfo.setOnPasswordRequired(const Value: TOnRARPasswordRequired);
begin
  FOnPasswordRequired := value;
end;

procedure TCallBackInfo.setOnRARProgress(const Value: TOnRARProgress);
begin
  FOnRARProgress := value;
end;

procedure TCallBackInfo.setRAR(const Value: TRARArchive);
begin
  FRAR := value;
end;

initialization
  RR := TRARResult.create;

end.
