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
  TRAROperation = (roOpenArchive, roCloseArchive, roListFiles, roExtract, roTest);

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
    comment:              Ansistring;
    time:                 TDateTime;
    compressionStrength:  cardinal;
    archiverVersion:      cardinal;
    encrypted:            boolean;
    blake2:               string; // Baz
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
  TRARArchiveInformation = record
    opened:                boolean;
    fileName:              Ansistring;

    // derived from RAROpenArchiveEx: TRAROpenArchiveDataEx in openArchive()
    locked:                boolean;
    signed:                boolean;
    recovery:              boolean;
    solid:                 boolean; // also set in processFileHeader(aHeaderDataEx: TRARHeaderDataEx)
    headerEncrypted:       boolean;
    SFX:                   boolean;
    comment:               AnsiString;
    archiveComment:        boolean;

    // derived from processFileHeader(aHeaderDataEx: TRARHeaderDataEx);
    archiverMajorVersion:  cardinal;
    archiverMinorVersion:  cardinal;
    hostOS:                string;
    totalFiles:            integer; // incremented when processing each file header
    dictionarySize:        int64;
    compressedSize:        int64;   // totalled from aHeaderDataEx.PackSize when processing each file header
    unCompressedSize:      int64;   // totalled from aHeaderDataEx.UnpSize  when processing eacah file header
    multiVolume:           boolean;
    fileComment:           boolean;
  end;

  TCallbackInfo = class
    progressInfo:  TRARProgressInfo;
    onRARProgress: TOnRARProgress;
  end;

type
  TRAR = class(TComponent)
  private
    FAbort:                 boolean;
    FProgressInfo:          TRARProgressInfo;
    FReadMVToEnd:           boolean;
    FPackedSizeMVVolume:    cardinal; // from processFileHeader(aHeaderDataEx: TRARHeaderDataEx)
    FPassword:              AnsiString;
    FComment:               PAnsiChar;
    FCommentResult:         cardinal;
    FArchiveInformation:    TRARArchiveInformation;
    FOpenArchiveDataEx:     TRAROpenArchiveDataEx;
    FArchiveHandle:         THandle;
    FFileHeaderDataEx:      TRARHeaderDataEx;
    FLastResult:            integer;
    FOnError:               TOnRARErrorNotifyEvent;
    FOnListFile:            TOnRARListFile;
    FOnPasswordRequired:    TOnRARPasswordRequired;
    FOnNextVolumeRequired:  TOnRARNextVolumeRequired;
    FOnProgress:            TOnRARProgress;
    FOnReplace:             TOnRARReplace;
    function  openArchive(const aFilePath: string; bExtract: boolean): THANDLE;
    function  closeArchive(aArchiveHandle: THANDLE): boolean;
    function  onUnRarCallBack(msg: cardinal; UserData: LPARAM; P1: LPARAM; P2: LPARAM): integer;
    procedure processFileHeader(aFileHeaderDataEx: TRARHeaderDataEx);
    function  checkRARResult(const aResultCode:integer; const aOperation: TRAROperation): integer;
    function  getVersion:string;
    procedure onRARProgressTest(Sender: TObject; const aProgressInfo: TRARProgressInfo);
    function  listArchiveFiles(aArchiveHandle: THANDLE; var bAbort: boolean): boolean;
    function  testArchiveFiles(aArchiveHandle: THANDLE; var aProgressInfo: TRARProgressInfo; var bAbort: boolean): boolean;
    function  getDLLName: string;
  protected
  public
    constructor create(AOwner: TComponent); override;
    destructor  destroy; override;

    function  listFiles(const aFilePath: string): boolean;
    function  extractFiles(const aFilePath: AnsiString; bRestoreFolder: boolean; aFiles: TStrings):boolean;
    procedure abort;
    function  getDLLVersion:  integer;

  public
    function  listArchive(const aFilePath:string):  boolean;
    function  testArchive(const aFilePath:string):  boolean;

  published
    property version: string read getVersion;
    property readMultiVolumeToEnd:  boolean                   read FReadMVToEnd           write FReadMVToEnd; //if true, mv's will be read until last part of the file
    //pro: display correct crc + display all files in all parts
    //con: all volumes required means to open you have to insert all disk if not all volumes in same folder
    property DLLName:               string                    read getDLLName;
    property onError:               TOnRARErrorNotifyEvent    read FOnError               write FOnError;
    property onListFile:            TOnRARListFile            read FOnListFile            write FOnListFile;
    property onPasswordRequired:    TOnRARPasswordRequired    read FOnPasswordRequired    write FOnPasswordRequired;
    property onNextVolumeRequired:  TOnRARNextVolumeRequired  read FOnNextVolumeRequired  write FOnNextVolumeRequired;
    property onProgress:            TOnRARProgress            read FOnProgress            write FOnProgress;
    property onReplace:             TOnRARReplace             read FOnReplace             write FOnReplace;
    property archiveInformation:    TRARArchiveInformation    read FArchiveInformation;
  end;

procedure Register;

implementation

uses
  _debugWindow;

const
  GVersion='2.0';

procedure Register;
begin
  RegisterComponents('Philippe Wechsler', [TRAR]);
end;

function UnRarCallBack(msg: cardinal; UserData: LPARAM; P1: LPARAM; P2: LPARAM): integer; {$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};
var vCBI: TCallBackInfo;
begin
//  result := RAR_CONTINUE;
//  vCBI := TCallbackInfo(UserData);
//  case assigned(vCBI.onRARProgress) of TRUE: vCBI.onRARProgress(NIL, vCBI.progressInfo); end;
  result:=TRAR(UserData).onUnRarCallBack(msg, UserData, P1, P2);
//  debugString('Archive', TRAR(UserData).FArchiveInformation.FileName);
end;

procedure TRAR.onRARProgressTest(Sender: TObject; const aProgressInfo: TRARProgressInfo);
begin
  debugString('FileName', aProgressInfo.FileName);
  debugFormat('Archive Bytes: %d, Archive Bytes Done: %d', [aProgressInfo.ArchiveBytesTotal, aProgressInfo.ArchiveBytesDone]);
  debugFormat('FileBytesTotal: %d, FileBytesDone: %d', [aProgressInfo.FileBytesTotal, aProgressInfo.FileBytesDone]);
end;

function TRAR.onUnRarCallBack(msg: cardinal; UserData: LPARAM; P1: LPARAM; P2: LPARAM): integer;
var
  vPassword:      AnsiString;
  vFileName:      AnsiString;
  vPasswordFile:  AnsiString;
  vCancel:        boolean;
begin
  vPassword := '';
  vCancel   := FALSE;
  result    := RAR_CONTINUE;

  case msg of
    UCM_CHANGEVOLUME: begin
                        vFileName := PAnsiChar(P1);
                        case P2 of
                          RAR_VOL_ASK:    begin
                                            if (NOT FArchiveInformation.Opened) and (NOT FReadMVToEnd) then result := RAR_CANCEL
                                            else begin
                                              if assigned(FOnNextVolumeRequired) then FOnNextVolumeRequired(SELF, PAnsiChar(P1), vFileName, vCancel);
                                              strPCopy(PAnsiChar(P1), vFileName); //todo: handle error if P1 has NOT enough space for FileName
                                              if FAbort or vCancel then result := RAR_CANCEL; end; end;
                          RAR_VOL_NOTIFY: result := RAR_CONTINUE; // occurs when next volume required and next part was found
                        end;
                      end;

    UCM_NEEDPASSWORD: begin
                        if NOT FArchiveInformation.opened then begin
                          // FArchiveInformation.headerEncrypted := TRUE;   // holy shit!
                          vPasswordFile := FArchiveInformation.fileName;
                        end
                        else vPasswordFile := FProgressInfo.fileName;

                        if assigned(FOnPasswordRequired) then FOnPasswordRequired(SELF, NOT FArchiveInformation.opened, vPasswordFile, vPassword, vCancel);
                        strPCopy(Pointer(P1), copy(vPassword, 1, P2)); // P2 = the maximum size of the password buffer in unrar
                        if FAbort or vCancel then result := RAR_CANCEL;
                      end;

    UCM_PROCESSDATA:  begin
                        FProgressInfo.archiveBytesTotal := FArchiveInformation.unCompressedSize;
                        FProgressInfo.archiveBytesDone  := FProgressInfo.archiveBytesDone + P2;
                        FProgressInfo.fileBytesTotal    := FFileHeaderDataEx.unpSize;
                        FProgressInfo.fileBytesDone     := FProgressInfo.fileBytesDone  + P2;

                        if assigned(FOnProgress) then FOnProgress(SELF, FProgressInfo);
                        if FAbort then result := RAR_CANCEL;
                      end;
  end;
  if FAbort then result := RAR_CANCEL;
end;

constructor TRAR.create(AOwner: TComponent);
begin
  inherited create(AOwner);

  FReadMVToEnd        := FALSE;
  FOnProgress         := onRARProgressTest;
end;

destructor TRAR.Destroy;
begin
  if assigned(FComment) then freeMem(FComment);
  inherited destroy;
end;

function TRAR.listArchive(const aFilePath: string): boolean;
begin
  result := listFiles(aFilePath);
end;

function TRAR.openArchive(const aFilePath: string; bExtract: boolean): THANDLE;
begin
//  debug('initArchive');
  FArchiveInformation := default(TRARArchiveInformation);

  FArchiveInformation.FileName  := aFilePath;
  FArchiveInformation.Opened    := TRUE;

  result          := 0;
  FCommentResult  := RAR_SUCCESS;

  FOpenArchiveDataEx := default(TRAROpenArchiveDataEx);
  with FOpenArchiveDataEx do begin
    OpenResult := RAR_SUCCESS;
    if bExtract
    then
      OpenMode := RAR_OM_EXTRACT
    else
      if FReadMVToEnd then
        OpenMode := RAR_OM_LIST_INCSPLIT
      else
        OpenMode := RAR_OM_LIST;

    ArcName := PAnsiChar(FArchiveInformation.FileName);

    if NOT Assigned(FComment) then getMem(FComment, RAR_MAX_COMMENT_SIZE);
    CmtBuf      := FComment;
    CmtBufSize  := RAR_MAX_COMMENT_SIZE;
    CmtSize     := length(FComment);
    CmtState    := FCommentResult;
  end;

  result := RAROpenArchiveEx(@FOpenArchiveDataEx);
  case result = RAR_INVALID_HANDLE of TRUE: begin
                                              checkRARResult(ERAR_EOPEN, roOpenArchive);
                                              EXIT; end;end;


  //((ArchiveData.Flags and $00000100)=$00000100)=first volume
  //((ArchiveData.Flags and $00000001)=$00000001)=Volume attribute (archive volume)
  //((ArchiveData.Flags and $00000010)=$00000010)=New volume naming scheme ('volname.partN.rar')

  //set archive info
  if ((FOpenArchiveDataEx.Flags AND $00000002) = $00000002) then FArchiveInformation.ArchiveComment   := TRUE; // unrar doesn't seem to set this
  if ((FOpenArchiveDataEx.Flags AND $00000004) = $00000004) then FArchiveInformation.Locked           := TRUE;
  if ((FOpenArchiveDataEx.Flags AND $00000008) = $00000008) then FArchiveInformation.Solid            := TRUE;
  if ((FOpenArchiveDataEx.Flags AND $00000020) = $00000020) then FArchiveInformation.Signed           := TRUE;
  if ((FOpenArchiveDataEx.Flags AND $00000040) = $00000040) then FArchiveInformation.Recovery         := TRUE; // unrar doesn't seem to set this
  if ((FOpenArchiveDataEx.Flags AND $00000080) = $00000080) then FArchiveInformation.HeaderEncrypted  := TRUE; // it does set this

  FArchiveInformation.SFX := isSFX(FArchiveInformation.FileName);

  case FOpenArchiveDataEx.CmtState of //read archive comment
    RAR_COMMENT_EXISTS:   begin
                            FArchiveInformation.Comment        := strPas(FComment);
                            FArchiveInformation.ArchiveComment := TRUE;
                          end;
    RAR_NO_COMMENT:       begin
                            FArchiveInformation.Comment        := '';
                            FArchiveInformation.ArchiveComment := FALSE;
                          end;
    ERAR_NO_MEMORY:       checkRARResult(ERAR_NO_MEMORY,       roOpenArchive);
    ERAR_BAD_DATA:        checkRARResult(ERAR_BAD_DATA,        roOpenArchive);
    ERAR_UNKNOWN_FORMAT:  checkRARResult(ERAR_UNKNOWN_FORMAT,  roOpenArchive);
    ERAR_SMALL_BUF:       checkRARResult(ERAR_SMALL_BUF,       roOpenArchive);
  end;

  case FOpenArchiveDataEx.cmtState in [RAR_NO_COMMENT, RAR_COMMENT_EXISTS] of FALSE: checkRARResult(RAR_COMMENT_UNKNOWN, roOpenArchive); end // unknown comment condition
end;

function TRAR.closeArchive(aArchiveHandle: THANDLE): boolean;
begin
  case aArchiveHandle = RAR_INVALID_HANDLE of FALSE: result := checkRARResult(RARCloseArchive(aArchiveHandle), roCloseArchive) = RAR_SUCCESS; end;
end;

procedure TRAR.processFileHeader(aFileHeaderDataEx: TRARHeaderDataEx); // populate FArchiveInformation: TRARArchiveInformation and vFileItem: TRARFileItem from aHeaderDataEx
var
  vFileItem:  TRARFileItem;
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
  if (FReadMVToEnd) and (NOT ((aFileHeaderDataEx.Flags AND $00000001) = $00000001)) and (((aFileHeaderDataEx.Flags AND $00000002) = $00000002)) then FPackedSizeMVVolume := aFileHeaderDataEx.PackSize;

  // NOT last, NOT first part
  if (FReadMVToEnd) and (((aFileHeaderDataEx.Flags AND $00000001) = $00000001))
                    and (((aFileHeaderDataEx.Flags AND $00000002) = $00000002)) then  begin
                                                                                    FPackedSizeMVVolume := FPackedSizeMVVolume + aFileHeaderDataEx.PackSize;
                                                                                    EXIT;
                                                                                  end;
  // last part
  if (FReadMVToEnd) and     (((aFileHeaderDataEx.Flags AND $00000001) = $00000001))
                    and (NOT ((aFileHeaderDataEx.Flags AND $00000002) = $00000002))
                    then aFileHeaderDataEx.PackSize := aFileHeaderDataEx.PackSize + FPackedSizeMVVolume;

  // NOT last part
  if (FReadMVToEnd) and ((aFileHeaderDataEx.Flags AND $00000002) = $00000002) then EXIT;

  if FArchiveInformation.ArchiverMajorVersion * 10 + FArchiveInformation.ArchiverMinorVersion < aFileHeaderDataEx.UnpVer then begin
     FArchiveInformation.ArchiverMinorVersion := aFileHeaderDataEx.UnpVer mod 10;
     FArchiveInformation.ArchiverMajorVersion :=(aFileHeaderDataEx.UnpVer - FArchiveInformation.ArchiverMinorVersion) div 10;
  end;

  if ((aFileHeaderDataEx.Flags AND $00000010) = $00000010) then FArchiveInformation.Solid      := TRUE;

  OS:='unknown';
  case aFileHeaderDataEx.HostOS of
    0: OS:='DOS';
    1: OS:='IBM OS/2';
    2: OS:='Windows';
    3: OS:='Unix';
  end;
  FArchiveInformation.HostOS := OS;

//  if (NOT ((aFileHeaderDataEx.Flags AND $00000070) = $00000070)) and (aFileHeaderDataEx.fileAttr <> faDirectory) then begin // NOT a directory
  case (aFileHeaderDataEx.fileAttr AND faDirectory) = faDirectory of TRUE: EXIT; end;

  inc(FArchiveInformation.totalFiles);
  FArchiveInformation.dictionarySize := aFileHeaderDataEx.dictSize;

  inc(FArchiveInformation.compressedSize,   aFileHeaderDataEx.packSize);
  inc(FArchiveInformation.unCompressedSize, aFileHeaderDataEx.unpSize);

  if ((aFileHeaderDataEx.Flags AND $00000001) = $00000001) or ((aFileHeaderDataEx.Flags AND $00000002) = $00000002) then FArchiveInformation.multiVolume := TRUE; // file continued in last or next part

  if aFileHeaderDataEx.cmtSize > 0 then FArchiveInformation.fileComment := TRUE;

  with vFileItem do begin
    fileName          := strPas(aFileHeaderDataEx.fileName);
    fileNameW         := aFileHeaderDataEx.fileNameW;
    case ((aFileHeaderDataEx.Flags AND $00000004) = $00000004) of TRUE: encrypted := TRUE; end;
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
    blake2              := binToStr(aFileHeaderDataEx.blake2); // Baz
  end;

  if assigned(FOnListFile) then FOnListFile(SELF, vFileItem);
end;

function TRAR.listArchiveFiles(aArchiveHandle: THANDLE; var bAbort: boolean): boolean;
begin
  var vHeaderDataEx: TRARHeaderDataEx := default(TRARHeaderDataEx);
  var vCallbackInfo := TCallbackInfo.create;
  vCallbackInfo.progressInfo := FProgressInfo;
  vCallbackInfo.onRARProgress := FOnProgress;
//  RARSetCallback(aArchivehandle, UnRarCallBack, LPARAM(pointer(vCallBackInfo)));
  RARSetCallback(aArchivehandle, UnRarCallBack, LPARAM(pointer(SELF)));

  try
    repeat

      case checkRARResult(RARReadHeaderEx(aArchiveHandle, @vHeaderDataEx), roListFiles)     = RAR_SUCCESS of FALSE: EXIT; end;  // get the next file header in the archive
      processFileHeader(vHeaderDataEx);
      case checkRARResult(RARProcessFile(aArchiveHandle, RAR_SKIP, NIL, NIL), roListFiles)  = RAR_SUCCESS of FALSE: EXIT; end;  // do nothing - skip to next file header

      application.processMessages;  // allow the user to actually press a cancel button
    until FAbort;                   // RARReadHeaderEx = ERAR_END_ARCHIVE will usually exit the loop

  finally
    result := FLastResult = ERAR_END_ARCHIVE; // not an error in this case
  end;
end;

function TRAR.listFiles(const aFilePath: string): boolean;
begin
  FAbort := FALSE;
  var vArchiveHandle := openArchive(aFilePath, FALSE);
  case vArchiveHandle  = RAR_INVALID_HANDLE of TRUE: EXIT; end;
//  RARSetCallback(vArchivehandle, UnRarCallBack, LPARAM(pointer(SELF)));
  case FPassword = '' of FALSE: RARSetPassword(vArchiveHandle, PAnsiChar(FPassword)); end;
  result := listArchiveFiles(vArchiveHandle, FAbort);
  closeArchive(vArchiveHandle);
end;

function extractFile(aFileName: string; aFiles: TStrings): boolean; // returns whether or not the actual file should be extracted
begin
  if aFiles = NIL
  then
    result := TRUE
  else  begin
          result := FALSE;
          for var i := 0 to aFiles.Count - 1 do // check if actual file is in the filelist
            if aFiles[i] = aFileName then begin
              result := TRUE;
              BREAK;
            end;
        end;
end;

function TRAR.extractFiles(const aFilePath: AnsiString; bRestoreFolder: boolean; aFiles:TStrings): boolean;
var
  vReadFileHeaderResult:  integer;
  vExistentFile:          TRARReplaceData;
  vArchiveFile:           TRARReplaceData;
  ft:                     _FILETIME;
  st:                     TSystemTime;
  vReplaceResult:         TRARReplace;
begin
  FAbort := FALSE;
  result := openArchive(aFilePath, TRUE) <> RAR_INVALID_HANDLE;
  if FAbort or NOT (result) then EXIT;

//  if aFilePath[Length(aFilePath)] <> '\' then aFilePath := aFilePath+'\';
  FProgressInfo := default(TRARProgressInfo); // reset all counts to zero

  try
    RARSetCallback(FArchivehandle, UnRarCallBack, LPARAM(pointer(SELF)));

    if FPassword <> '' then RARSetPassword(FArchiveHandle, PAnsiChar(FPassword));

    vReadFileHeaderResult := RAR_SUCCESS;

    while (vReadFileHeaderResult = RAR_SUCCESS) and result do begin

      vReadFileHeaderResult := RARReadHeaderEx(FArchiveHandle, @FFileHeaderDataEx);
      if vReadFileHeaderResult = ERAR_END_ARCHIVE then BREAK;

      if vReadFileHeaderResult <> RAR_SUCCESS then  begin
                                                      result := FALSE;
                                                      checkRARResult(vReadFileHeaderResult, roListFiles);
                                                    end;

      FProgressInfo                     := default(TRARProgressInfo);
      FProgressInfo.FileName            := FFileHeaderDataEx.FileNameW;
      FProgressInfo.ArchiveBytesTotal   := FFileHeaderDataEx.UnpSize;
      vReplaceResult                    := rrOverWrite;

      if extractFile(strPas(FFileHeaderDataEx.FileName), aFiles) then begin    //todo: UniCode FileName

        if bRestoreFolder then
          vExistentFile.FileName  := aFilePath + strPas(FFileHeaderDataEx.FileName)
        else
          vExistentFile.FileName  := aFilePath + extractFileName(strPas(FFileHeaderDataEx.FileName));

        vExistentFile.Size := getFileSize(vExistentFile.FileName);
        vExistentFile.Time := getFileModifyDate(vExistentFile.FileName);

        if bRestoreFolder then
          vArchiveFile.FileName := strPas(FFileHeaderDataEx.FileName)
        else
          vArchiveFile.FileName := extractFileName(StrPas(FFileHeaderDataEx.FileName));

        vArchiveFile.Size := FFileHeaderDataEx.UnpSize;

        dosDateTimeToFileTime(hiWord(FFileHeaderDataEx.FileTime), loWord(FFileHeaderDataEx.FileTime), ft);
        fileTimeToSystemTime(ft, st);
        vArchiveFile.Time := systemTimeToDateTime(st);

        if fileExists(string(vExistentFile.FileName)) then
          if assigned(FOnReplace) then FOnReplace(SELF, vExistentFile, vArchiveFile, vReplaceResult);

        case vReplaceResult of
          rrCancel:     FAbort := TRUE;
          rrOverwrite:  if bRestoreFolder
                        then
                          vReadFileHeaderResult := RARProcessFile(FArchiveHandle, RAR_EXTRACT, PAnsiChar(aFilePath), NIL)
                        else
                        if (NOT ((FFileHeaderDataEx.Flags AND $00000070) = $00000070)) and (FFileHeaderDataEx.FileAttr <> faDirectory) then
                          vReadFileHeaderResult := RARProcessFile(FArchiveHandle, RAR_EXTRACT, Nil, PAnsiChar(vExistentFile.FileName));
          rrSkip:       begin
                          vReadFileHeaderResult := RARProcessFile(FArchiveHandle, RAR_SKIP, PAnsiChar(aFilePath), NIL);
                          {$WARN COMBINING_SIGNED_UNSIGNED OFF}
                          FProgressInfo.ArchiveBytesDone := FProgressInfo.ArchiveBytesDone + FFileHeaderDataEx.UnpSize;
                          {$WARN COMBINING_SIGNED_UNSIGNED ON}
                        end;
        end;

      end else vReadFileHeaderResult := RARProcessFile(FArchiveHandle, RAR_SKIP, NIL, NIL); // select next file without extracting

      if vReadFileHeaderResult <> RAR_SUCCESS then begin
        result := FALSE;
        checkRARResult(vReadFileHeaderResult, roListFiles);
      end;

      if FAbort then result := FALSE;
    end;
  finally
    closeArchive(FArchiveHandle);
  end;
  if FAbort then result := FALSE;
end;

function TRAR.testArchiveFiles(aArchiveHandle: THANDLE; var aProgressInfo: TRARProgressInfo; var bAbort: boolean): boolean;
var vHeaderDataEx: TRARHeaderDataEx;
begin
  aProgressInfo := default(TRARProgressInfo);

  try
    repeat

      case checkRARResult(RARReadHeaderEx(aArchiveHandle, @vHeaderDataEx), roTest)     = RAR_SUCCESS of FALSE: EXIT; end;  // get the next file header in the archive
      processFileHeader(vHeaderDataEx);

      aProgressInfo.FileBytesDone   := 0;
      aProgressInfo.FileBytesTotal  := vHeaderDataEx.UnpSize;
      aProgressInfo.FileName        := vHeaderDataEx.FileNameW;

      case checkRARResult(RARProcessFile(aArchiveHandle, RAR_TEST, NIL, NIL), roTest)  = RAR_SUCCESS of FALSE: EXIT; end;

      application.processMessages; // allow the user to actually press a cancel button
    until FAbort; // RARReadHeaderEx = ERAR_END_ARCHIVE will usually exit the loop

  finally
    result := FLastResult = ERAR_END_ARCHIVE; // not an error in this case
  end;
end;

function TRAR.testArchive(const aFilePath: string): boolean;
begin
  FAbort := FALSE;
  var vArchiveHandle := openArchive(aFilePath, TRUE);
  case vArchiveHandle = RAR_INVALID_HANDLE of TRUE: EXIT; end;
  RARSetCallback(vArchivehandle, UnRarCallBack, LPARAM(pointer(SELF)));
  case FPassword = '' of FALSE: RARSetPassword(vArchiveHandle, PAnsiChar(FPassword)); end;
  result := testArchiveFiles(vArchiveHandle, FProgressInfo, FAbort);
  closeArchive(vArchiveHandle);
end;

function TRAR.getDLLName: string;
begin
  result := RARDLLName;
end;

function TRAR.getDLLVersion: integer;
begin
  result := RARGetDLLVersion;
end;

procedure TRAR.abort;
begin
  FAbort := TRUE;
end;

function TRAR.checkRARResult(const aResultCode: integer; const aOperation: TRAROperation): integer;
begin
  result      := aResultCode;
  FLastResult := aResultCode;

  FAbort := (aResultCode = RAR_DLL_LOAD_ERROR)
//  or (ResultCode=ERAR_END_ARCHIVE) = 10
  or (aResultCode = ERAR_NO_MEMORY)
  or (aResultCode = ERAR_BAD_DATA)
  or (aResultCode = ERAR_UNKNOWN_FORMAT)
  or (aResultCode = ERAR_EOPEN)
  or (aResultCode = ERAR_ECREATE)
  or (aResultCode = ERAR_ECLOSE)
  or (aResultCode = ERAR_EREAD)
  or (aResultCode = ERAR_EWRITE)
  or (aResultCode = ERAR_SMALL_BUF)
  or (aResultCode = ERAR_UNKNOWN);

  case (FAbort or (aResultCode = RAR_COMMENT_UNKNOWN)) and assigned(FOnError) of TRUE: FOnError(SELF, aResultCode, aOperation); end;
end;

function TRAR.getVersion: string;
begin
  result := GVersion;
end;

end.
