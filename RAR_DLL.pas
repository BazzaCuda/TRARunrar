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

unit RAR_DLL;

interface

uses
  windows, sysUtils;

const
  RAR_METHOD_STORE      = 48;
  RAR_METHOD_FASTEST    = 49;
  RAR_METHOD_FAST       = 50;
  RAR_METHOD_NORMAL     = 51;
  RAR_METHOD_GOOD       = 52;
  RAR_METHOD_BEST       = 53;

  RAR_SUCCESS           =  0;
  ERAR_END_ARCHIVE      = 10;
  ERAR_NO_MEMORY        = 11;
  ERAR_BAD_DATA         = 12;
  ERAR_BAD_ARCHIVE      = 13;
  ERAR_UNKNOWN_FORMAT   = 14;
  ERAR_EOPEN            = 15;
  ERAR_ECREATE          = 16;
  ERAR_ECLOSE           = 17;
  ERAR_EREAD            = 18;
  ERAR_EWRITE           = 19;
  ERAR_SMALL_BUF        = 20;
  ERAR_UNKNOWN          = 21;
  ERAR_MISSING_PASSWORD = 22;
  ERAR_EREFERENCE       = 23;
  ERAR_BAD_PASSWORD     = 24;
  ERAR_LARGE_DICT       = 25;

  RAR_OM_LIST           =  0;
  RAR_OM_EXTRACT        =  1;
  RAR_OM_LIST_INCSPLIT  =  2;

  RAR_SKIP              =  0;
  RAR_TEST              =  1;
  RAR_EXTRACT           =  2;

  RAR_VOL_ASK           =  0;
  RAR_VOL_NOTIFY        =  1;

  RAR_DLL_VERSION       =  3;

//
  RAR_COMMENT_EXISTS    =  1;
  RAR_NO_COMMENT        =  0;
  RAR_COMMENT_UNKNOWN   = 98;
  RAR_DLL_LOAD_ERROR    = 99;
  RAR_INVALID_HANDLE    =  0;

  UCM_CHANGEVOLUME      =  0;
  UCM_PROCESSDATA       =  1;
  UCM_NEEDPASSWORD      =  2;

  RAR_MAX_COMMENT_SIZE  = 65536;
  RAR_MIN_VERSION       =  4;

  RAR_CANCEL            = -1;
  RAR_CONTINUE          =  0;

type
  TProcessDataProc  = function(Addr: PByte; Size: integer): integer;
  TChangeVolProc    = function(ArcName: PAnsiChar; Mode: integer): integer; {$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};
  TUnRarCallBack    = function(msg: Cardinal; UserData: LPARAM; P1: LPARAM; P2: LPARAM): integer; {stdcall;} {$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};

  {$ALIGN 1}
  TRARHeaderData = record
    ArcName:    array[0..259] of AnsiChar;
    FileName:   array[0..259] of AnsiChar;
    Flags:      cardinal;
    PackSize:   cardinal;
    UnpSize:    cardinal;
    HostOS:     cardinal;
    FileCRC:    cardinal;
    FileTime:   cardinal;
    UnpVer:     cardinal;
    Method:     cardinal;
    FileAttr:   cardinal;
    CmtBuf:     PAnsiChar;
    CmtBufSize: cardinal;
    CmtSize:    cardinal;
    CmtState:   cardinal;
  end;
  {$A-} // Reset alignment to default
  PRARHeaderData = ^TRARHeaderData;

  //for UniCode FileNames and 64-Bit Sizes
  {$ALIGN 1}
  TRARHeaderDataEx = record
    ArcName:      array[0..1023] of AnsiChar;
    ArcNameW:     array[0..1023] of WideChar;
    FileName:     array[0..1023] of AnsiChar;
    FileNameW:    array[0..1023] of WideChar;
    Flags:        cardinal;
    PackSize:     cardinal;
    PackSizeHigh: cardinal;
    UnpSize:      cardinal;
    UnpSizeHigh:  cardinal;
    HostOS:       cardinal;
    FileCRC:      cardinal;
    FileTime:     cardinal;
    UnpVer:       cardinal;
    Method:       cardinal;
    FileAttr:     cardinal;
    CmtBuf:       PAnsiChar;
    CmtBufSize:   cardinal;
    CmtSize:      cardinal;
    CmtState:     cardinal;
    DictSize:     cardinal;                   // Baz
    HashType:     cardinal;                   // Baz
    Blake2:       array[0..31] of byte;       // Baz
    Reserved:     array[0..981] of cardinal;
  end;
  {$A-} // Reset alignment to default
  PRARHeaderDataEx = ^TRARHeaderDataEx;

  {$ALIGN 1}
  TRAROpenArchiveData = record
    ArcName:    PAnsiChar;
    OpenMode:   cardinal;
    OpenResult: cardinal;
    CmtBuf:     PAnsiChar;
    CmtBufSize: cardinal;
    CmtSize:    cardinal;
    CmtState:   cardinal;
  end;
  {$A-} // Reset alignment to default
  PRAROpenArchiveData = ^TRAROpenArchiveData;

  {$ALIGN 1}
  TRAROpenArchiveDataEx = record
    ArcName:      PAnsiChar;
    ArcNameW:     PWideChar;
    OpenMode:     cardinal;
    OpenResult:   cardinal;
    CmtBuf:       PAnsiChar;
    CmtBufSize:   cardinal;
    CmtSize:      cardinal;
    CmtState:     cardinal;
    Flags:        cardinal;
    Callback:     cardinal;
    LParam:       cardinal;
    OpFlags:      cardinal;
    CmtBufW:      PWideChar;
    MarkOfTheWeb: PWideChar;
    Reserved:   array[1..23] of cardinal;
  end;
  {$A-} // Reset alignment to default
  PRAROpenArchiveDataEx = ^TRAROpenArchiveDataEx;

var
  RAROpenArchive:         function  (ArchiveData: PRAROpenArchiveData):   THandle;                                                {$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};
  RAROpenArchiveEx:       function  (ArchiveData: PRAROpenArchiveDataEx): THandle;                                                {$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};

  RARCloseArchive:        function  (hArcData: THandle):                                                                integer;  {$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};
  RARReadHeader:          function  (hArcData: THandle; HeaderData: PRARHeaderData):                                    integer;  {$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};
  RARReadHeaderEx:        function  (hArcData: THandle; HeaderData: PRARHeaderDataEx):                                  integer;  {$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};

  RARProcessFile:         function  (hArcData: THandle; Operation: integer; DestPath: PAnsiChar; DestName: PAnsiChar):  integer;  {$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};
  RARProcessFileW:        function  (hArcData: THandle; Operation: integer; DestPath: PWideChar; DestName: PWideChar):  integer;  {$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};

  RARSetCallback:         procedure (hArcData: THandle; Callback:         TUnRarCallback; UserData: LPARAM);                      {$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};
  RARSetChangeVolProc:    procedure (hArcData: THandle; ChangeVolProc:    TChangeVolProc);                                        {$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};
  RARSetProcessDataProc:  procedure (hArcData: THandle; ProcessDataProc:  TProcessDataProc);                                      {$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};
  RARSetPassword:         procedure (hArcData: THandle; Password:         PAnsiChar);                                             {$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};
  RARGetDllVersion:       function:                                                                                     integer;  {$IFDEF Win32} stdcall {$ELSE} cdecl {$ENDIF};

function getFileModifyDate(const fileName: string): TDateTime;
function getFileSize(const s: string): int64;
function isSFX(const fileName:String): boolean;

implementation

function getFileModifyDate(const fileName:string): TDateTime;
var
  vHandle:    THandle;
  vStruct:    TOFStruct;
  vLastWrite: integer;
begin
  result  := 0;
  vHandle := openFile(PAnsiChar(fileName), vStruct, OF_SHARE_DENY_NONE);
  try
    if vHandle <> HFILE_ERROR then
    begin
      vLastwrite := fileGetDate(vHandle);
      result    := fileDateToDateTime(vLastwrite);
    end;
  finally
    closeHandle(vHandle);
  end;
end;

function getFileSize(const s:string): int64;
var
  vFindData:  TWin32FindDataA;
  vHandle:    cardinal;
begin
  vHandle := findFirstFileA(PAnsiChar(s), vFindData);
  if (vHandle <> INVALID_HANDLE_VALUE) then
  begin
    result := vFindData.nFileSizeLow;
    PCardinal(cardinal(@result) + sizeOf(cardinal))^ := vFindData.nFileSizeHigh;
    windows.findClose(vHandle);
  end
  else
    result := 0;
end;

function isSFX(const fileName:String):boolean;
var
  vBinaryType: DWORD;
begin
  if getBinaryTypeA(PAnsiChar(fileName), vBinaryType) then  begin
                                                              if (vBinaryType = SCS_32BIT_BINARY)
                                                              or (vBinaryType = SCS_DOS_BINARY)
                                                              or (vBinaryType = SCS_WOW_BINARY)
                                                              or (vBinaryType = SCS_PIF_BINARY)
                                                              or (vBinaryType = SCS_POSIX_BINARY)
                                                              or (vBinaryType = SCS_OS216_BINARY)
                                                              then result := TRUE
                                                              else result := FALSE;
                                                            end
  else
    result := FALSE;
end;

end.
