//  written by Philippe Wechsler 2008
//
//  web: www.PhilippeWechsler.ch
//  mail: contact@PhilippeWechsler.ch
//
//  This is a simple demo showing TRAR in action...

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RAR, ComCtrls, Menus,RAR_DLL, StdCtrls, ExtCtrls, FileCtrl;

type
  TMainForm = class(TForm)
    RARArchive: TRAR;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    N1: TMenuItem;
    Close1: TMenuItem;
    Open1: TMenuItem;
    N2: TMenuItem;
    ListView: TListView;
    OpenDialog: TOpenDialog;
    LogMemo: TMemo;
    Archiveinformation1: TMenuItem;
    N3: TMenuItem;
    Archive1: TMenuItem;
    test1: TMenuItem;
    Extract1: TMenuItem;
    About1: TMenuItem;
    Dllversion1: TMenuItem;
    Panel1: TPanel;
    OverallBar: TProgressBar;
    SingleBar: TProgressBar;
    fileLabel: TLabel;
    CancelButton: TButton;
    ExtractSelected1: TMenuItem;
    N4: TMenuItem;
    refresh1: TMenuItem;
    procedure Close1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Archiveinformation1Click(Sender: TObject);
    procedure test1Click(Sender: TObject);
    procedure Extract1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Dllversion1Click(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure ExtractSelected1Click(Sender: TObject);
    procedure RARArchiveError(Sender: TObject; const ErrorCode: Integer;
      const Operation: TRAROperation);
    procedure RARArchiveListFile(Sender: TObject;
      const FileInformation: TRARFileItem);
    procedure RARArchiveNextVolumeRequired(Sender: TObject;
      const requiredFileName: string; out newFileName: string;
      out Cancel: Boolean);
    procedure RARArchivePasswordRequired(Sender: TObject;
      const HeaderPassword: Boolean; const FileName: string;
      out NewPassword: string; out Cancel: Boolean);
    procedure RARArchiveProgress(Sender: TObject; const FileName: WideString;
      const ArchiveBytesTotal, ArchiveBytesDone, FileBytesTotal,
      FileBytesDone: Cardinal);
    procedure RARArchiveReplace(Sender: TObject; const ExistingData,
      NewData: TRARReplaceData; out Action: TRARReplace);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  MainForm: TMainForm;

implementation

uses Replace;

{$R *.dfm}

function GetSizeName(const Size : int64): String;     //just a helper method to improve readybility
begin
  Result := 'error';
  if Size < 0 then exit; 
  if Size < 1024 then 
  begin 
    Result := inttostr(Size)+' Byte'; 
    exit; 
  end; 
  if (1024 <= Size) and (Size < 1048576) then 
  begin 
    Result := floattostr((round((Size/1024)*100))/100)+' KB'; 
    exit; 
  end; 
  if (1048576 <= Size) and (Size < 1073741824) then
  begin 
    Result := floattostr((round((Size/1048576)*100))/100)+' MB'; 
    exit; 
  end;
  if Size = 1073741824 then
    Result:=('1 GB');
  if Size > 1073741824 then
  begin
    Result := floattostr((round((Size/1073741824)*100))/100)+' GB';
  end; 
end;

function BoolToStr(value:boolean):String;           //just a helper method to improve readybility
begin
  if Value then
    Result:='True'
  else
    Result:='False';
end;

procedure TMainForm.About1Click(Sender: TObject);
begin
  Application.MessageBox(
  PChar('written by Philippe Wechsler, 2008'+#10#13+
    ''+#10#13+
    'www.PhilippeWechsler.ch'+#10#13+
    'contact@PhilippeWechsler.ch'+#10#13+
    ''+#10#13+
    'This is a demo application for the UnRar Library for Delphi by Philippe Wechsler'+#10#13+
    'Source for this demo and the UnRar Component for free on my site...'+#10#13+
    #10#13+'TRAR version: '+RARArchive.Version
    +#10#13+
    ''+#10#13+'have fun ;-)'
  ),'about',MB_OK or MB_ICONINFORMATION);
end;

procedure TMainForm.Archiveinformation1Click(Sender: TObject);
begin
  Application.MessageBox(
  PChar('files and folders: '+inttostr(RARArchive.ArchiveInformation.TotalFiles)+#10#13+
    'compressed: '+getSizeName(RARArchive.ArchiveInformation.CompressedSize)+#10#13+
    'uncompressed: '+getSizeName(RARArchive.ArchiveInformation.UnCompressedSize)+#10#13+
    'OS: '+RARArchive.ArchiveInformation.HostOS+#10#13+
    'version: '+inttostr(RARArchive.ArchiveInformation.ArchiverMajorVersion)+'.'+inttostr(RARArchive.ArchiveInformation.ArchiverMinorVersion)+#10#13+
    'dictionary size: '+getSizeName(RARArchive.ArchiveInformation.DictionarySize)+#10#13+
    'locked: '+BoolToStr(RARArchive.ArchiveInformation.Locked)+#10#13+
    'signed: '+BoolToStr(RARArchive.ArchiveInformation.Signed)+#10#13+
    'recovery: '+BoolToStr(RARArchive.ArchiveInformation.Recovery)+#10#13+
    'solid: '+BoolToStr(RARArchive.ArchiveInformation.Solid)+#10#13+
    'multivolume: '+BoolToStr(RARArchive.ArchiveInformation.MultiVolume)+#10#13+
    'encrypted: '+BoolToStr(RARArchive.ArchiveInformation.Encryption)+#10#13+
    'header encrypted: '+BoolToStr(RARArchive.ArchiveInformation.HeaderEncrypted)+#10#13+
    'sfx: '+BoolToStr(RARArchive.ArchiveInformation.sfx)+#10#13+
    'file comment: '+BoolToStr(RARArchive.ArchiveInformation.FileComment)+#10#13+
    'archive comment: '+BoolToStr(RARArchive.ArchiveInformation.ArchiveComment)+#10#13+
    'comment: '+RARArchive.ArchiveInformation.Comment
  ),'archive information',MB_OK or MB_ICONINFORMATION);
end;

procedure TMainForm.CancelButtonClick(Sender: TObject);
begin
  RARArchive.Abort;
end;

procedure TMainForm.Close1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.Dllversion1Click(Sender: TObject);
begin
  ShowMessage(inttostr(RARArchive.getDllVersion));
end;

procedure TMainForm.Extract1Click(Sender: TObject);
var
  Path:String;
begin
  if SelectDirectory('Select a directory', 'C:\', Path) then
    if RARArchive.Extract(path,True,nil) then showmessage('done')
      else
    showmessage('failed');
end;

procedure TMainForm.ExtractSelected1Click(Sender: TObject);
var
  Path:String;
  i:integer;
  files:TStrings;
begin
  files:=TStringList.Create;                     //write all selected files into "files" list
  for i := 0 to ListView.Items.Count - 1 do
    if ListView.Items.Item[i].Selected then
    files.add(ListView.Items.Item[i].Caption);

  if SelectDirectory('Select a directory', 'C:\', Path) then
    if RARArchive.Extract(path,True,files) then showmessage('done')
      else
    showmessage('failed');

  files.free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DoubleBuffered:=True;
  OverallBar.DoubleBuffered:=True;
  SingleBar.DoubleBuffered:=True;
end;

procedure TMainForm.Open1Click(Sender: TObject);
begin
  if OpenDialog.Execute then begin
    ListView.Items.BeginUpdate;
    ListView.Items.Clear;
    if RARArchive.OpenFile(OpenDialog.FileName) then
      Archiveinformation1.Click
    else
      showmessage('error opening file');
     ListView.Items.EndUpdate;
  end;
end;

procedure TMainForm.RARArchiveError(Sender: TObject; const ErrorCode: Integer;
  const Operation: TRAROperation);
var
  op:String;
begin
  case operation of
    roInitArchive: op:='init arc';
    roListFiles: op:='list arc';
    roExtract: op:='extract arc';
    roTest: op:='test arc';
  end;
  LogMemo.lines.add(op+': '+inttostr(ErrorCode));
end;

procedure TMainForm.RARArchiveListFile(Sender: TObject;
  const FileInformation: TRARFileItem);
var
  Attrib:String;
begin
  with listView.Items.Add do begin
    Caption:=FileInformation.FileNameW;
    SubItems.Add(getSizeName(FileInformation.CompressedSize));
    SubItems.Add(getSizeName(FileInformation.UncompressedSize));
    Attrib:='';
    case fileinformation.Attributes of
      {$WARN SYMBOL_PLATFORM OFF}
      faReadOnly: Attrib:=Attrib+'R';
      faHidden: Attrib:=Attrib+'H';
      faSysFile: Attrib:=Attrib+'S';
      faVolumeID: Attrib:=Attrib+'V';
      faDirectory: Attrib:=Attrib+'D';
      faArchive: Attrib:=Attrib+'A';
      faAnyFile: Attrib:=Attrib+'F';
      {$WARN SYMBOL_PLATFORM ON}
    end;
    subitems.add(Attrib);
    SubItems.Add(FileInformation.HostOS);
    Subitems.Add(FileInformation.Comment);
    subitems.Add(formatdatetime('c',FileInformation.Time));
    case FileInformation.CompressionStrength of
      48: subitems.Add('stored');
      49: subitems.Add('fastest');
      50: subitems.Add('fast');
      51: subitems.Add('normal');
      52: subitems.Add('good');
      53: subitems.Add('best');
    end;
    subitems.add(inttostr(FileInformation.ArchiverVersion div 10)+'.'+(inttostr(FileInformation.ArchiverVersion mod 10)));
    subitems.add(BoolToStr(fileinformation.Encrypted));
    Subitems.Add(FileInformation.CRC32);
  end;
end;

procedure TMainForm.RARArchiveNextVolumeRequired(Sender: TObject;
  const requiredFileName: string; out newFileName: string; out Cancel: Boolean);
begin
  newFileName:=requiredFileName;
  Cancel:=not InputQuery('next volume',requiredFileName,newFileName);
end;

procedure TMainForm.RARArchivePasswordRequired(Sender: TObject;
  const HeaderPassword: Boolean; const FileName: string;
  out NewPassword: string; out Cancel: Boolean);
begin
  Cancel:=not InputQuery('Password required for '+extractFileName(FileName),extractFileName(FileName),NewPassword);
end;

procedure TMainForm.RARArchiveProgress(Sender: TObject; const FileName: WideString;
  const ArchiveBytesTotal, ArchiveBytesDone, FileBytesTotal,
  FileBytesDone: Cardinal);
begin
  fileLabel.Caption:='archive: '+getSizeName(ArchiveBytesDone)+' of '+getSizeName(ArchiveBytesTotal)+', file: '+getSizeName(FileBytesDone)+' of '+getSizeName(FileBytesTotal)+' '+FileName;
  if FileBytesTotal>0 then
    SingleBar.Position:=round(100 / FileBytesTotal * FileBytesDone)
  else
    SingleBar.Position:=0;
  if ArchiveBytesTotal>0 then
    OverallBar.Position:=round(100 / ArchiveBytesTotal * ArchiveBytesDone)
  else
    OverallBar.Position:=0;
  fileLabel.Repaint;
  SingleBar.Repaint;
  OverallBar.Repaint;
  Application.ProcessMessages;          //you can use threads to improve stability and smoothness of your application
end;

procedure TMainForm.RARArchiveReplace(Sender: TObject; const ExistingData,
  NewData: TRARReplaceData; out Action: TRARReplace);
var
  ReplaceForm:TReplaceForm;
begin
  ReplaceForm:=TReplaceForm.Create(Self);
  ReplaceForm.existent.Caption:='existent: '+ExistingData.FileName;
  ReplaceForm.archive.Caption:='in archive: '+NewData.FileName;
  ReplaceForm.ShowModal;
  Action:=ReplaceForm.Action;
  ReplaceForm.Free;
end;

procedure TMainForm.test1Click(Sender: TObject);
begin
  if RARArchive.Test then
    showmessage('no error found')
  else
    showmessage('archive is damaged');
end;

end.
