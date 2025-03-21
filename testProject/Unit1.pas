unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RAR, Vcl.StdCtrls, Vcl.Mask, Vcl.ExtCtrls, Vcl.DBCtrls,
  system.generics.collections;

type
  TForm1 = class(TForm)
    RAR1: TRAR;
    label1: TLabel;
    lblError: TLabel;
    edtRARArchive: TLabeledEdit;
    Label2: TLabel;
    btnListFiles: TButton;
    Memo1: TMemo;
    btnExtract: TButton;
    Label3: TLabel;
    edtExtractPath: TLabeledEdit;
    edtPassword: TLabeledEdit;
    Label4: TLabel;
    Label5: TLabel;
    btnFindFiles: TButton;
    lblLastError: TLabel;
    procedure RAR1ListFile(Sender: TObject; const aFileInformation: TRARFileItem);
    procedure RAR1Error(Sender: TObject; const aErrorCode: Integer; const aOperation: TRAROperation);
    procedure RAR1NextVolumeRequired(Sender: TObject; const aRequiredFileName: AnsiString; out oNewFileName: AnsiString; out oCancel: Boolean);
    procedure RAR1PasswordRequired(Sender: TObject; const aFileName: AnsiString; out oNewPassword: AnsiString; out oCancel: Boolean);
    procedure RAR1Progress(Sender: TObject; const aProgressInfo: TRARProgressInfo);
    procedure btnListFilesClick(Sender: TObject);
    procedure btnExtractClick(Sender: TObject);
    procedure btnFindFilesClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TDebug = class(TObject)
     class function debugEnum<T>(const value: T): string;
   end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses RTTI;

procedure TForm1.btnExtractClick(Sender: TObject);
begin
  RAR1.extractArchive(edtRARArchive.Text, edtExtractPath.Text); // auto-creates the extraction path folder
  lblLastError.caption := format('Last result code: %d', [RAR1.lastResult]);
end;

procedure TForm1.btnFindFilesClick(Sender: TObject);
begin
  memo1.lines.clear;
  RAR1.findFiles('..\..', TRUE);
  RAR1.foundFiles.sorted := TRUE;
  memo1.lines.assign(RAR1.foundFiles);
end;

procedure TForm1.btnListFilesClick(Sender: TObject);
begin
  memo1.Lines.Clear;
  case edtPassword.text <> '' of TRUE: RAR1.password := edtPassword.Text; end;
  RAR1.listArchive(edtRARArchive.text); // triggers the onListFile event for each file in the archive
end;

procedure TForm1.RAR1Error(Sender: TObject; const aErrorCode: Integer; const aOperation: TRAROperation);

begin
  lblError.Caption := format('code: %d, operation: %s', [aErrorCode, TDebug.debugEnum<TRAROperation>(aOperation)]);
end;

procedure TForm1.RAR1ListFile(Sender: TObject; const aFileInformation: TRARFileItem);
begin
  memo1.Lines.add(aFileInformation.fileNameW);
end;

procedure TForm1.RAR1NextVolumeRequired(Sender: TObject; const aRequiredFileName: AnsiString; out oNewFileName: AnsiString; out oCancel: Boolean);
begin
//
end;

procedure TForm1.RAR1PasswordRequired(Sender: TObject; const aFileName: AnsiString; out oNewPassword: AnsiString; out oCancel: Boolean);
// You can either provide the password up-front (RAR1.password := '<pw>') or let TRAR request it when required
begin
  case edtPassword.text = '' of TRUE: oNewPassword := 'this'; end;
end;

procedure TForm1.RAR1Progress(Sender: TObject; const aProgressInfo: TRARProgressInfo);
begin
//  N.B. excessive updating of labels or progressbars, etc. can significantly impact speed
end;

{ TDebug }

class function TDebug.debugEnum<T>(const value: T): string;
begin
  result := TRttiEnumerationType.getName(value);
end;

end.
