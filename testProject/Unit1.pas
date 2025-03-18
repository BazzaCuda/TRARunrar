unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RAR;

type
  TForm1 = class(TForm)
    RAR1: TRAR;
    procedure RAR1ListFile(Sender: TObject; const aFileInformation: TRARFileItem);
    procedure RAR1Error(Sender: TObject; const aErrorCode: Integer; const aOperation: TRAROperation);
    procedure RAR1NextVolumeRequired(Sender: TObject; const aRequiredFileName: AnsiString; out oNewFileName: AnsiString; out oCancel: Boolean);
    procedure RAR1PasswordRequired(Sender: TObject; const aHeaderPassword: Boolean; const aFileName: AnsiString; out oNewPassword: AnsiString; out oCancel: Boolean);
    procedure RAR1Progress(Sender: TObject; const aProgressInfo: TRARProgressInfo);
    procedure RAR1Replace(Sender: TObject; const aExistingData: TRARReplaceData; aNewData: TRARReplaceData; out oAction: TRARReplace);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.RAR1Error(Sender: TObject; const aErrorCode: Integer; const aOperation: TRAROperation);
begin
//
end;

procedure TForm1.RAR1ListFile(Sender: TObject; const aFileInformation: TRARFileItem);
begin
//
end;

procedure TForm1.RAR1NextVolumeRequired(Sender: TObject; const aRequiredFileName: AnsiString; out oNewFileName: AnsiString; out oCancel: Boolean);
begin
//
end;

procedure TForm1.RAR1PasswordRequired(Sender: TObject; const aHeaderPassword: Boolean; const aFileName: AnsiString; out oNewPassword: AnsiString; out oCancel: Boolean);
begin
//
end;

procedure TForm1.RAR1Progress(Sender: TObject; const aProgressInfo: TRARProgressInfo);
begin
//
end;

procedure TForm1.RAR1Replace(Sender: TObject; const aExistingData: TRARReplaceData; aNewData: TRARReplaceData; out oAction: TRARReplace);
begin
//
end;

end.
