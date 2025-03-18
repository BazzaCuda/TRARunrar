program testProject;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  RAR in '..\RAR.pas',
  RAR_DLL in '..\RAR_DLL.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
