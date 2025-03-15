program RARComponent_Demo;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  RAR_DLL in '..\RAR_DLL.pas',
  Replace in 'Replace.pas' {ReplaceForm};

{$R *.res}

begin
  {$WARN SYMBOL_PLATFORM OFF}
    ReportMemoryLeaksOnShutDown := debughook<>0; //detect memory leaks
  {$WARN SYMBOL_PLATFORM ON}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  //Application.CreateForm(TReplaceForm, ReplaceForm);
  Application.Run;
end.
