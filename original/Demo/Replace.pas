//  written by Philippe Wechsler 2008
//
//  web: www.PhilippeWechsler.ch
//  mail: contact@PhilippeWechsler.ch
//
//  This is a simple demo showing TRAR in action...

unit Replace;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,RAR;

type
  TReplaceForm = class(TForm)
    existent: TLabel;
    archive: TLabel;
    replaceButton: TButton;
    cancelButton: TButton;
    skipButton: TButton;
    procedure replaceButtonClick(Sender: TObject);
    procedure skipButtonClick(Sender: TObject);
    procedure cancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    Action: TRARReplace;
  end;

var
  ReplaceForm: TReplaceForm;

implementation

{$R *.dfm}

procedure TReplaceForm.FormCreate(Sender: TObject);
begin
  Action:=rrCancel; //if close without selection repalce or skip
end;

procedure TReplaceForm.cancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TReplaceForm.replaceButtonClick(Sender: TObject);
begin
  Action:=rrOverwrite;
  Close;
end;

procedure TReplaceForm.skipButtonClick(Sender: TObject);
begin
  Action:=rrSkip;
  Close;
end;

end.
