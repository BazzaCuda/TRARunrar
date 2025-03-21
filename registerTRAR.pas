unit registerTRAR;
//  originally written by Philippe Wechsler in 2008
//  completely re-written by Baz Cuda in 2025
//
//  web: www.PhilippeWechsler.ch
//  mail: contact@PhilippeWechsler.ch
//
//  please see license.txt and documentation.txt
//
//  changes in 2.0 stable (2025, Baz Cuda - https://github.com/BazzaCuda/TRARunrar/)
//  - supports both the 32-bit and 64-bit versions of unrar.dll
//  - switched to using the ...Ex functions in unrar.dll to get added info
//  - each file's checksum (e.g. Blake2) is now accessible
//  - support for WideChar filenames in archives
//  - reformatted the code and added significant amounts of whitespace for enhanced readability
//  - completely re-architected and rewritten
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
interface

// for the Delphi IDE only

uses
  designEditors, designIntf;

type
  TVersionPropertyEditor = class(TStringProperty)
  public
    function getAttributes: TPropertyAttributes; override;
  end;

procedure Register;

implementation

uses system.classes, RAR;

procedure Register;
begin
  RegisterComponents('Baz Cuda', [TRAR]);
end;


function TVersionPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  result := [paReadOnly, paValueList]; // Make it read-only and show as a list
end;

initialization
  registerPropertyEditor(TypeInfo(string), TRAR, 'Version', TVersionPropertyEditor);

end.
