// -----------------------------------------------------------------------------
// Last changed: 23.10.2025
// -----------------------------------------------------------------------------

unit uHelperStrings;

interface

uses
  System.Classes, FMX.TabControl;

type
  TStringsHelper = class helper for TStrings
    private
      function GetTheObject(const AString: string): TObject;
      procedure SetTheObject(const AString: string; const Value: TObject);
    public
      property ObjectFor[const AString : string]: TObject read GetTheObject write SetTheObject;
      function Contains(const AString: string): boolean;
    end;

implementation

{ TStringsHelper }

(*
  Usage example: if ListBox1.Items.Contains('some string') then ...
*)
function TStringsHelper.Contains(const AString: string): boolean;
begin
  Result := -1 <> IndexOf(AString);   // Returns True if found, false if not
end;

function TStringsHelper.GetTheObject(const AString: string): TObject;
var
  idx : integer;
begin
  Result := nil;
  idx := IndexOf(AString);
  if idx > -1 then
    Result := Objects[idx];
end;

procedure TStringsHelper.SetTheObject(const AString: string; const Value: TObject);
var
  idx : integer;
begin
  idx := IndexOf(AString);
  if idx > -1 then
    Objects[idx] := Value;
end;

end.
