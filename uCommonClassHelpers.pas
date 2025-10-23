// -----------------------------------------------------------------------------
// Last changed: 23.10.2025
// -----------------------------------------------------------------------------

unit uCommonClassHelpers;

interface

uses
  System.Classes, FMX.TabControl;

{ The idea here is to simplfy tabmovement, saves lots of typing! }
type
  TTabControlHelper = class helper for TTabControl
  public
    // Changed so can choose to show the slide (default) or not
    procedure TabLeft(TargetTab: TTabItem; Slide: boolean = true);
    procedure TabRight(TargetTab: TTabItem; Slide: boolean = true);
  end;

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

{ TTabControl Helper procedures }

procedure TTabControlHelper.TabLeft(TargetTab: TTabItem; Slide: boolean = true);
begin
  if not Assigned(TargetTab) then
    exit;
  if Slide then
    Self.SetActiveTabWithTransitionAsync(TargetTab, TTabTransition.Slide, TTabTransitionDirection.Reversed, nil)
  else
    Self.ActiveTab := TargetTab;
end;

procedure TTabControlHelper.TabRight(TargetTab: TTabItem; Slide: boolean = true);
begin
  if not Assigned(TargetTab) then
    exit;
  if Slide then
    Self.SetActiveTabWithTransitionAsync(TargetTab, TTabTransition.Slide, TTabTransitionDirection.Normal, nil)
  else
    Self.ActiveTab := TargetTab;
end;

{ TStringsHelper }

(*
  Usage example: if ListBox1.Items.Contains('some string') then ...
*)
function TStringsHelper.Contains(const AString: string): boolean;
begin
  // Returns True if found, false if not
  Result := -1 <> IndexOf(AString);
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
