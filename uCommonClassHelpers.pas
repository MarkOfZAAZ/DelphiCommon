// -----------------------------------------------------------------------------
// Copyright © 1994 - 2025 Aldwicks Limited
//
// Last changed: 22.10.2025 17:03
// -----------------------------------------------------------------------------

unit uCommonClassHelpers;

interface

uses
  FMX.TabControl;

{ The idea here is to simplfy tabmovement, saves lots of typing! }
type
  TTabControlHelper = class helper for TTabControl
  public
    procedure TabLeft(TargetTab: TTabItem);
    procedure TabRight(TargetTab: TTabItem);
  end;

implementation

{ TTabControl Helper procedures }

procedure TTabControlHelper.TabLeft(TargetTab: TTabItem);
begin
  if Assigned(TargetTab) then
    Self.SetActiveTabWithTransitionAsync(TargetTab, TTabTransition.Slide, TTabTransitionDirection.Reversed, nil);
end;

procedure TTabControlHelper.TabRight(TargetTab: TTabItem);
begin
  if Assigned(TargetTab) then
    Self.SetActiveTabWithTransitionAsync(TargetTab, TTabTransition.Slide, TTabTransitionDirection.Normal, nil);
end;

end.
