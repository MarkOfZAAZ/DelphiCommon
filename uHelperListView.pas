// -----------------------------------------------------------------------------
// Last changed: 23.10.2025 18:01
// -----------------------------------------------------------------------------

unit uHelperListView;

(*
  ListViewHelperTheme has been created to set the searchbox of a TListView to be a different style than
  the default, without having to create a component

  How to use the ListViewHelperTheme...

  In your FormCreate:

  1 Use built-in themes
      ListView1.SetSearchDefault;
      ListView1.SetSearchThemeLight;
      ListView1.SetSearchThemeDark;

  2 Apply your own custom theme
      ListView1.SetSearchStyle(40, 20, TAlphaColors.RoyalBlue, TEditCharCase.ecUpperCase);

  3 Use individual tweaks
      ListView1.SetSearchHeight(44);
      ListView1.SetSearchFontSize(22);
      ListView1.SetSearchFontColor(TAlphaColors.Red);
      ListView1.SetSearchCharCase(const ACharCase: TEditCharCase);
*)

interface

uses
  System.SysUtils, System.UITypes, FMX.ListView, FMX.SearchBox, FMX.Types;

type
  TListViewHelper = class helper for TListView
  private
    function FindSearchBox: TSearchBox;
  public
    // core styling
    procedure SetSearchStyle(const AHeight: Single; const AFontSize: Single; const AFontColor: TAlphaColor; const ACharCase: TEditCharCase);
    procedure SetSearchDefault;
    procedure SetSearchThemeLight;
    procedure SetSearchThemeDark;
    // individual property setters
    procedure SetSearchHeight(const AHeight: Single);
    procedure SetSearchFontSize(const AFontSize: Single);
    procedure SetSearchFontColor(const AColor: TAlphaColor);
    procedure SetSearchCharCase(const ACharCase: TEditCharCase);
    // helper methods
    procedure ClearSearchText;
    procedure SetSearchText(const AText: string);
    function IsSearchClear: Boolean;
  end;

implementation

{ TListViewHelper }

function TListViewHelper.FindSearchBox: TSearchBox;
begin
  Result := nil;
  for var I := 0 to Self.ChildrenCount - 1 do
    if Self.Children[I] is TSearchBox then
      Exit(TSearchBox(Self.Children[I]));
end;

procedure TListViewHelper.SetSearchStyle(const AHeight: Single; const AFontSize: Single; const AFontColor: TAlphaColor; const ACharCase: TEditCharCase);
begin
  Self.SearchVisible := True;
  var SB: TSearchBox := FindSearchBox;
  if Assigned(SB) then
  begin
    SB.StyledSettings := SB.StyledSettings - [TStyledSetting.Size, TStyledSetting.FontColor, TStyledSetting.Style];
    SB.Height := AHeight;
    SB.Font.Size := AFontSize;
    SB.FontColor := AFontColor;
    SB.CharCase := ACharCase;
    SB.Repaint;
    SB.SetBounds(SB.Position.X, SB.Position.Y, SB.Width, AHeight);
  end;
end;

procedure TListViewHelper.SetSearchHeight(const AHeight: Single);
begin
  var SB: TSearchBox := FindSearchBox;
  if Assigned(SB) then
  begin
    SB.Height := AHeight;
    SB.SetBounds(SB.Position.X, SB.Position.Y, SB.Width, AHeight);
    SB.Repaint;
  end;
end;

procedure TListViewHelper.SetSearchFontSize(const AFontSize: Single);
begin
  var SB: TSearchBox := FindSearchBox;
  if Assigned(SB) then
  begin
    SB.StyledSettings := SB.StyledSettings - [TStyledSetting.Size];
    SB.Font.Size := AFontSize;
    SB.Repaint;
  end;
end;

procedure TListViewHelper.SetSearchFontColor(const AColor: TAlphaColor);
begin
  var SB: TSearchBox := FindSearchBox;
  if Assigned(SB) then
  begin
    SB.StyledSettings := SB.StyledSettings - [TStyledSetting.FontColor];
    SB.FontColor := AColor;
    SB.Repaint;
  end;
end;

procedure TListViewHelper.SetSearchCharCase(const ACharCase: TEditCharCase);
begin
  var SB: TSearchBox := FindSearchBox;
  if Assigned(SB) then
    SB.CharCase := ACharCase;
end;

procedure TListViewHelper.ClearSearchText;
begin
  var SB: TSearchBox := FindSearchBox;
  if Assigned(SB) then
    SB.Text := '';
end;

procedure TListViewHelper.SetSearchText(const AText: string);
begin
  var SB: TSearchBox := FindSearchBox;
  if Assigned(SB) then
    SB.Text := AText;
end;

procedure TListViewHelper.SetSearchDefault;
begin
  SetSearchStyle(34, 18, TAlphaColors.Darkorchid, TEditCharCase.ecUpperCase)
end;

procedure TListViewHelper.SetSearchThemeDark;
begin
  SetSearchStyle(34, 18, TAlphaColors.Chartreuse, TEditCharCase.ecUpperCase)
end;

procedure TListViewHelper.SetSearchThemeLight;
begin
  SetSearchStyle(34, 18, TAlphaColors.Black, TEditCharCase.ecUpperCase)
end;

function TListViewHelper.IsSearchClear: Boolean;
begin
  var SB: TSearchBox := FindSearchBox;
  Result := Assigned(SB) and (SB.Text = '');
end;

end.

