unit uHelperComboBox;

interface

uses
  System.SysUtils, System.UITypes, System.Classes,
  FMX.Objects, FMX.Types, FMX.Controls, FMX.ListBox, FMX.Edit, FMX.ComboEdit;

type
  TComboEditHelper = class helper for TComboEdit
    procedure SetCharCase(Value: TEditCharCase);
  end;

type
  TComboBoxHelper = class helper for TComboBox
  private
    procedure StyleListItem(AItem: TListBoxItem; const ASize: Single; const AColour: TAlphaColor);
  public
    procedure ApplyFontStyle(const ASize: Single; const AColour: TAlphaColor);
  end;

implementation

{ TComboBoxHelper }

procedure TComboBoxHelper.StyleListItem(AItem: TListBoxItem; const ASize: Single; const AColour: TAlphaColor);
begin
  if not Assigned(AItem) then
    Exit;
  AItem.StyledSettings := [];
  AItem.Font.Size := ASize;
  AItem.FontColor := AColour;
  AItem.StyledSettings := AItem.StyledSettings - [TStyledSetting.Size, TStyledSetting.FontColor];
end;

procedure TComboBoxHelper.ApplyFontStyle(const ASize: Single; const AColour: TAlphaColor);
var
  i: Integer;
  Txt: TFmxObject;
begin
  Txt := FindStyleResource('content');
  if Txt = nil then
    Txt := FindStyleResource('text');
  if Assigned(Txt) and (Txt is TText) then
  begin
    TText(Txt).Font.Size := ASize;
    TText(Txt).TextSettings.FontColor := AColour;
  end;
  for i := 0 to Pred(Self.Count) do
    StyleListItem(Self.ListItems[i], ASize, AColour);   // Style all dropdown items DOESN'T DO OWT!
end;

{ TComboEditHelper }

procedure TComboEditHelper.SetCharCase(Value: TEditCharCase);
begin
  Self.CharCase := Value;
end;

end.

