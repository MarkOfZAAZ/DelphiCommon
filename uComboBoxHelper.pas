unit uComboBoxHelper;

interface

uses
  System.UITypes, FMX.ComboBox, FMX.ListBox, FMX.Types, FMX.Controls, FMX.Graphics;

type
  TComboBoxHelper = class helper for TComboBox
  private
    FAppliedFontSize: Single;
    FAppliedFontColor: TAlphaColor;
    FPopupHooked: Boolean;
    FOriginalOnPopup: TNotifyEvent;

    procedure ForceFMXStyle;
    procedure HookOnPopup;
    procedure InternalOnPopup(Sender: TObject);
    procedure StyleDropdownItems;
    procedure StyleDisplayedText;
    procedure StyleListItem(AItem: TListBoxItem);
  public
    /// <summary>
    /// Apply font size and color to the displayed text and all dropdown items (automatic)
    /// </summary>
    procedure ApplyFontStyle(const ASize: Single; const AColour: TAlphaColor);
  end;

implementation

{ TComboBoxHelper }

procedure TComboBoxHelper.ForceFMXStyle;
begin
  if not SameText(StyleLookup, 'comboboxstylebox') then
  begin
    StyleLookup := 'comboboxstylebox';
    ApplyStyleLookup;
  end;
end;

procedure TComboBoxHelper.HookOnPopup;
begin
  if not FPopupHooked then
  begin
    FOriginalOnPopup := OnPopup;
    OnPopup := InternalOnPopup;
    FPopupHooked := True;
  end;
end;

procedure TComboBoxHelper.InternalOnPopup(Sender: TObject);
begin
  // Style dropdown items after they exist
  StyleDropdownItems;

  // Call original OnPopup if any
  if Assigned(FOriginalOnPopup) then
    FOriginalOnPopup(Sender);
end;

procedure TComboBoxHelper.StyleListItem(AItem: TListBoxItem);
begin
  if Assigned(AItem) then
  begin
    AItem.StyledSettings := [];
    AItem.Font.Size := FAppliedFontSize - 2; // slightly smaller than displayed text
    AItem.FontColor := FAppliedFontColor;
    // Remove override settings
    AItem.StyledSettings := AItem.StyledSettings - [TStyledSetting.Size, TStyledSetting.FontColor];
  end;
end;

procedure TComboBoxHelper.StyleDropdownItems;
var
  i: Integer;
begin
  if Assigned(ListBox) then
    for i := 0 to ListBox.Items.Count - 1 do
      StyleListItem(ListBox.ListItems[i]);
end;

procedure TComboBoxHelper.StyleDisplayedText;
var
  Txt: TFmxObject;
begin
  Txt := FindStyleResource('content');
  if Txt = nil then Txt := FindStyleResource('text');

  if Assigned(Txt) and (Txt is TText) then
  begin
    TText(Txt).Font.Size := FAppliedFontSize;
    TText(Txt).TextSettings.FontColor := FAppliedFontColor;
  end;
end;

procedure TComboBoxHelper.ApplyFontStyle(const ASize: Single; const AColour: TAlphaColor);
begin
  FAppliedFontSize := ASize;
  FAppliedFontColor := AColour;

  // Force FMX style
  ForceFMXStyle;

  // Hook OnPopup to style dropdown items automatically
  HookOnPopup;

  // Style the displayed text immediately
  StyleDisplayedText;
end;

end.

