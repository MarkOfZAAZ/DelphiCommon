// -----------------------------------------------------------------------------
// Copyright © 1994 - 2026 Aldwicks Limited - Developed for RAD Studio 13
//
// Last changed: 20.01.2026 14:11
// -----------------------------------------------------------------------------

unit uHelperLastChangedLabel;

(*
  Allows the user to drop a SkLabel component (skia label) onto a form 
  and it is automatically assignmed as a two word label, the first word showing the text Last changed 
  the second word dd/mm/yyyy hh:nn:ss of a TDateTime or a string passed to it.
  It automatically decides which colours to use, based on the parent background colour
  
  ? How to use...

    Drop a plain TSkLabel on your form.
    
    Then whenever required:
  
    SkLabel1.SetLastChanged(Now); // Show now (replace Now with database setting etc)
    SkLabel1.SetLastChanged(JobRec.LastChanged); // Show from a class with a TDateTime component
    SkLabel1.SetLastChangedText('2026-01-19 14:22:01');
  
    If you want to show a clear value the
    SkLabel1.ClearLastChanged; // This will display the Lastchanged -- 
*)

interface

uses
  System.SysUtils,
  System.UITypes,
  FMX.Skia,
  FMX.Types,
  FMX.Controls;

type
  TSkLabelLastChangedHelper = class helper for TSkLabel
  public
    procedure ApplyLastChangedThemeAuto;
    procedure ClearLastChanged;
    procedure SetLastChangedText(const AValue: string); overload;
    procedure SetLastChanged(const ADateTime: TDateTime); overload;
  end;

implementation

{ TSkLabelLastChangedHelper }

procedure TSkLabelLastChangedHelper.ApplyLastChangedThemeAuto;
var
  Word0Color, Word1Color: TAlphaColor;
  Brightness: Single;
  LBackColor: TAlphaColor;

  function GetBrightness(const Color: TAlphaColor): Single;
  var
    R, G, B: Byte;
  begin
    R := TAlphaColorRec(Color).R;
    G := TAlphaColorRec(Color).G;
    B := TAlphaColorRec(Color).B;
    Result := (0.299 * R + 0.587 * G + 0.114 * B) / 255;
  end;

  // Attempt to estimate a background color from parent controls
  function EstimateBackgroundColor: TAlphaColor;
  var
    C: TFmxObject;
  begin
    C := Self.Parent;
    while Assigned(C) do
    begin
      if C is TControl then
      begin
        // Use StyleLookup or other heuristics here if you want
        Exit(TAlphaColors.Black); // fallback to dark (label over dark background)
      end;
      C := C.Parent;
    end;
    Result := TAlphaColors.Black;
  end;

begin
  LBackColor := EstimateBackgroundColor;
  Brightness := GetBrightness(LBackColor);

  if Brightness > 0.7 then
  begin
    // Light background ? dark text
    Word0Color := TAlphaColors.DodgerBlue;
    Word1Color := TAlphaColors.Black;
  end
  else
  begin
    // Dark background ? light text
    Word0Color := TAlphaColors.Paleturquoise;
    Word1Color := TAlphaColors.White;
  end;

  Words.BeginUpdate;
  try
    Words.Clear;
    // Word 0: static label
    with Words.Add do
    begin
      Text := 'Last changed ';
      Font.Size := 14;
      FontColor := Word0Color;
    end;
    // Word 1: date/time
    with Words.Add do
    begin
      Text := '--';
      Font.Size := 16;
      FontColor := Word1Color;
    end;
  finally
    Words.EndUpdate;
  end;
end;

procedure TSkLabelLastChangedHelper.ClearLastChanged;
begin
  ApplyLastChangedThemeAuto;
end;

procedure TSkLabelLastChangedHelper.SetLastChangedText(const AValue: string);
var
  DT: TDateTime;
begin
  if not TryStrToDateTime(AValue, DT) then
  begin
    ClearLastChanged;
    Exit;
  end;
  SetLastChanged(DT);
end;

procedure TSkLabelLastChangedHelper.SetLastChanged(const ADateTime: TDateTime);
begin
  ApplyLastChangedThemeAuto;
  Words[1].Text := FormatDateTime('dd/mm/yyyy hh:nn:ss', ADateTime);
end;

end.
