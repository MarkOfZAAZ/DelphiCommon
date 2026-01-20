// -----------------------------------------------------------------------------
// Copyright © 1994 - 2026 Aldwicks Limited - Developed for RAD Studio 13
//
// Last changed: 20.01.2026 17:24
// -----------------------------------------------------------------------------

unit uHelperSkiaLabel;

(*
   ✅ Key Features
      Fully auto-initializing: EnsureTwoWordLayout creates Words[0] and Words[1] if they don’t exist.
      Dark theme default: no brightness checks or parent color estimation.
      Overloaded SetLastChanged: works with both TDateTime and string.
      Header layout handled automatically.
      Clean API — you never touch ApplyTwoWordTheme unless you want a custom theme manually.

   👉 How to use DARK (Default)...
      // Last changed
      SkLabel1.SetLastChanged(Now);
      SkLabel1.SetLastChanged('2026-01-20 14:00:00');

      // Header
      SkLabel1.SetHeaderText('EDIT CUSTOMER', 'SCOTT BLACKSMITHS');

   👉 How to use LIGHT
      // Last changed
      SkLabel1.SetLastChanged(Now, true);
      SkLabel1.SetLastChanged('2026-01-20 14:00:00', true);

      // Header
      SkLabel1.SetHeaderText('EDIT CUSTOMER', 'SCOTT BLACKSMITHS', true);

   👉 How to clear
      SkLabel1.ClearLastChanged;
      SkLabel1.ClearHeader;
*)

interface

uses
  System.SysUtils, System.UITypes, System.Classes, FMX.Skia;

type
  TSkLabelHelper = class helper for TSkLabel
  private
    procedure EnsureTwoWordLayout(Word0FontSize, Word1FontSize: Single;
      UseLightTheme: Boolean = False);
    procedure ApplyTwoWordTheme(const Word0Text, Word1Text: string;
      Word0FontSize, Word1FontSize: Single; UseLightTheme: Boolean = False);
  public
    // LastChanged helpers
    procedure ClearLastChanged;
    procedure SetLastChanged(const ADateTime: TDateTime; UseLightTheme: Boolean = False); overload;
    procedure SetLastChanged(const AValue: string; UseLightTheme: Boolean = False); overload;

    // Header helpers
    procedure ClearHeader;
    procedure SetHeaderText(const AMain, ASubhead: string; UseLightTheme: Boolean = False);
  end;

implementation

{ TSkLabelHelper }

procedure TSkLabelHelper.EnsureTwoWordLayout(Word0FontSize, Word1FontSize: Single;
  UseLightTheme: Boolean);
var
  Word0Color, Word1Color: TAlphaColor;
begin
  if UseLightTheme then
  begin
    Word0Color := TAlphaColors.DodgerBlue;
    Word1Color := TAlphaColors.Black;
  end
  else
  begin
    Word0Color := TAlphaColors.Paleturquoise;
    Word1Color := TAlphaColors.White;
  end;

  Words.BeginUpdate;
  try
    // Ensure at least two words exist
    while Words.Count < 2 do
      Words.Add;

    // Always enforce font sizes and colors
    Words[0].Font.Size := Word0FontSize;
    Words[0].FontColor := Word0Color;

    Words[1].Font.Size := Word1FontSize;
    Words[1].FontColor := Word1Color;
  finally
    Words.EndUpdate;
  end;
end;

procedure TSkLabelHelper.ApplyTwoWordTheme(const Word0Text, Word1Text: string;
  Word0FontSize, Word1FontSize: Single; UseLightTheme: Boolean);
begin
  EnsureTwoWordLayout(Word0FontSize, Word1FontSize, UseLightTheme);

  Words.BeginUpdate;
  try
    Words[0].Text := Word0Text;
    Words[1].Text := Word1Text;
  finally
    Words.EndUpdate;
  end;
end;

procedure TSkLabelHelper.ClearLastChanged;
begin
  ApplyTwoWordTheme('Last changed ', '--', 14, 16);
end;

procedure TSkLabelHelper.SetLastChanged(const ADateTime: TDateTime; UseLightTheme: Boolean);
begin
  ApplyTwoWordTheme('Last changed ', FormatDateTime('dd/mm/yyyy hh:nn:ss', ADateTime), 14, 16, UseLightTheme);
end;

procedure TSkLabelHelper.SetLastChanged(const AValue: string; UseLightTheme: Boolean);
var
  DT: TDateTime;
begin
  if not TryStrToDateTime(AValue, DT) then
    ClearLastChanged
  else
    SetLastChanged(DT, UseLightTheme);
end;

procedure TSkLabelHelper.ClearHeader;
begin
  ApplyTwoWordTheme(' ', ' ', 16, 18);
end;

procedure TSkLabelHelper.SetHeaderText(const AMain, ASubhead: string; UseLightTheme: Boolean);
begin
  ApplyTwoWordTheme(AMain, ASubhead, 16, 18, UseLightTheme);
end;

end.