// -----------------------------------------------------------------------------
// Copyright © 1994 - 2026 Aldwicks Limited - Developed for RAD Studio 13
//
// Last changed: 20.01.2026 14:11
// -----------------------------------------------------------------------------

unit uHelperLastChangedLabel;

(*
  uHelperLastChangedLabel allows the user to drop a SkLabel component (skia label) onto a form 
  and it is automatically assignmed as a two word label, the first word showing the text Last changed 
  the second word dd/mm/yyyy hh:nn:ss of a TDateTime or a string passed to it.
  
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
  FMX.Skia;

type
  TSkLabelLastChangedHelper = class helper for TSkLabel
  public
    procedure ClearLastChanged;
    procedure EnsureLastChangedLayout;
    procedure SetLastChangedText(const AValue: string); overload;
    procedure SetLastChanged(const ADateTime: TDateTime); overload;
  end;

implementation

{ TSkLabelLastChangedHelper }
procedure TSkLabelLastChangedHelper.ClearLastChanged;
begin
  EnsureLastChangedLayout;
  Words[1].Text := '--';
end;

procedure TSkLabelLastChangedHelper.EnsureLastChangedLayout;
begin
  if Words.Count >= 2 then
    Exit;
  Words.BeginUpdate;
  try
    Words.Clear;
    // Word 0: static label
    with Words.Add do
    begin
      Text := 'Last changed ';
      Font.Size := 14;
      FontColor := TAlphaColors.Paleturquoise;
    end;
    // Word 1: date/time
    with Words.Add do
    begin
      Text := '--';
      Font.Size := 16;
      FontColor := TAlphaColors.White;
    end;
  finally
    Words.EndUpdate;
  end;
end;

procedure TSkLabelLastChangedHelper.SetLastChangedText(const AValue: string);
var
  DT: TDateTime;
begin
  if not TryStrToDateTime(AValue, DT) then
    Exit;
  SetLastChanged(DT);
end;

procedure TSkLabelLastChangedHelper.SetLastChanged(const ADateTime: TDateTime);
begin
  EnsureLastChangedLayout;
  Words[1].Text := FormatDateTime('dd/mm/yyyy hh:nn:ss', ADateTime);
end;

end.

