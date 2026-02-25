// -----------------------------------------------------------------------------
// Copyright © 1994 - 2026 Aldwicks Limited
//
// Last changed: 17.02.2026 15:37
// -----------------------------------------------------------------------------

unit uSkSvgHelper;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.UITypes, System.IOUtils,
  FMX.Graphics, FMX.Skia, Skia, Xml.XMLDoc, Xml.XMLIntf;

type
  TSvgSettings = record
    // Intrinsic SVG size
    SourceWidth: Integer;
    SourceHeight: Integer;
    Ratio: Single;

    // Calculated output size
    OutputWidth: Integer;
    OutputHeight: Integer;
    Scale: Single;

    procedure CalculateOutput(const AMaxSize: Integer);
  end;

function ExtractSvgSettings(const Xml: IXMLDocument): TSvgSettings;
function GetSvgSettings(const ASvgSource: string): TSvgSettings;
procedure ExportSvgToPngKeepingAspect(const ASvgSource: string; const AMaxSize: Integer; const AFileName: string);
procedure ExportSvgToPng(const ASvgSource: string; const AOutputFile: string; AWidth, AHeight: Integer);

implementation

function ExtractSvgSettings(const Xml: IXMLDocument): TSvgSettings;
var
  Root: IXMLNode;
  ViewBox: string;
  Parts: TArray<string>;
  W, H: Single;
  function CleanNumber(const S: string): Single;
  var
    Tmp: string;
  begin
    Tmp := StringReplace(S, 'px', '', [rfIgnoreCase]);
    Tmp := StringReplace(Tmp, ',', '.', []);
    Result := StrToFloatDef(Tmp, 0, TFormatSettings.Invariant);
  end;

begin
  Result.SourceWidth := 0;
  Result.SourceHeight := 0;
  Result.Ratio := 0;
  H := 0;
  W := 0;
  Root := Xml.DocumentElement;
  if not Assigned(Root) then
    Exit;
  // Prefer viewBox
  if Root.HasAttribute('viewBox') then
  begin
    ViewBox := Root.Attributes['viewBox'];
    ViewBox := StringReplace(ViewBox, ',', ' ', [rfReplaceAll]);
    Parts := ViewBox.Split([' '], TStringSplitOptions.ExcludeEmpty);
    if Length(Parts) = 4 then
    begin
      W := CleanNumber(Parts[2]);
      H := CleanNumber(Parts[3]);
    end;
  end
  else if Root.HasAttribute('width') and Root.HasAttribute('height') then
  begin
    W := CleanNumber(Root.Attributes['width']);
    H := CleanNumber(Root.Attributes['height']);
  end
  else
    Exit;
  Result.SourceWidth := Round(W);
  Result.SourceHeight := Round(H);
  if H > 0 then
    Result.Ratio := W / H;
end;

function GetSvgSettings(const ASvgSource: string): TSvgSettings;
var
  Xml: IXMLDocument;
begin
  Xml := TXMLDocument.Create(nil);
  Xml.LoadFromXML(ASvgSource);
  Xml.Active := True;
  Result := ExtractSvgSettings(Xml);
end;

procedure ExportSvgToPng(const ASvgSource: string; const AOutputFile: string; AWidth, AHeight: Integer);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create(AWidth, AHeight);
  try
    LBitmap.SkiaDraw(procedure (const ACanvas: IskCanvas)
    var
    LSvgBrush: TSkSvgBrush;
    begin
      LSvgBrush := TSkSvgBrush.Create;
      try
        LSvgBrush.Source := ASvgSource;
        LSvgBrush.Render(ACanvas, RectF(0, 0, AWidth, AHeight), 1);
      finally
        LSvgBrush.Free;
      end;
    end);
    LBitmap.SaveToFile(AOutputFile);
  finally
    LBitmap.Free;
  end;
end;

procedure ExportSvgToPngKeepingAspect(const ASvgSource: string; const AMaxSize: Integer; const AFileName: string);
var
  SvgInfo: TSvgSettings;
begin
  SvgInfo := GetSvgSettings(ASvgSource);
  SvgInfo.CalculateOutput(AMaxSize);
  ExportSvgToPng(ASvgSource, AFileName, SvgInfo.OutputWidth, SvgInfo.OutputHeight);
end;

{ TSvgSettings }

procedure TSvgSettings.CalculateOutput(const AMaxSize: Integer);
begin
  OutputWidth := 0;
  OutputHeight := 0;
  Scale := 0;

  if (SourceWidth = 0) or (SourceHeight = 0) then// -----------------------------------------------------------------------------
// Copyright © 1994 - 2026 Aldwicks Limited
//
// Last changed: 17.02.2026 15:37
// -----------------------------------------------------------------------------

unit uSkSvgHelper;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.UITypes, System.IOUtils,
  FMX.Graphics, FMX.Skia, Skia, Xml.XMLDoc, Xml.XMLIntf;

type
  TSvgSettings = record
    // Intrinsic SVG size
    SourceWidth: Integer;
    SourceHeight: Integer;
    Ratio: Single;

    // Calculated output size
    OutputWidth: Integer;
    OutputHeight: Integer;
    Scale: Single;

    procedure CalculateOutput(const AMaxSize: Integer);
  end;

function ExtractSvgSettings(const Xml: IXMLDocument): TSvgSettings;
function GetSvgSettings(const ASvgSource: string): TSvgSettings;
procedure ExportSvgToPngKeepingAspect(const ASvgSource: string; const AMaxSize: Integer; const AFileName: string);
procedure ExportSvgToPng(const ASvgSource: string; const AOutputFile: string; AWidth, AHeight: Integer);
procedure ExportSvgToPngWhiteBackground(const ASvgSource: string; const AOutputFile: string;
  AWidth, AHeight: Integer; AWhiteBackground: Boolean = False);
procedure ExportSvgToPngBackground(const ASvgSource: string; const AOutputFile: string;
  AWidth, AHeight: Integer; ABackgroundColor: TAlphaColor = $FFFFFFFF);
implementation

function ExtractSvgSettings(const Xml: IXMLDocument): TSvgSettings;
var
  Root: IXMLNode;
  ViewBox: string;
  Parts: TArray<string>;
  W, H: Single;

  function CleanNumber(const S: string): Single;
  var
    Tmp: string;
  begin
    Tmp := StringReplace(S, 'px', '', [rfIgnoreCase]);
    Tmp := StringReplace(Tmp, ',', '.', []);
    Result := StrToFloatDef(Tmp, 0, TFormatSettings.Invariant);
  end;

begin
  Result.SourceWidth := 0;
  Result.SourceHeight := 0;
  Result.Ratio := 0;
  H := 0;
  W := 0;
  Root := Xml.DocumentElement;
  if not Assigned(Root) then
    Exit;
  // Prefer viewBox
  if Root.HasAttribute('viewBox') then
  begin
    ViewBox := Root.Attributes['viewBox'];
    ViewBox := StringReplace(ViewBox, ',', ' ', [rfReplaceAll]);
    Parts := ViewBox.Split([' '], TStringSplitOptions.ExcludeEmpty);
    if Length(Parts) = 4 then
    begin
      W := CleanNumber(Parts[2]);
      H := CleanNumber(Parts[3]);
    end;
  end
  else if Root.HasAttribute('width') and Root.HasAttribute('height') then
  begin
    W := CleanNumber(Root.Attributes['width']);
    H := CleanNumber(Root.Attributes['height']);
  end
  else
    Exit;
  Result.SourceWidth := Round(W);
  Result.SourceHeight := Round(H);
  if H > 0 then
    Result.Ratio := W / H;
end;

function GetSvgSettings(const ASvgSource: string): TSvgSettings;
var
  Xml: IXMLDocument;
begin
  Xml := TXMLDocument.Create(nil);
  Xml.LoadFromXML(ASvgSource);
  Xml.Active := True;
  Result := ExtractSvgSettings(Xml);
end;

procedure ExportSvgToPngWhiteBackground(const ASvgSource: string; const AOutputFile: string;
  AWidth, AHeight: Integer; AWhiteBackground: Boolean = False);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create(AWidth, AHeight);
  try
    LBitmap.SkiaDraw(
      procedure (const ACanvas: ISkCanvas)
      var
        LSvgBrush: TSkSvgBrush;
      begin
        // Optional white background
        if AWhiteBackground then
          ACanvas.Clear($FFFFFFFF);  // White (ARGB)

        LSvgBrush := TSkSvgBrush.Create;
        try
          LSvgBrush.Source := ASvgSource;
          LSvgBrush.Render(ACanvas, RectF(0, 0, AWidth, AHeight), 1);
        finally
          LSvgBrush.Free;
        end;
      end
    );

    LBitmap.SaveToFile(AOutputFile);
  finally
    LBitmap.Free;
  end;
end;

procedure ExportSvgToPng(const ASvgSource: string; const AOutputFile: string; AWidth, AHeight: Integer);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create(AWidth, AHeight);
  try
    LBitmap.SkiaDraw(procedure (const ACanvas: IskCanvas)
    var
    LSvgBrush: TSkSvgBrush;
    begin
      LSvgBrush := TSkSvgBrush.Create;
      try
        LSvgBrush.Source := ASvgSource;
        LSvgBrush.Render(ACanvas, RectF(0, 0, AWidth, AHeight), 1);
      finally
        LSvgBrush.Free;
      end;
    end);
    LBitmap.SaveToFile(AOutputFile);
  finally
    LBitmap.Free;
  end;
end;

procedure ExportSvgToPngKeepingAspect(const ASvgSource: string; const AMaxSize: Integer; const AFileName: string);
var
  SvgInfo: TSvgSettings;
begin
  SvgInfo := GetSvgSettings(ASvgSource);
  SvgInfo.CalculateOutput(AMaxSize);
  ExportSvgToPngBackground(ASvgSource, AFileName, SvgInfo.OutputWidth, SvgInfo.OutputHeight, $FFCCFFCC);
end;

procedure ExportSvgToPngBackground(const ASvgSource: string; const AOutputFile: string;
  AWidth, AHeight: Integer; ABackgroundColor: TAlphaColor = $FFFFFFFF);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create(AWidth, AHeight);
  try
    LBitmap.SkiaDraw(
      procedure (const ACanvas: ISkCanvas)
      var
        LSvgBrush: TSkSvgBrush;
      begin
        // Fill background (defaults to white)
        ACanvas.Clear(ABackgroundColor);

        LSvgBrush := TSkSvgBrush.Create;
        try
          LSvgBrush.Source := ASvgSource;
          LSvgBrush.Render(ACanvas, RectF(0, 0, AWidth, AHeight), 1);
        finally
          LSvgBrush.Free;
        end;
      end
    );

    LBitmap.SaveToFile(AOutputFile);
  finally
    LBitmap.Free;
  end;
end;
{ TSvgSettings }

procedure TSvgSettings.CalculateOutput(const AMaxSize: Integer);
begin
  OutputWidth := 0;
  OutputHeight := 0;
  Scale := 0;

  if (SourceWidth = 0) or (SourceHeight = 0) then
    Exit;

  if SourceWidth >= SourceHeight then
  begin
    Scale := AMaxSize / SourceWidth;
    OutputWidth := AMaxSize;
    OutputHeight := Round(SourceHeight * Scale);
  end
  else
  begin
    Scale := AMaxSize / SourceHeight;
    OutputHeight := AMaxSize;
    OutputWidth := Round(SourceWidth * Scale);
  end;
end;

end.

    Exit;

  if SourceWidth >= SourceHeight then
  begin
    Scale := AMaxSize / SourceWidth;
    OutputWidth := AMaxSize;
    OutputHeight := Round(SourceHeight * Scale);
  end
  else
  begin
    Scale := AMaxSize / SourceHeight;
    OutputHeight := AMaxSize;
    OutputWidth := Round(SourceWidth * Scale);
  end;
end;

end.
