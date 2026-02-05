// -----------------------------------------------------------------------------
// Copyright © 1994 - 2026 Aldwicks Limited
//
// Last changed: 05.02.2026 13:48
// -----------------------------------------------------------------------------

unit uSkSvgHelper;

interface

uses
  System.SysUtils, System.Classes,  System.IOUtils, FMX.Skia;

type
  TSkSVGHelper = class helper for TSkSVG
  public
    procedure LoadFromFile(const AFileName: string);
  end;

implementation

procedure TSkSVGHelper.LoadFromFile(const AFileName: string);
var
  WorkFile: string;
  SL: TStringList;
begin
  {$IFDEF ANDROID}
    WorkFile := TPath.Combine(TPath.GetHomePath, AFileName);
  {$ELSE}
    WorkFile := TPath.Combine(ExtractFilePath(ParamStr(0)), AFileName);
  {$ENDIF}

  SL := TStringList.Create;
  try
    SL.LoadFromFile(WorkFile, TEncoding.UTF8);
    Self.SVG.Source := SL.Text;
  finally
    SL.Free;
  end;
end;

end.
