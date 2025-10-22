// -----------------------------------------------------------------------------
// Copyright © 1994 - 2025 Aldwicks Limited
//
// Last changed: 22.10.2025 17:29
// -----------------------------------------------------------------------------

unit uCommonDBFuncs;

interface

uses
  System.SysUtils, System.Classes, System.Types, FireDAC.Comp.Client, FireDAC.Stan.Param, uCommonDialogs;


  function IsValueUnique(const AConnection: TFDConnection; const ATable, AColumn: string; const AValue: Variant): Boolean;


implementation

function IsValueUnique(const AConnection: TFDConnection; const ATable, AColumn: string; const AValue: Variant): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := AConnection;
    Query.ResourceOptions.ParamCreate := True;
    Query.Connection := AConnection;
    try
      Query.SQL.Text := Format('SELECT EXISTS(SELECT 1 FROM %s WHERE %s = :val) AS isTaken', [ATable, AColumn]);
      Query.ParamByName('val').Value := AValue;
      Query.Open;
      Result := not Query.FieldByName('isTaken').AsBoolean;
    except
      on E: Exception do
        ShowException('IsValueUnique', E);
    end;
  finally
    Query.Close;
    Query.Free;
  end;
end;

end.
