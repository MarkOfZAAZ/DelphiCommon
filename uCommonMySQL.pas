unit uCommonMySQL;

interface

uses
  System.SysUtils, System.Classes, System.Types,
  Data.DB, DBAccess, MyAccess, MemDS,
  uCommonDialogs;

  function CheckRecordExistsMySQL(const AConnection: TMyConnection; const ATable, AColumn: string; const AValue: Variant): Boolean;

implementation

function CheckRecordExistsMySQL(const AConnection: TMyConnection; const ATable, AColumn: string; const AValue: Variant): Boolean;
var
  Query: TMyQuery;
begin
  Result := False;
  Query := TMyQuery.Create(nil);
  try
    Query.Connection := AConnection;
    Query.ReadOnly := true;
    Query.UniDirectional := true;
    Query.FetchAll := true;
    try
      Query.SQL.Text := Format('SELECT EXISTS(SELECT 1 FROM %s WHERE %s = :val) AS recfound', [ATable, AColumn]);
      Query.ParamCheck := True;
      Query.Params.ParamByName('val').Value := AValue;
      Query.Open;
      Result := not Query.FieldByName('recfound').AsBoolean;
    except
      on E: Exception do
        ShowException('CheckRecordExistsMySQL', E);
    end;
  finally
    Query.Close;
    Query.Free;
  end;
end;



end.

