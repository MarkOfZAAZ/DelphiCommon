// -----------------------------------------------------------------------------
// Last changed: 23.10.2025
// -----------------------------------------------------------------------------
unit uCommonSQLLite;

{ TBitmapHelper }

    (*
      Example usage:
      Image1.Bitmap.FetchImageFromBlobField(Query.FieldByName('image_blob'));
    *)


interface

uses
  System.SysUtils, System.Classes, System.Types, FireDAC.Comp.Client, FireDAC.Stan.Param,  FMX.Graphics, Data.DB, uCommonDialogs;

type
  TBitmapHelper = class helper for TBitmap
  public
    procedure FetchImageFromBlobField(AField: TField);
  end;

  function CheckRecordExists(const AConnection: TFDConnection; const ATable, AColumn: string; const AValue: Variant): Boolean;

implementation

function CheckRecordExists(const AConnection: TFDConnection; const ATable, AColumn: string; const AValue: Variant): Boolean;
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
      Query.SQL.Text := Format('SELECT EXISTS(SELECT 1 FROM %s WHERE %s = :val) AS recfound', [ATable, AColumn]);
      Query.ParamByName('val').Value := AValue;
      Query.Open;
      Result := not Query.FieldByName('recfound').AsBoolean;
    except
      on E: Exception do
        ShowException('CheckRecordExists', E);
    end;
  finally
    Query.Close;
    Query.Free;
  end;
end;

{ TBitmapHelper }

    (*
      Example usage:
      Image1.Bitmap.FetchImageFromBlobField(Query.FieldByName('image_blob'));
    *)

procedure TBitmapHelper.FetchImageFromBlobField(AField: TField);
var
  Stream: TMemoryStream;
begin
  if Assigned(AField) and (AField is TBlobField) and not AField.IsNull then
  begin
    Stream := TMemoryStream.Create;
    try
      TBlobField(AField).SaveToStream(Stream);
      Stream.Position := 0;
      Self.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end;
end;

end.
