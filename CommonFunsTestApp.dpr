// -----------------------------------------------------------------------------
// Copyright © 1994 - 2025 Aldwicks Limited
//
// Last changed: 22.10.2025 10:23
// -----------------------------------------------------------------------------

program CommonFunsTestApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit37 in 'Unit37.pas' {Form37},
  uCommonDialogs in 'uCommonDialogs.pas',
  uCommonClassHelpers in 'uCommonClassHelpers.pas',
  uCommonPDF in 'uCommonPDF.pas',
  uCommonSQLLite in 'uCommonSQLLite.pas',
  uCommonMySQL in 'uCommonMySQL.pas',
  uCommonAppversion in 'uCommonAppversion.pas',
  uListViewHelper in 'uListViewHelper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm37, Form37);
  Application.Run;
end.
