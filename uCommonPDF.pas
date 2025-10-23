// -----------------------------------------------------------------------------
// Last changed: 22.10.2025
// -----------------------------------------------------------------------------

(*
    This unit is designed to launch a PDF file for viewing
    Each target processing PDF vieing in different ways
*)

unit uCommonPDF;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  {$IFDEF Android}
    Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Embarcadero, Androidapi.JNI.Net,
    Androidapi.JNI.JavaTypes, Androidapi.JNI.Support, Androidapi.JNI.App, Androidapi.JNI.Os,
    Androidapi.JNIBridge, Androidapi.JNI.Provider, Androidapi.Helpers, FMX.Platform.Android,
    FMX.Helpers.Android, Posix.Unistd;
  {$ENDIF}
  {$IF Defined(IOS)}
    iOSapi.Foundation, iOSapi.UIKit, Macapi.Helpers, Posix.Unistd;
  {$ENDIF}
  {$IF Defined(MACOS)}
    Macapi.Foundation, Macapi.AppKit, Macapi.Helpers, Posix.Unistd;
  {$ENDIF}
  {$IF Defined(MSWINDOWS)}
    Winapi.ShellAPI, Winapi.Windows;
  {$ENDIF}

  function PDFRebuildRequired(ADatetime: TDateTime; AFilename: string): Boolean;
  procedure LaunchPDF(const AFilename: string);

implementation

uses
  System.IOUtils;

{ Basically, can check if a PDF requires a rebuild }

function PDFRebuildRequired(ADatetime: TDateTime; AFilename: string): Boolean;
var
  FileDateTime: TDateTime;
begin
  if AFilename.IsEmpty or (not FileExists(AFilename)) then
    Exit(True); // requires a rebuild as it doesn't exist!
  FileDateTime := TFile.GetLastWriteTime(AFilename); // Get the last modified datetime of the file
  Result := FileDateTime < ADateTime; // sets true or false
end;

{ Actually launch the PDF viewer using the approraite target device }
// Requires a FULLY QUALIFIED file path for the filename!
procedure LaunchPDF(const AFilename: string);
{$IF Defined(ANDROID)}
var
  Intent: JIntent;
  FileURI: Jnet_Uri;
  PDFFilePath: string;
{$ENDIF}
{$IF Defined(MACOS)}
var
  URL: NSURL;
{$ENDIF}
{$IF Defined(IOS)}
var
  URL: NSURL;
{$ENDIF}

begin
  if AFilename.IsEmpty or (not FileExists(AFilename)) then
    Exit;
  {$IF Defined(MSWINDOWS)}
    ShellExecute(0, nil, PChar(AFilename), nil,  nil, SW_SHOWNORMAL);
  {$ENDIF}
  {$IF Defined(ANDROID)}
    PDFFilePath := TPath.Combine(TPath.GetDocumentsPath, AFilename);
    Intent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_VIEW);
    FileURI := TAndroidHelper.JFileToJURI(TJFile.JavaClass.init(StringToJString(PDFFilePath)));
    Intent.setDataAndType(FileURI, StringToJString('application/pdf'));
    Intent.setFlags(TJIntent.JavaClass.FLAG_ACTIVITY_NO_HISTORY or TJIntent.JavaClass.FLAG_GRANT_READ_URI_PERMISSION);
    TAndroidHelper.Activity.startActivity(Intent);
  {$ENDIF}
  {$IF Defined(MACOS)}
    URL := TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(NSStr(TPath.Combine(TPath.GetDocumentsPath, AFilename))));
    NSWorkspace.sharedWorkspace.openURL(URL);
  {$ENDIF}
  {$IF Defined(IOS)}
    URL := TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(NSStr(TPath.Combine(TPath.GetDocumentsPath, AFilename))));
    SharedApplication.openURL(URL);
  {$ENDIF}
end;

end.

