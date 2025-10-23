// -----------------------------------------------------------------------------
// Last changed: 23.10.2025
// -----------------------------------------------------------------------------

unit uCommonAppversion;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Rtti,
  {$IF Defined(MSWINDOWS)}
    Winapi.Windows;
  {$ENDIF}
  {$IF Defined(Android)}
    Androidapi.Jni.JavaTypes, Androidapi.Jni.Widget, Androidapi.Jni.App,
    Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers, Posix.Unistd;
  {$ENDIF}

function GetAppVersion(const UseSymbol: boolean = false): string;

implementation

function GetAppVersion(const UseSymbol: boolean = false): string;
{$IF Defined(MSWINDOWS)}
var
  FileName: string;
  InfoSize, Handle: DWORD;
  Buffer: Pointer;
  VersionInfo: PVSFixedFileInfo;
  VerSize: UINT;
{$ENDIF}
{$IF Defined(Android)}
var
  PackageManager: JPackageManager;
  PackageInfo: JPackageInfo;
{$ENDIF}
 Symbol: string;
begin
  Result := '';
  {$IF Defined(MSWINDOWS)}
    FileName := ParamStr(0); // The path to the running application
    InfoSize := GetFileVersionInfoSize(PChar(FileName), Handle);
    if InfoSize = 0 then
      Exit('Version info not found');
    GetMem(Buffer, InfoSize);
    try
      if UseSymbol then
        Symbol := '[🪟]';
      if GetFileVersionInfo(PChar(FileName), Handle, InfoSize, Buffer) then
        if VerQueryValue(Buffer, '\', Pointer(VersionInfo), VerSize) then
          with VersionInfo^ do
            Result := Format('[V%d.%d.%d.%d] %s', [HiWord(dwFileVersionMS), LoWord(dwFileVersionMS), HiWord(dwFileVersionLS), LoWord(dwFileVersionLS), Symbol]);
    finally
      FreeMem(Buffer);
    end;
  {$ENDIF}
  {$IF Defined(Android)}
    if UseSymbol then
      Symbol := '[🤖]';
    PackageManager := TAndroidHelper.Context.getPackageManager;
    PackageInfo := PackageManager.getPackageInfo(TAndroidHelper.Context.getPackageName, 0);
    Result := Format('[V%s] %s', [JStringToString(PackageInfo.versionName), Symbol]);
  {$ENDIF}
end;

end.
