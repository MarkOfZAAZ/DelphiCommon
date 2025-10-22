unit uCommonAppversion;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Rtti,
  {$IF Defined(MSWINDOWS)}
    Winapi.Windows
  {$ENDIF}
;

function GetAppVersion: string;

implementation

function GetAppVersion: string;
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
begin
  Result := '';
  {$IF Defined(MSWINDOWS)}
    FileName := ParamStr(0); // The path to the running application
    InfoSize := GetFileVersionInfoSize(PChar(FileName), Handle);
    if InfoSize = 0 then
      Exit('Version info not found');
    GetMem(Buffer, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Handle, InfoSize, Buffer) then
        if VerQueryValue(Buffer, '\', Pointer(VersionInfo), VerSize) then
          with VersionInfo^ do
            Result := Format('[V%d.%d.%d.%d]', [
              HiWord(dwFileVersionMS), LoWord(dwFileVersionMS),
              HiWord(dwFileVersionLS), LoWord(dwFileVersionLS)]);
    finally
      FreeMem(Buffer);
    end;
  {$ENDIF}
  {$IF Defined(Android)}
    PackageManager := TAndroidHelper.Context.getPackageManager;
    PackageInfo := PackageManager.getPackageInfo(TAndroidHelper.Context.getPackageName, 0);
    Result := Format('[V%s]', [JStringToString(PackageInfo.versionName)]);
  {$ENDIF}
end;

end.
