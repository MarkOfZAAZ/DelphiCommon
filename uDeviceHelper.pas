// -----------------------------------------------------------------------------
// Copyright © 1994 - 2026 Aldwicks Limited
//
// Last changed: 05.02.2026 13:29
// -----------------------------------------------------------------------------

unit uDeviceHelper;

interface

uses
  System.Classes, System.SysUtils, System.TypInfo,
  FMX.Types,
  {$IFDEF ANDROID}
    Androidapi.JNI.Os,
    Androidapi.Helpers,
    Androidapi.JNI.JavaTypes,
    Androidapi.JNI.GraphicsContentViewText,
    Androidapi.JNIBridge,
  {$ENDIF}
  System.Skia, FMX.Skia  ;

type
  TDeviceHelper = record
  public
    class function IsAndroid: Boolean; static;
    class function IsWindows: Boolean; static;
    class function IsIOS: Boolean; static;

    // Android specific
    class function AndroidSDK: Integer; static;
    class function AndroidRelease: string; static;
    class function AndroidModel: string; static;
    class function AndroidManufacturer: string; static;

    // Convenience
    class function Summary: string; static;

  end;

implementation

{ TDeviceHelper }

class function TDeviceHelper.IsAndroid: Boolean;
begin
  Result := TOSVersion.Platform = TOSVersion.TPlatform.pfAndroid;
end;

class function TDeviceHelper.IsWindows: Boolean;
begin
  Result := TOSVersion.Platform = TOSVersion.TPlatform.pfWindows;
end;

class function TDeviceHelper.IsIOS: Boolean;
begin
  Result := TOSVersion.Platform = TOSVersion.TPlatform.pfiOS;
end;

class function TDeviceHelper.AndroidSDK: Integer;
begin
  {$IFDEF ANDROID}
  Result := TJBuild_VERSION.JavaClass.SDK_INT;
  {$ELSE}
  Result := -1;
  {$ENDIF}
end;

class function TDeviceHelper.AndroidRelease: string;
begin
  {$IFDEF ANDROID}
  Result := JStringToString(TJBuild_VERSION.JavaClass.RELEASE);
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

class function TDeviceHelper.AndroidModel: string;
begin
  {$IFDEF ANDROID}
  Result := JStringToString(TJBuild.JavaClass.MODEL);
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

class function TDeviceHelper.AndroidManufacturer: string;
begin
  {$IFDEF ANDROID}
  Result := JStringToString(TJBuild.JavaClass.MANUFACTURER);
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

class function TDeviceHelper.Summary: string;
begin
  if IsAndroid then
    Result :=
      'Android Device:' + sLineBreak +
      'SDK: ' + AndroidSDK.ToString + sLineBreak +
      'Version: ' + AndroidRelease + sLineBreak +
      'Model: ' + AndroidManufacturer + ' ' + AndroidModel
  else
    Result := 'Platform: ' + GetEnumName(TypeInfo(TOSVersion.TPlatform), Ord(TOSVersion.Platform));
end;

end.
