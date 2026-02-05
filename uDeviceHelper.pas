unit uDeviceHelper;

interface

uses
  System.Classes, System.SysUtils, System.TypInfo, System.types,
  {$IFDEF ANDROID}
    Androidapi.JNI.Os,
    Androidapi.Helpers,
    Androidapi.JNI.JavaTypes,
    Androidapi.JNI.GraphicsContentViewText,
    Androidapi.JNIBridge,
    Androidapi.JNI.Util,
    Androidapi.JNI.App,
  {$ENDIF}
  FMX.Types, FMX.Platform;

type
  TDeviceHelper = record
  private
    class function ScreenDPI: Single; static;
    class function AndroidUsableResolution: TSize; static;
    class function AndroidRealDPI: TPointF; static;
  public
    class function IsAndroid: Boolean; static;
    class function IsWindows: Boolean; static;
    class function IsIOS: Boolean; static;

    // Android specific
    class function AndroidSDK: Integer; static;
    class function AndroidRelease: string; static;
    class function AndroidModel: string; static;
    class function AndroidManufacturer: string; static;

    // Screen info
    class function ScreenSize: TSizeF; static;        // logical size (points)
    class function ScreenScale: Single; static;        // scale factor
    class function ScreenResolution: TSize; static;   // physical pixels

    // Convenience
    class function Summary: string; static;
  end;

implementation

{ TDeviceHelper }
class function TDeviceHelper.ScreenDPI: Single;
begin
  Result := ScreenScale * 96.0;
end;


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

class function TDeviceHelper.ScreenResolution: TSize;
var
  Size: TSizeF;
  Scale: Single;
begin
  Size := ScreenSize;
  Scale := ScreenScale;

  Result.cx := Round(Size.Width * Scale);
  Result.cy := Round(Size.Height * Scale);
end;

class function TDeviceHelper.ScreenScale: Single;
var
  ScreenService: IFMXScreenService;
begin
  Result := 1.0;

  if TPlatformServices.Current.SupportsPlatformService(
       IFMXScreenService, ScreenService) then
  begin
    Result := ScreenService.GetScreenScale;
  end;
end;

class function TDeviceHelper.ScreenSize: TSizeF;
var
  ScreenService: IFMXScreenService;
begin
  Result := TSizeF.Create(0, 0);

  if TPlatformServices.Current.SupportsPlatformService(
       IFMXScreenService, ScreenService) then
  begin
    Result := ScreenService.GetScreenSize;
  end;
end;

class function TDeviceHelper.AndroidUsableResolution: TSize;
{$IFDEF ANDROID}
var
  Insets: JWindowInsets;
begin
  Insets :=
    TAndroidHelper.Activity
      .getWindow
      .getDecorView
      .getRootWindowInsets;

  if Insets <> nil then
  begin
    Result.cx :=
      ScreenResolution.cx -
      Insets.getSystemWindowInsetLeft -
      Insets.getSystemWindowInsetRight;

    Result.cy :=
      ScreenResolution.cy -
      Insets.getSystemWindowInsetTop -
      Insets.getSystemWindowInsetBottom;
  end
  else
    Result := ScreenResolution;
end;
{$ELSE}
begin
  Result := TSize.Create(0, 0);
end;
{$ENDIF}


class function TDeviceHelper.AndroidRealDPI: TPointF;
{$IFDEF ANDROID}
var
  Metrics: JDisplayMetrics;
begin
  Metrics := TJDisplayMetrics.Create;
  TAndroidHelper.Activity
    .getWindowManager
    .getDefaultDisplay
    .getMetrics(Metrics);

  Result := TPointF.Create(Metrics.xdpi, Metrics.ydpi);
end;
{$ELSE}
begin
  Result := TPointF.Zero;
end;
{$ENDIF}


class function TDeviceHelper.Summary: string;
var
  Size: TSizeF;
  Res, Usable: TSize;
  DPI: TPointF;
begin
  Size := ScreenSize;
  Res := ScreenResolution;

  if IsAndroid then
  begin
    Usable := AndroidUsableResolution;
    DPI := AndroidRealDPI;

    Result :=
      'Android Device:' + sLineBreak +
      'SDK: ' + AndroidSDK.ToString + sLineBreak +
      'Version: ' + AndroidRelease + sLineBreak +
      'Model: ' + AndroidManufacturer + ' ' + AndroidModel + sLineBreak +
      'Screen (dp): ' +
        Format('%.0f x %.0f', [Size.Width, Size.Height]) + sLineBreak +
      'Scale: ' + ScreenScale.ToString + sLineBreak +
      'FMX DPI: ' + ScreenDPI.ToString + sLineBreak +
      'Real DPI: ' +
        Format('%.0f x %.0f', [DPI.X, DPI.Y]) + sLineBreak +
      'Resolution (px): ' +
        Format('%d x %d', [Res.cx, Res.cy]) + sLineBreak +
      'Usable (px): ' +
        Format('%d x %d', [Usable.cx, Usable.cy]);
  end
  else
    Result :=
      'Platform: ' +
      GetEnumName(TypeInfo(TOSVersion.TPlatform),
                  Ord(TOSVersion.Platform)) + sLineBreak +
      'Screen (dp): ' +  Format('%.0f x %.0f', [Size.Width, Size.Height]) + sLineBreak +
      'Scale: ' + ScreenScale.ToString + sLineBreak +
      'Resolution (px): ' + Format('%d x %d', [Res.cx, Res.cy]) + sLineBreak +
      'DPI: ' + ScreenDPI.ToString;

end;

end.
