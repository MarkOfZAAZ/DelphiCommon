// -----------------------------------------------------------------------------
// Last changed: 23.10.2025
// -----------------------------------------------------------------------------

unit uCommonDialogs;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Notification,
  {$IF Defined(Android)}
    Androidapi.Jni.JavaTypes, Androidapi.Jni.Widget, Androidapi.Jni.App,
    Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers, Posix.Unistd,
  {$ENDIF}

  FMX.Controls, FMX.TabControl, FMX.DialogService, FMX.DialogService.Async, FMX.Types;

  { normal dialog style messages }
  procedure ShowError(const AErrMsg: string); overload;
  procedure ShowException(const AFuncName: string; E: Exception); overload;
  procedure ShowWarning(const AMessage: string; AFocus: TControl = nil; ATab: TTabItem = nil);
  procedure ShowInfo(const AMessage: string);
  procedure ShowMsg(const AMessage: string; ADlgType: TMsgDlgType);

  { non blocking Toasts and Notifications (show message without require user interaction) }
  procedure ShowToast(const AMessage: string; const ATitle: string = ''; const ALongDuration: Boolean = False);
  procedure ShowNotification(const AMessage: string; ATitle: string = '');

implementation

{ general functions used by other procedures and functions, not published in interface }

function GetAppName: string;
begin
  Result := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
end;

function FindParentOfClass(Obj: TFmxObject; ParentClass: TClass): TFmxObject;
begin
  Result := nil;
  while Assigned(Obj) do
  begin
    if Obj.ClassType = ParentClass then
      Exit(Obj);
    Obj := Obj.Parent;
  end;
end;

{ normal dialog style messages }

procedure ShowError(const AErrMsg: string);
begin
  ShowMsg(Format('ERROR!%s%s', [sLineBreak, AErrMsg]), TMsgDlgType.mtError);
end;

procedure ShowException(const AFuncName: string; E: Exception);
begin
  ShowMsg(Format('ERROR in %s%s[%s] %s%s%s', [AFuncName, sLineBreak, E.ClassName, sLineBreak, E.Message, sLineBreak]), TMsgDlgType.mtError);
end;

procedure ShowInfo(const AMessage: string);
begin
  ShowMsg(AMessage, TMsgDlgType.mtInformation);
end;

procedure ShowWarning(const AMessage: string; AFocus: TControl = nil; ATab: TTabItem = nil);
begin
  {$IF DEFINED(MSWINDOWS)}
    if Assigned(ATab) then
    begin
      var TabCtrl := FindParentOfClass(ATab, TTabControl) as TTabControl;
      if Assigned(TabCtrl) then
        TabCtrl.ActiveTab := ATab;
    end;
    if Assigned(AFocus) then
      AFocus.SetFocus;
  {$ENDIF}
  ShowMsg(AMessage, TMsgDlgType.mtWarning);
end;

procedure ShowMsg(const AMessage: string; ADlgType: TMsgDlgType);
begin
  TDialogService.MessageDialog(AMessage, ADlgType, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, nil);
end;

{ Toast for android Notifcations for windows }

procedure ShowNotification(const AMessage: string; ATitle: string = '');
{$IF DEFINED(MSWINDOWS)}
var
  NotificationCenter: TNotificationCenter;
  MyNotification: TNotification;
{$ENDIF}
begin
  if AMessage = '' then
    Exit;
  {$IF Defined(MSWINDOWS)}
    NotificationCenter := TNotificationCenter.Create(nil);
    try
      MyNotification := NotificationCenter.CreateNotification;
      try
        MyNotification.Name := GetAppName;
        MyNotification.Title := ATitle;
        MyNotification.EnableSound := false;
        MyNotification.AlertBody := AMessage;
        NotificationCenter.PresentNotification(MyNotification);
      finally
        MyNotification.Free;
      end;
    finally
      NotificationCenter.Free;
    end;
  {$ENDIF}
end;

procedure ShowToast(const AMessage: string; const ATitle: string = ''; const ALongDuration: Boolean = False);
begin
  if AMessage.IsEmpty then
    Exit;
  {$IF Defined(ANDROID)}
    var duration := TJToast.JavaClass.LENGTH_SHORT;
    if ALongDuration then
      duration := TJToast.JavaClass.LENGTH_LONG;
    TThread.Synchronize(nil,
      procedure
      begin
        TJToast.JavaClass.makeText(TAndroidHelper.Context, StrToJCharSequence(AMessage), duration).show;
      end);
  {$ENDIF}
  {$IF Defined(MSWINDOWS)}
    // We call notification as windows would normally have an annoying button in a dialog
    ShowNotification(AMessage, ATitle);
  {$ENDIF}
end;

end.
