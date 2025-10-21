unit uCommonMessages;

(*
  ────────────────────────────────────────────────────────────────────────────
  ───── Usage Examples                                                   ─────
  ────────────────────────────────────────────────────────────────────────────

  ───── Blocking (VCL-like) – Great for Windows/macOS ────────────────────────
  case AskQuestionSync('Are you sure you want to delete this record?') of
    ucYes: DeleteRecord;
    ucNo: Exit;
    ucCancel: ShowInfo('Operation cancelled.');
  end;

  ───── Non-blocking (Android-safe) – Perfect for mobile ─────────────────────
  AskQuestionAsync('Save changes before closing?',
    procedure(Choice: TUserChoice)
    begin
      case Choice of
        ucYes: SaveChanges;
        ucNo: CloseWithoutSave;
        ucCancel: ; // Do nothing
      end;
    end);
*)

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Threading, System.SyncObjs,
  FMX.Controls, FMX.TabControl, FMX.DialogService, FMX.Types,
  {$IF Defined(ANDROID)}
    Androidapi.Jni.JavaTypes,
    Androidapi.Jni.Widget,
    Androidapi.Jni.App,
    Androidapi.JNI.GraphicsContentViewText,
    Androidapi.Helpers,
    Posix.Unistd,
  {$ENDIF}
  {$IF Defined(MSWINDOWS)}
    Winapi.Windows,
  {$ENDIF};

// ─── ENUM TYPES ─────────────────────────────────────────────────────────────

type
  TUserChoice = (ucYes, ucNo, ucCancel);

// ─── BASIC MESSAGE TYPES ────────────────────────────────────────────────────

procedure ShowError(const AErrMsg: string); overload;
procedure ShowError(const AFuncName: string; E: Exception); overload;

procedure ShowWarning(const AMessage: string); overload;
procedure ShowWarning(const AMessage: string; AFocus: TControl); overload;
procedure ShowWarning(const AMessage: string; AFocus: TControl; const ATab: TTabItem); overload;

procedure ShowInfo(const AMessage: string);
procedure ShowMsg(const AMessage: string; ADlgType: TMsgDlgType; const ATitle: string = '');
procedure ShowToast(const AMessage: string; const ALongDuration: Boolean = False);

// ─── CONFIRMATION DIALOGS ───────────────────────────────────────────────────

procedure ConfirmAsync(const AMessage: string; const AOnResult: TProc<Boolean>); overload;
function ConfirmSync(const AMessage: string): Boolean; overload;
function ConfirmSync(const ATitle, AMessage: string): Boolean; overload;

// ─── QUESTION DIALOGS (YES / NO / CANCEL) ───────────────────────────────────

procedure AskQuestionAsync(const AMessage: string; const AOnResult: TProc<TUserChoice>); overload;
function AskQuestionSync(const AMessage: string): TUserChoice; overload;
function AskQuestionSync(const ATitle, AMessage: string): TUserChoice; overload;

implementation

// ─── COMMON MESSAGE DISPLAY HELPERS ─────────────────────────────────────────

procedure ShowMsg(const AMessage: string; ADlgType: TMsgDlgType; const ATitle: string = '');
begin
  TDialogService.MessageDialog(
    AMessage,
    ADlgType,
    [TMsgDlgBtn.mbOK],
    TMsgDlgBtn.mbOK,
    0,
    procedure(const AResult: TModalResult)
    begin
      // placeholder callback
    end
  );
end;

// ─── ERROR HANDLERS ─────────────────────────────────────────────────────────

procedure ShowError(const AErrMsg: string);
begin
  ShowMsg(Format('Error!%s%s', [sLineBreak, AErrMsg]), TMsgDlgType.mtError);
end;

procedure ShowError(const AFuncName: string; E: Exception);
begin
  ShowMsg(
    Format('Error in %s:%s%s (%s)%s%s',
      [AFuncName, sLineBreak, E.ClassName, sLineBreak, E.Message, sLineBreak]),
    TMsgDlgType.mtError
  );
end;

// ─── WARNINGS ───────────────────────────────────────────────────────────────

procedure ShowWarning(const AMessage: string);
begin
  ShowMsg(AMessage, TMsgDlgType.mtWarning);
end;

procedure ShowWarning(const AMessage: string; AFocus: TControl);
begin
  {$IF DEFINED(MSWINDOWS)}
  if Assigned(AFocus) then
    AFocus.SetFocus;
  {$ELSE}
  if Assigned(AFocus) then
    AFocus.BringToFront;
  {$ENDIF}
  ShowMsg(AMessage, TMsgDlgType.mtWarning);
end;

procedure ShowWarning(const AMessage: string; AFocus: TControl; const ATab: TTabItem);

  function FindParentOfType(Obj: TFmxObject; ParentClass: TClass): TFmxObject;
  begin
    Result := nil;
    while Assigned(Obj) do
    begin
      if Obj.ClassType = ParentClass then
        Exit(Obj);
      Obj := Obj.Parent;
    end;
  end;

var
  TabCtrl: TTabControl;
begin
  {$IF DEFINED(MSWINDOWS)}
  if Assigned(ATab) then
  begin
    TabCtrl := FindParentOfType(ATab, TTabControl) as TTabControl;
    if Assigned(TabCtrl) then
      TabCtrl.ActiveTab := ATab;
  end;

  if Assigned(AFocus) then
    AFocus.SetFocus;
  {$ENDIF}

  ShowMsg(AMessage, TMsgDlgType.mtWarning);
end;

// ─── INFO ───────────────────────────────────────────────────────────────────

procedure ShowInfo(const AMessage: string);
begin
  ShowMsg(AMessage, TMsgDlgType.mtInformation);
end;

// ─── ANDROID TOAST MESSAGE ──────────────────────────────────────────────────
procedure ShowToast(const AMessage: string; const ALongDuration: Boolean);
begin
  if AMessage = '' then
    Exit;

  {$IF Defined(ANDROID)}
  var Duration := TJToast.JavaClass.LENGTH_SHORT;
  if ALongDuration then
    Duration := TJToast.JavaClass.LENGTH_LONG;

  TThread.Queue(nil,
    procedure
    begin
      TJToast.JavaClass.makeText(
        TAndroidHelper.Context,
        StrToJCharSequence(AMessage),
        Duration
      ).show;
    end);
  {$ELSE}
  ShowInfo(AMessage);
  {$ENDIF}
end;

// ─── CONFIRMATION DIALOGS ───────────────────────────────────────────────────
procedure ConfirmAsync(const AMessage: string; const AOnResult: TProc<Boolean>);
begin
  TDialogService.MessageDialog(
    AMessage,
    TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
    TMsgDlgBtn.mbNo,
    0,
    procedure(const AResult: TModalResult)
    begin
      if Assigned(AOnResult) then
        AOnResult(AResult = mrYes);
    end
  );
end;

function ConfirmSync(const AMessage: string): Boolean;
begin
  Result := ConfirmSync('Confirm', AMessage);
end;

function ConfirmSync(const ATitle, AMessage: string): Boolean;
var
  SyncObj: TObject;
  Response: Boolean;
begin
  Response := False;
  SyncObj := TObject.Create;
  try
    TMonitor.Enter(SyncObj);
    TDialogService.MessageDialog(
      AMessage,
      TMsgDlgType.mtConfirmation,
      [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
      TMsgDlgBtn.mbNo,
      0,
      procedure(const AResult: TModalResult)
      begin
        Response := (AResult = mrYes);
        TMonitor.Enter(SyncObj);
        try
          TMonitor.Pulse(SyncObj);
        finally
          TMonitor.Exit(SyncObj);
        end;
      end
    );
    TMonitor.Wait(SyncObj);
  finally
    TMonitor.Exit(SyncObj);
    SyncObj.Free;
  end;
  Result := Response;
end;

// ─── QUESTION DIALOGS ───────────────────────────────────────────────────────
procedure AskQuestionAsync(const AMessage: string; const AOnResult: TProc<TUserChoice>);
begin
  TDialogService.MessageDialog(
    AMessage,
    TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel],
    TMsgDlgBtn.mbCancel,
    0,
    procedure(const AResult: TModalResult)
    var
      Choice: TUserChoice;
    begin
      case AResult of
        mrYes: Choice := ucYes;
        mrNo: Choice := ucNo;
      else
        Choice := ucCancel;
      end;

      if Assigned(AOnResult) then
        AOnResult(Choice);
    end
  );
end;

function AskQuestionSync(const AMessage: string): TUserChoice;
begin
  Result := AskQuestionSync('Question', AMessage);
end;

function AskQuestionSync(const ATitle, AMessage: string): TUserChoice;
var
  SyncObj: TObject;
  Choice: TUserChoice;
begin
  Choice := ucCancel;
  SyncObj := TObject.Create;
  try
    TMonitor.Enter(SyncObj);
    TDialogService.MessageDialog(
      AMessage,
      TMsgDlgType.mtConfirmation,
      [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel],
      TMsgDlgBtn.mbCancel,
      0,
      procedure(const AResult: TModalResult)
      begin
        case AResult of
          mrYes: Choice := ucYes;
          mrNo: Choice := ucNo;
        else
          Choice := ucCancel;
        end;

        TMonitor.Enter(SyncObj);
        try
          TMonitor.Pulse(SyncObj);
        finally
          TMonitor.Exit(SyncObj);
        end;
      end
    );
    TMonitor.Wait(SyncObj);
  finally
    TMonitor.Exit(SyncObj);
    SyncObj.Free;
  end;

  Result := Choice;
end;

end.

