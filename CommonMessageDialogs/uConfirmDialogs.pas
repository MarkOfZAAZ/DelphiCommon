unit uConfirmDialogs;

(*
  ────────────────────────────────────────────────────────────────────────────
  ───── Usage Examples                                                   ─────
  ────────────────────────────────────────────────────────────────────────────

  ───── Blocking (VCL-like) – Great for Windows/macOS ────────────────────────
  procedure TForm1.BtnDeleteClick(Sender: TObject);
  begin
    if TDialogHelper.ConfirmDelete('Patient: John Smith') = ucYes then
      ShowMessage('Record deleted.');
  end;

  ───── Non-blocking (Android-safe) – Perfect for mobile ─────────────────────
  procedure TForm1.BtnDeleteClick(Sender: TObject);
  begin
    TDialogHelper.ConfirmDeleteAsync('Patient: John Smith',
      procedure(Choice: TUserChoice)
      begin
        if Choice = ucYes then
          ShowMessage('Record deleted.')
        else
          ShowMessage('Cancelled.');
      end);
  end;
*)

interface

uses
  System.SysUtils, System.Classes, System.Threading, System.SyncObjs,
  FMX.DialogService, FMX.Dialogs;

type
  TUserChoice = (ucNone, ucYes, ucNo, ucCancel);

  TDialogHelper = class
  public
    // ───── Synchronous (blocking) versions ──────────────────────────────────
    class function ConfirmCustom(const ATitle, AMessage: string;
      const AYes, ANo, ACancel: string): TUserChoice;

    class function ConfirmDelete(const RecordName: string = ''): TUserChoice;
    class function ConfirmSave(const ItemName: string = ''): TUserChoice;
    class function ConfirmOverwrite(const FileName: string = ''): TUserChoice;
    class function ConfirmResetPassword(const UserName: string = ''): TUserChoice;
    class function ConfirmLogout(const UserName: string = ''): TUserChoice;
    class function ConfirmExitApp: TUserChoice;

    // ───── Asynchronous (non-blocking) versions ─────────────────────────────
    class procedure ConfirmDeleteAsync(const RecordName: string;
      const ACallback: TProc<TUserChoice>);
    class procedure ConfirmSaveAsync(const ItemName: string;
      const ACallback: TProc<TUserChoice>);
    class procedure ConfirmCustomAsync(const ATitle, AMessage: string;
      const ACallback: TProc<TUserChoice>);
  end;

implementation

// ───── Blocking (VCL-style) versions ────────────────────────────────────────
class function TDialogHelper.ConfirmCustom(const ATitle, AMessage: string;
  const AYes, ANo, ACancel: string): TUserChoice;
begin
  Result := TTask.Future<TUserChoice>(
    function: TUserChoice
    var
      LChoice: TUserChoice;
      LEvent: TEvent;
    begin
      LChoice := ucNone;
      LEvent := TEvent.Create;
      try
        TDialogService.MessageDialog(
          AMessage,
          TMsgDlgType.mtConfirmation,
          [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel],
          TMsgDlgBtn.mbCancel, 0,
          procedure(const AResult: TModalResult)
          begin
            case AResult of
              mrYes:    LChoice := ucYes;
              mrNo:     LChoice := ucNo;
              mrCancel: LChoice := ucCancel;
            end;
            LEvent.SetEvent;
          end);
        // Wait until the user responds
        LEvent.WaitFor(INFINITE);
        Result := LChoice;
      finally
        LEvent.Free;
      end;
    end).Value;
end;

class function TDialogHelper.ConfirmDelete(const RecordName: string): TUserChoice;
var
  Msg: string;
begin
  if RecordName.Trim <> '' then
    Msg := Format('Are you sure you want to delete the record "%s"?', [RecordName])
  else
    Msg := 'Are you sure you want to delete this record?';
  Result := ConfirmCustom('Confirm Delete', Msg, 'Yes', 'No', 'Cancel');
end;

class function TDialogHelper.ConfirmSave(const ItemName: string): TUserChoice;
var
  Msg: string;
begin
  if ItemName.Trim <> '' then
    Msg := Format('Do you want to save changes to "%s"?', [ItemName])
  else
    Msg := 'Do you want to save your changes?';
  Result := ConfirmCustom('Confirm Save', Msg, 'Yes', 'No', 'Cancel');
end;

class function TDialogHelper.ConfirmOverwrite(const FileName: string): TUserChoice;
var
  Msg: string;
begin
  if FileName.Trim <> '' then
    Msg := Format('File "%s" already exists. Do you want to overwrite it?', [FileName])
  else
    Msg := 'This file already exists. Do you want to overwrite it?';
  Result := ConfirmCustom('Confirm Overwrite', Msg, 'Yes', 'No', 'Cancel');
end;

class function TDialogHelper.ConfirmResetPassword(const UserName: string): TUserChoice;
var
  Msg: string;
begin
  if UserName.Trim <> '' then
    Msg := Format('Are you sure you want to reset the password for "%s"?', [UserName])
  else
    Msg := 'Are you sure you want to reset the password?';
  Result := ConfirmCustom('Reset Password', Msg, 'Yes', 'No', 'Cancel');
end;

class function TDialogHelper.ConfirmLogout(const UserName: string): TUserChoice;
var
  Msg: string;
begin
  if UserName.Trim <> '' then
    Msg := Format('Are you sure you want to log out "%s"?', [UserName])
  else
    Msg := 'Are you sure you want to log out?';
  Result := ConfirmCustom('Confirm Logout', Msg, 'Yes', 'No', 'Cancel');
end;

class function TDialogHelper.ConfirmExitApp: TUserChoice;
begin
  Result := ConfirmCustom('Exit Application',
    'Are you sure you want to exit the application?',
    'Yes', 'No', 'Cancel');
end;

// ───── Non-blocking (Android-safe) versions ─────────────────────────────────
class procedure TDialogHelper.ConfirmCustomAsync(const ATitle, AMessage: string;
  const ACallback: TProc<TUserChoice>);
begin
  TDialogService.MessageDialog(
    AMessage,
    TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel],
    TMsgDlgBtn.mbCancel, 0,
    procedure(const AResult: TModalResult)
    var
      Choice: TUserChoice;
    begin
      case AResult of
        mrYes: Choice := ucYes;
        mrNo: Choice := ucNo;
        mrCancel: Choice := ucCancel;
      else
        Choice := ucNone;
      end;
      if Assigned(ACallback) then
        ACallback(Choice);
    end);
end;

class procedure TDialogHelper.ConfirmDeleteAsync(const RecordName: string;
  const ACallback: TProc<TUserChoice>);
var
  Msg: string;
begin
  if RecordName.Trim <> '' then
    Msg := Format('Are you sure you want to delete the record "%s"?', [RecordName])
  else
    Msg := 'Are you sure you want to delete this record?';
  ConfirmCustomAsync('Confirm Delete', Msg, ACallback);
end;

class procedure TDialogHelper.ConfirmSaveAsync(const ItemName: string;
  const ACallback: TProc<TUserChoice>);
var
  Msg: string;
begin
  if ItemName.Trim <> '' then
    Msg := Format('Do you want to save changes to "%s"?', [ItemName])
  else
    Msg := 'Do you want to save your changes?';
  ConfirmCustomAsync('Confirm Save', Msg, ACallback);
end;

end.

