// -----------------------------------------------------------------------------
// Copyright © 1994 - 2025 Aldwicks Limited
//
// Last changed: 22.10.2025 17:07
// -----------------------------------------------------------------------------

unit Unit37;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.TabControl,
  uCommonClassHelpers, uCommonDialogs;

type
  TForm37 = class(TForm)
    Button1: TButton;
    TCMain: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TCSecondary: TTabControl;
    TabItem3: TTabItem;
    TabItem4: TTabItem;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form37: TForm37;

implementation

{$R *.fmx}

procedure TForm37.Button1Click(Sender: TObject);
begin
  TCMain.TabLeft(TabItem1);
end;

procedure TForm37.Button2Click(Sender: TObject);
begin
  TCMain.TabRight(TabItem2);
end;

procedure TForm37.Button3Click(Sender: TObject);
begin
  ShowToast('hello mates');
end;

end.
