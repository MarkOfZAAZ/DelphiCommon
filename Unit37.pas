// -----------------------------------------------------------------------------
// Last changed: 23.10.2025 18:01
// -----------------------------------------------------------------------------

unit Unit37;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.TabControl,
  FMX.Header, FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView,
  uCommonDialogs, uCommonAppversion, uHelperListView, uHelperTabControl;

type
  TForm37 = class(TForm)
    btnLeftSlide: TButton;
    TCMain: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TCSecondary: TTabControl;
    TabItem3: TTabItem;
    TabItem4: TTabItem;
    btnRightSlide: TButton;
    Button3: TButton;
    btnLeftDirect: TButton;
    btnRightDirect: TButton;
    ListView1: TListView;
    Panel1: TPanel;
    lblVersion: TLabel;
    procedure btnLeftSlideClick(Sender: TObject);
    procedure btnRightSlideClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure btnLeftDirectClick(Sender: TObject);
    procedure btnRightDirectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form37: TForm37;

implementation

{$R *.fmx}

procedure TForm37.btnLeftDirectClick(Sender: TObject);
begin
  TCMain.TabLeft(TabItem1, false);
end;

procedure TForm37.btnLeftSlideClick(Sender: TObject);
begin
  TCMain.TabLeft(TabItem1);
end;

procedure TForm37.btnRightDirectClick(Sender: TObject);
begin
  TCMain.TabRight(TabItem2, false);
end;

procedure TForm37.btnRightSlideClick(Sender: TObject);
begin
  TCMain.TabRight(TabItem2);
end;

procedure TForm37.Button3Click(Sender: TObject);
begin
  ShowToast('hello mates');
end;

procedure TForm37.FormCreate(Sender: TObject);
begin
  ListView1.SetSearchDefault;
  ListView1.SetSearchFontColor(TAlphaColors.Red);
  lblVersion.Text := Format('Version %s', [GetAppVersion(true)]);
end;

end.
