// -----------------------------------------------------------------------------
// Last changed: 23.10.2025 18:01
// -----------------------------------------------------------------------------

unit Unit37;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.TabControl,
  FMX.Header, FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView,
  uCommonDialogs, uCommonAppversion, uHelperListView, uHelperTabControl,
  FMX.Edit, FMX.ComboEdit, FMX.Layouts, FMX.ListBox,
  FMX.Objects, uHelperComboBox;

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
    TabItem5: TTabItem;
    ComboEdit1: TComboEdit;
    TabItem6: TTabItem;
    Label1: TLabel;
    ListBox1: TListBox;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Text1: TText;
    Button1: TButton;
    procedure btnLeftSlideClick(Sender: TObject);
    procedure btnRightSlideClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure btnLeftDirectClick(Sender: TObject);
    procedure btnRightDirectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


  procedure StyleComboBox(AComboBox: TComboBox; const ASize: Single; const AColour: TAlphaColor);
  procedure StyleListBox(AListBox: TListBox; const ASize: Single; const AColour: TAlphaColor);
  procedure StyleListItem(AItem: TListBoxItem; const ASize: Single; const AColour: TAlphaColor);

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

procedure TForm37.Button1Click(Sender: TObject);
begin
  ComboBox1.Items.Add('Apple');
  ComboBox1.Items.Add('Banana');
  ComboBox1.Items.Add('Cherry');

  ComboBox1.ApplyFontStyle(22, TAlphaColors.Chartreuse);
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



  ComboBox1.Items.Add('Apple');
  ComboBox1.Items.Add('Banana');
  ComboBox1.Items.Add('Cherry');

    ComboBox1.ApplyFontStyle(24, TAlphaColors.DarkBlue);


//  ComboBox1.StyleText();
end;

procedure StyleComboBox(AComboBox: TComboBox; const ASize: Single; const AColour: TAlphaColor);
begin
  // Routine to change the default font size and color of ALL items of a TComboBox
  for var I := 0 to Pred(AComboBox.Count) do
    StyleListItem(AComboBox.ListItems[I], ASize, AColour);
end;

procedure StyleListBox(AListBox: TListBox; const ASize: Single; const AColour: TAlphaColor);
begin
  // Routine to change the default font size and color of ALL items of a TComboBox
  for var I := 0 to Pred(AListBox.Count) do
    StyleListItem(AListBox.ListItems[I], ASize, AColour);
end;

procedure StyleListItem(AItem: TListBoxItem; const ASize: Single; const AColour: TAlphaColor);
begin
  // Routine to change the default font size and color of individual items of a TListItem
  AItem.StyledSettings := [];
  AItem.Font.Size := ASize;
  AItem.FontColor := AColour;
  AItem.StyledSettings := AItem.StyledSettings - [TStyledSetting.Size, TStyledSetting.FontColor];
end;

end.
