unit beMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,
  LCLType, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  ShellCtrls, ValEdit,
  bmpcomn, mpHexEditor, ImgList;

type

  { TMainForm }

  TMainForm = class(TForm)
    cbHexAddressMode: TCheckBox;
    cbHexSingleBytes: TCheckBox;
    ColorTableValueList: TValueListEditor;
    ImageList: TImageList;
    Panel2: TPanel;
    StatusBar: TStatusBar;
    SummaryValueList: TValueListEditor;
    InfoHeaderValueList: TValueListEditor;
    Image1: TImage;
    MainPageControl: TPageControl;
    DataPageControl: TPageControl;
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    ShellListView: TShellListView;
    ShellTreeView: TShellTreeView;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    pgImage: TTabSheet;
    pgData: TTabSheet;
    pgFileHeader: TTabSheet;
    FileHeaderValueList: TValueListEditor;
    pgBitmapInfoHeader: TTabSheet;
    pgColorTable: TTabSheet;
    pgSummary: TTabSheet;
    pgHex: TTabSheet;
    procedure cbHexAddressModeChange(Sender: TObject);
    procedure cbHexSingleBytesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImageListGetWidthForPPI(Sender: TCustomImageList; AImageWidth,
      APPI: Integer; var AResultWidth: Integer);
    procedure ShellListViewFileAdded(Sender: TObject; Item: TListItem);
    procedure ShellListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ShellTreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure ShellTreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
  private
    FFileHeader: TBitmapFileHeader;
    FInfoHeader: TBitmapInfoHeader;
    FColorFormat: String;
    FHexEditor: TMPHexEditor;
    procedure HexEditorClick(Sender: TObject);
    procedure LoadColorTable(AStream: TStream);
    procedure LoadFileHeader(AStream: TStream);
    procedure LoadHex(AStream: TStream);
    procedure LoadImage(AStream: TStream);
    procedure LoadInfoHeader(AStream: TStream);
    procedure UpdateStatusbar;
    procedure UpdateSummary;

  public
    procedure LoadFile(AFileName: String);

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  Math;

const
  APP_TITLE = 'BMP Explorer';

  PANEL_OFFSET = 0;
  PANEL_ENDIAN = 1;
  PANEL_MSG = 2;

function GetFixedFontName: String;
var
  idx: Integer;
begin
  Result := Screen.SystemFont.Name;
  idx := Screen.Fonts.IndexOf('Courier New');
  if idx = -1 then
    idx := Screen.Fonts.IndexOf('Courier 10 Pitch');
  if idx <> -1 then
    Result := Screen.Fonts[idx]
  else
    for idx := 0 to Screen.Fonts.Count-1 do
      if pos('courier', Lowercase(Screen.Fonts[idx])) = 1 then
      begin
        Result := Screen.Fonts[idx];
        exit;
      end;
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := APP_TITLE;

 {$IFNDEF MSWINDOWS}
  ShellTreeView.Images := ImageList;
  ShellTreeView.OnGetImageIndex := @ShellTreeViewGetImageIndex;
  ShellTreeView.OnGetSelectedIndex := @ShellTreeViewGetSelectedIndex;

  ShellListView.SmallImages := ImageList;
  ShellListView.OnFileAdded := @ShellListViewFileAdded;
 {$ENDIF}

  FHexEditor := TMPHExEditor.Create(self);
  FHexEditor.Parent := pgHex;
  FHexEditor.Align := alClient;
  FHexEditor.Font.Name := GetFixedFontName;   // The hard-coded Courier New does not exist in Linux
  FHexEditor.Font.Size := 9;
  FHexEditor.BytesPerColumn := IfThen(cbHexSingleBytes.Checked, 1, 2);
  FHexEditor.RulerNumberBase := IfThen(cbHexAddressMode.Checked, 16, 10);
  FHexEditor.OffsetFormat := IfThen(cbHexAddressMode.Checked, '-!10:$|', '-!0A: |');
  FHexEditor.ReadOnlyView := true;
  FHexEditor.OnClick := @HexEditorClick;

  MainPageControl.ActivePageIndex := 0;
  DataPageControl.ActivePageIndex := 0;

  if ParamCount > 0 then
  begin
    ShellTreeView.Path := ParamStr(1);
  end;
end;

procedure TMainForm.cbHexAddressModeChange(Sender: TObject);
begin
  FHexEditor.RulerNumberBase := IfThen(cbHexAddressMode.Checked, 16, 10);
  FHexEditor.OffsetFormat := IfThen(cbHexAddressMode.Checked, '-!10:$|', '-!0A: |');
end;

procedure TMainForm.cbHexSingleBytesChange(Sender: TObject);
begin
  FHexEditor.BytesPerColumn := IfThen(cbHexSingleBytes.Checked, 1, 2);
end;

procedure TMainForm.HexEditorClick(Sender: TObject);
begin
  UpdateStatusbar;
end;

procedure TMainForm.ImageListGetWidthForPPI(Sender: TCustomImageList;
  AImageWidth, APPI: Integer; var AResultWidth: Integer);
begin
  AResultWidth := AImageWidth * APPI div 96;
end;

procedure TMainForm.LoadColorTable(AStream: TStream);
const
  COLORS: array[0..2] of string = ('red', 'green', 'blue');
var
  i, j, n: Integer;
  rgb: TRGBQuad;
  s: String;
  dw: DWord = 0;
  bits: array[0..2] of byte = (0, 0, 0);
  alpha: string = '';
begin
  FColorFormat := '';

  ColorTableValueList.RowCount := 1;
  case FInfoHeader.Compression of
    BI_RGB:
      if FInfoHeader.BitCount <= 8 then
      begin
        rgb := Default(TRGBQuad);
        pgColorTable.Caption := 'Palette';
        n := FInfoHeader.ClrUsed;
        if n = 0 then n := 2 ** FInfoHeader.BitCount;
        for i := 0 to n-1 do
        begin
          AStream.Read(rgb, SizeOf(TRGBQuad));
          ColorTableValueList.InsertRow('Color #' + i.ToString, Format('%0:d ($%0:.8x)', [dword(rgb)]), true);
        end;
      end else
      begin
        pgColorTable.Caption := 'Palette';
        ColorTableValueList.InsertRow('No palette', '', true);
      end;
    BI_BITFIELDS:
      begin
        pgColorTable.Caption := 'Color Masks';
        for i := 0 to 2 do
        begin
          AStream.Read(dw, SizeOf(dw));
          s := BinStr(dw, 16);
          bits[i] := 0;
          for j := 0 to Length(s) do
            if s[j] = '1' then inc(bits[i]);
          if (i = 0) and (s[1] = '0') then alpha := 'a1';
          ColorTableValueList.InsertRow(COLORS[i], Format('%0:d = $%0:.4x = %%%1:s',
            [ dw, BinStr(dw, 16) ]), true);
        end;
        FColorFormat :=  Format('%sr%dg%db%d', [alpha, bits[0], bits[1], bits[2]]);
        ColorTableValueList.InsertRow('16-bit color mask', FColorFormat, true);
      end;
    else
      ;  // unclear: Is there a color table or not?

  end;

end;

procedure TMainForm.LoadFile(AFileName: String);
var
  stream: TStream;
begin
  if not FileExists(AFileName) then
  begin
    Image1.Picture.Clear;
    FileHeaderValueList.RowCount := 1;
    InfoHeaderValueList.RowCount := 1;
    Caption := APP_TITLE;
    exit;
  end;

  Caption := APP_TITLE + ' - ' + ExpandFileName(AFileName);

  stream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadImage(stream);
    LoadFileHeader(stream);
    LoadInfoHeader(stream);
    LoadColorTable(stream);
    LoadHex(stream);
    UpdateSummary;
  finally
    stream.Free;
  end;
end;

procedure TMainForm.LoadFileHeader(AStream: TStream);
{ bfType:word;          // BM
  bfSize: LongInt;      // File size in bytes
  bfReserved: LongInt;  // Reserved
  bfOffset: LongInt;    // Offsetof image data : size of the file header + the info header + palette
}
var
  signature: String[2] = '';
begin
  FFileHeader := Default(TBitmapFileHeader);
  AStream.Position := 0;
  AStream.Read(FFileHeader, SizeOf(FFileHeader));

  FileHeaderValueList.RowCount := 1;
  with FFileHeader do
  begin
    SetLength(signature, 2);
    Move(bfType, signature[1], 2);
    FileHeaderValueList.InsertRow('Type', Format('%0:s (%1:d = $%1:2x)', [signature, bfType]), true);
    FileHeaderValueList.InsertRow('Size', Format('%.0n bytes', [1.0*bfSize]), true);
    FileHeaderValueList.InsertRow('Reserved', Format('%0:d ($%0:4x)', [bfReserved]), true);
    FileHeaderValueList.InsertRow('Offset to image data', Format('%0:d ($%0:4x)', [bfOffset]), true);
  end;
end;

procedure TMainForm.LoadImage(AStream: TStream);
begin
  AStream.Position := 0;
  Image1.Picture.LoadFromStream(AStream);
  Image1.Width := Image1.Picture.Width;
  Image1.Height := Image1.Picture.Height;
end;

procedure TMainForm.LoadInfoHeader(AStream: TStream);
const
  COMPRESSION_NAME: array[0..3] of String = (
    'RGB - uncompressed',
    'RLE8 - runlength-encoded for 8bpp',
    'RLE4 - runlength-encoded for 4bpp',
    'BIT_FIELDS - uncompressed, coded by color masks'
  );
(*
TBitMapInfoHeader = packed record
{14+04 : Size of the bitmap info header : sould be 40=$28}
   Size:longint;
{18+04 : Image width in pixels}
   Width:longint;
{22+04 : Image height in pixels}
   Height:longint;
{26+02 : Number of image planes : should be 1 always}
   Planes:word;
{28+02 : Color resolution : Number of bits per pixel (1,4,8,16,24,32)}
   BitCount:word;
{30+04 : Compression Type}
   Compression:longint;
{34+04 : Size of image data (not headers nor palette): can be 0 if no compression}
   SizeImage:longint;
{38+04 : Horizontal resolution in pixel/meter}
   XPelsPerMeter:Longint;
{42+04 : Vertical resolution in pixel/meter}
   YPelsPerMeter:Longint;
{46+04 : Number of colors used}
   ClrUsed:longint;
{50+04 : Number of imprtant colors used : useful for displaying on VGA256}
   ClrImportant:longint;
*)
begin
  FInfoHeader := Default(TBitmapInfoHeader);
  AStream.Read(FInfoHeader, SizeOf(TBitmapInfoHeader));

  InfoHeaderValueList.RowCount := 1;
  with FInfoHeader do
  begin
    InfoHeaderValueList.InsertRow('Size of info header', Format('%0:d ($%0:4x)', [Size]), true);
    InfoHeaderValueList.InsertRow('Image width', Format('%d pixels', [Width]), true);
    InfoHeaderValueList.InsertRow('Image height', Format('%d pixels', [Height]), true);
    InfoHeaderValueList.InsertRow('Number of planes', Format('%d', [Planes]), true);
    InfoHeaderValueList.InsertRow('Bits per pixel', Format('%d', [BitCount]), true);
    InfoHeaderValueList.InsertRow('Compression', Format('%d (%s)', [Compression, COMPRESSION_NAME[Compression]]), true);
    InfoHeaderValueList.InsertRow('Image data size (w/o headers and palette', Format('%0:d ($%0:4x)', [SizeImage]), true);
    InfoHeaderValueList.InsertRow('Horizontal resolution', Format('%d px/m, %.0f ppi', [XPelsPerMeter, XPelsPerMeter * 0.0254]), true);
    InfoHeaderValueList.InsertRow('Vertical resolution', Format('%d px/m, %.0f ppi', [YPelsPerMeter, YPelsPerMeter * 0.0254]), true);
    InfoHeaderValueList.InsertRow('Count of colors used', Format('%d', [ClrUsed]), true);
    InfoHeaderValueList.InsertRow('Count of important colors used', Format('%d', [ClrImportant]), true);
  end;
end;

procedure TMainForm.LoadHex(AStream: TStream);
begin
  AStream.Position := 0;
  FHexEditor.LoadFromStream(AStream);
  HexEditorClick(nil);
end;

procedure TMainForm.ShellListViewFileAdded(Sender: TObject; Item: TListItem);
begin
  Item.ImageIndex := 1;
end;

procedure TMainForm.ShellListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Selected then
    LoadFile(ShellListView.GetPathFromItem(Item));
end;

procedure TMainForm.ShellTreeViewGetImageIndex(Sender: TObject;
  Node: TTreeNode);
begin
  Node.ImageIndex := 0;
end;

procedure TMainForm.ShellTreeViewGetSelectedIndex(Sender: TObject;
  Node: TTreeNode);
begin
  Node.SelectedIndex := 0;
end;

procedure TMainForm.UpdateStatusbar;
var
  offs: Int64;
begin
  offs := FHexEditor.SelStart;
  if offs > -1 then
    Statusbar.Panels[PANEL_OFFSET].Text := Format('HexViewer offset: %d ($%x)', [offs, offs])
  else
    Statusbar.Panels[PANEL_OFFSET].Text := '';
end;

procedure TMainForm.UpdateSummary;
begin
  SummaryValueList.RowCount := 1;
  with FInfoHeader do
  begin
    SummaryValueList.InsertRow('Image width and height', Format('%d x %d', [Width, Height]), true);
    SummaryValueList.InsertRow('Horizontal resolution', Format('%d px/m, %.0f ppi', [XPelsPerMeter, XPelsPerMeter * 0.0254]), true);
    SummaryValueList.InsertRow('Vertical resolution', Format('%d px/m, %.0f ppi', [YPelsPerMeter, YPelsPerMeter * 0.0254]), true);
    SummaryValueList.InsertRow('Bits per pixel', Format('%d', [BitCount]), true);
  end;
  if FColorFormat <> '' then
    SummaryValueList.InsertRow('16 bit color mask', FColorFormat, true);
end;

end.

