unit bsMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LCLType, Forms, Controls, Graphics, Dialogs, ShellCtrls, ExtCtrls,
  ComCtrls, ValEdit, bmpComn;

type

  { TMainForm }

  TMainForm = class(TForm)
    ColorTableValueList: TValueListEditor;
    InfoHeaderValueList: TValueListEditor;
    Image1: TImage;
    MainPageControl: TPageControl;
    DataPageControl: TPageControl;
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    ShellListView1: TShellListView;
    ShellTreeView1: TShellTreeView;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    pgImage: TTabSheet;
    pgHex: TTabSheet;
    pgData: TTabSheet;
    pgFileHeader: TTabSheet;
    FileHeaderValueList: TValueListEditor;
    pgBitmapInfoHeader: TTabSheet;
    pgColorTable: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure ShellListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FFileHeader: TBitmapFileHeader;
    FInfoHeader: TBitmapInfoHeader;
    FColorFormat: String;
    procedure LoadColorTable(AStream: TStream);
    procedure LoadFileHeader(AStream: TStream);
    procedure LoadImage(AStream: TStream);
    procedure LoadInfoHeader(AStream: TStream);

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

{ TMainForm }

procedure TMainForm.LoadColorTable(AStream: TStream);
const
  COLORS: array[0..2] of string = ('red', 'green', 'blue');
var
  i, j, n: Integer;
  rgb: TRGBQuad;
  s: String;
  dw: DWord;
  bits: array[0..2] of byte = (0, 0, 0);
  alpha: string = '';
begin
  ColorTableValueList.RowCount := 1;
  case FInfoHeader.Compression of
    BI_RGB:
      if FInfoHeader.BitCount <= 8 then
      begin
        pgColorTable.Caption := 'Palette';
        n := FInfoHeader.ClrUsed;
        if n = 0 then n := 2 ** FInfoHeader.BitCount;
        for i := 0 to n-1 do
        begin
          AStream.Read(rgb, SizeOf(TRGBQuad));
          ColorTableValueList.InsertRow('Color #' + i.ToString, Format('%0:d ($%0:.8x)', [dword(rgb)]), true);
        end;
      end else
        ColorTableValueList.InsertRow('No palette', '', true);
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
        ColorTableValueList.InsertRow('Format', FColorFormat, true);
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

  SetLength(signature, 2);
  Move(FFileHeader.bfType, signature[1], 2);

  FileHeaderValueList.RowCount := 1;
  FileHeaderValueList.InsertRow('Type', Format('%0:s (%1:d = $%1:2x)', [signature, FFileHeader.bfType]), true);
  FileHeaderValueList.InsertRow('Size', Format('%.0n bytes', [1.0*FFileHeader.bfSize]), true);
  FileHeaderValueList.InsertRow('Reserved', Format('%0:d ($%0:4x)', [FFileHeader.bfReserved]), true);
  FileHeaderValueList.InsertRow('Offset to image data', Format('%0:d ($%0:4x)', [FFileHeader.bfOffset]), true);
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
procedure TMainForm.ShellListView1SelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  LoadFile(ShellListView1.GetPathFromItem(Item));
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := APP_TITLE;
  ShellTreeView1.Path := Application.Location + '../../samples/';
  ShellListView1.Root := Application.Location + '../../samples/';
end;

end.

