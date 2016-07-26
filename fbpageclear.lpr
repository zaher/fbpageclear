program fbpageclear;
{**
> gfix -user sysdba -pass masterkey data.fdb -v -full
database file appears corrupt ()
-bad checksum
-checksum error on database page 36813

> fbpageclear.exe data.fdb 36813
data.fdb 36813
Page Size:4096
Offset: 150786048
Done.


>fbpageclear d:\temp\data.fdb -s -f

isql>connect d:\temp\data.fdb user SYSDBA password masterkey;

*}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this };

type
  //36516
  { TFBPageClear }

  TFBPageClear = class(TCustomApplication)
  protected
    PageSize: Integer;
    FileName: string;
    procedure DoRun; override;
    procedure ClearPage(vPage: Int64);
    procedure ClearPage(aFile: TFileStream; vPage: Int64);
    procedure ScanPages(Fix: Boolean; All: Boolean = True);
    procedure Truncate(vPage: Int64);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TFBPageClear }

procedure TFBPageClear.DoRun;
var
  aParams: TStringList;
  aPage: string;
begin
  PageSize := 4096;

  {ErrorMsg := CheckOptions('h', 'help');
  if ErrorMsg <>'' then
  begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;}

  if HasOption('h', 'help') or (ParamCount = 0) then
    WriteHelp
  else
  begin
    aParams:=TStringList.Create;
    try
      CheckOptions('', nil, nil, aParams, False);
      if aParams.Count > 0 then
        FileName := aParams[0]
      else
      begin
        WriteLn('Please define the file name.');
        WriteHelp;
      end;

      if HasOption('p', 'page') then
      begin
        PageSize := StrToInt(GetOptionValue('p', 'page'));
      end;

      WriteLn(FileName + ' ' + IntToStr(PageSize));

      if HasOption('s', 'scan') then
      begin
        ScanPages(HasOption('f', 'fix'));
      end
      else if HasOption('t', 'trunc') then
      begin
        if aParams.Count > 1 then
          aPage := aParams[1]
        else
        begin
          WriteLn('Please define the page number.');
          WriteHelp;
          Terminate;
          Exit;
        end;
        Truncate(StrToInt(aPage));
      end
      else
      begin
        if aParams.Count > 1 then
          aPage := aParams[1]
        else
        begin
          WriteLn('Please define the page number.');
          WriteHelp;
          Terminate;
          Exit;
        end;
        WriteLn('Page Size:' + IntToStr(PageSize));
        ClearPage(StrToInt64(aPage));
        Writeln('Done.');
      end;
    finally
      aParams.Free;
    end;
  end;

  //ReadLn();
  Terminate;
end;

type
  TPageHeader = packed record
    PageType: Byte;
    Flag: Byte;
    CheckSum: Word;
  end;

const
  sCheckSum = 12345;

procedure TFBPageClear.ClearPage(vPage: Int64);
var
  aFile:TFileStream;
  offset: Int64;
  i: Integer;
begin
  aFile := TFileStream.Create(FileName, fmOpenReadWrite);
  try
    offset := vPage * PageSize;
    writeln('Offset: ' + IntToStr(offset));
    aFile.Seek(offset, soBeginning);
    aFile.WriteByte($07);
    aFile.WriteByte($01);
    aFile.WriteByte($39);
    aFile.WriteByte($30);
    for i := 4 to PageSize - 1 do
      aFile.WriteByte(0);
  finally
    aFile.Free;
  end;
end;

procedure TFBPageClear.ClearPage(aFile: TFileStream; vPage: Int64);
var
  offset: Int64;
  i: Integer;
  PageHeader: TPageHeader;
begin
  offset := vPage * PageSize;
  aFile.Seek(offset, soBeginning);
  Finalize(PageHeader);

  PageHeader.CheckSum := sCheckSum;
  PageHeader.Flag := 1;
  PageHeader.PageType := 0;

  aFile.WriteBuffer(PageHeader, SizeOf(PageHeader));
  for i := SizeOf(PageHeader) to PageSize - 1 do
    aFile.WriteByte(0);
  WriteLn('Page: '+ IntToStr(vPage) + ' is fixed');
end;

procedure TFBPageClear.ScanPages(Fix: Boolean; All: Boolean);
var
  aFile:TFileStream;
  size, offset: Int64;
  p: Int64;
  PageHeader: TPageHeader;
  FoundErrors: Integer;
begin
  Writeln('Scanning...');
  aFile := TFileStream.Create(FileName, fmOpenReadWrite);
  try
    Finalize(PageHeader);
    FoundErrors := 0;
    size := aFile.Size;
    p := 0;
    offset := p * PageSize;
    while (offset < size) and (not Terminated) do
    begin
      Write('Checked '+ InttoStr(p)+#13);
      aFile.Seek(offset, soBeginning);
      aFile.ReadBuffer(PageHeader, SizeOf(PageHeader));
      if PageHeader.CheckSum <> sCheckSum then
      begin
        Writeln('Page CheckSum Error: ' + IntToStr(p)+ ' CheckSum: ' + IntToStr(PageHeader.CheckSum));
        Inc(FoundErrors);
        ClearPage(aFile, p);
      end;
      inc(p);
      offset := p * PageSize;
      //if p> 10 then  exit;
    end;
    if FoundErrors = 0  then
      Writeln('No Errors found.')
    else
      Writeln('Errors found check it!');
  finally
    aFile.Free;
  end;
end;

procedure TFBPageClear.Truncate(vPage: Int64);
var
  aFile:TFileStream;
  offset: Int64;
  i: Integer;
begin
  aFile := TFileStream.Create(FileName, fmOpenReadWrite);
  try
    offset := vPage * PageSize;
    writeln('Offset: ' + IntToStr(offset));
    //aFile.Seek(offset, soBeginning);
    aFile.Size := offset;
  finally
    aFile.Free;
  end;
end;

constructor TFBPageClear.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException :=True;
end;

destructor TFBPageClear.Destroy;
begin
  inherited Destroy;
end;

procedure TFBPageClear.WriteHelp;
begin
  writeln('Usage: ', ExtractFileName(ExeName), ' filename pagenumber');
end;

var
  Application: TFBPageClear;
begin
  Application :=TFBPageClear.Create(nil);
  Application.Title :='FB Page Clear';
  Application.Run;
  Application.Free;
end.

