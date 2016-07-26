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

*}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this };

type

  { TFBPageClear }

  TFBPageClear = class(TCustomApplication)
  protected
    PageSize: Integer;
    procedure DoRun; override;
    procedure ClearPage(vFileName: string; vPage: Int64);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TFBPageClear }

procedure TFBPageClear.DoRun;
var
  ErrorMsg: String;
  aParams: TStringList;
  aFileName, aPage: string;
begin
  PageSize := 4096;

  ErrorMsg := CheckOptions('h', 'help');
  if ErrorMsg <>'' then
  begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  if HasOption('h', 'help') or (ParamCount = 0) then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  aParams:=TStringList.Create;
  try
    CheckOptions('', nil, nil, aParams, False);

    if aParams.Count > 0 then
      aFileName := aParams[0]
    else
    begin
      WriteLn('Please define the file name.');
      WriteHelp;
    end;

    if aParams.Count > 1 then
      aPage := aParams[1]
    else
    begin
      WriteLn('Please define the page number.');
      WriteHelp;
    end;

    WriteLn(aFileName + ' ' + aPage);

    if HasOption('p', 'page') then
    begin
      PageSize := StrToInt(GetOptionValue('p', 'page'));
    end;
    WriteLn('Page Size:' + IntToStr(PageSize));

  finally
    aParams.Free;
  end;

  ClearPage(aFileName, StrToInt64(aPage));
  Writeln('Done.');
  //ReadLn();
  Terminate;
end;

procedure TFBPageClear.ClearPage(vFileName: string; vPage: Int64);
var
  aFile:TFileStream;
  offset: Int64;
  i: Integer;
begin
  aFile := TFileStream.Create(vFileName, fmOpenReadWrite);
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

