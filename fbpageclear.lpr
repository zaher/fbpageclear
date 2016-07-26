program fbpageclear;

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
    procedure DoRun; override;
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
  aPage: string;
begin
  aParams:=TStringList.Create;
  try
    CheckOptions('', nil, nil, aParams, False);
    if aParams.Count > 0 then
      aPage := aParams[0];

    WriteLn(aPage);
    // quick check parameters
    ErrorMsg :=CheckOptions('h', 'help');
    if ErrorMsg <>'' then begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then begin
      WriteHelp;
      Terminate;
      Exit;
    end;
  finally
    aParams.Free;
  end;
  { add your program here }

  ReadLn();
  // stop program loop
  Terminate;
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
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TFBPageClear;
begin
  Application :=TFBPageClear.Create(nil);
  Application.Title :='FB Page Clear';
  Application.Run;
  Application.Free;
end.

