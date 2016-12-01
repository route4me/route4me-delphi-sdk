unit ActivityAreaRemovedUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TActivityAreaRemoved = class(TBaseExample)
  public
    procedure Execute;
  end;

implementation

procedure TActivityAreaRemoved.Execute;
var
  ErrorString: String;
begin
  Route4MeManager.ActivityFeed.AreaRemoved(ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
    WriteLn('ActivityAreaRemoved executed successfully')
  else
    WriteLn(Format('ActivityAreaRemoved error: "%s"', [ErrorString]));
end;

end.
