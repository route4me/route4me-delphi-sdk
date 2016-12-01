unit ActivityAreaAddedUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TActivityAreaAdded = class(TBaseExample)
  public
    procedure Execute;
  end;

implementation

procedure TActivityAreaAdded.Execute;
var
  ErrorString: String;
begin
  Route4MeManager.ActivityFeed.AreaAdded(ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
    WriteLn('ActivityAreaAdded executed successfully')
  else
    WriteLn(Format('ActivityAreaAdded error: "%s"', [ErrorString]));
end;

end.
