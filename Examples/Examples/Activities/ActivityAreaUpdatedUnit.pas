unit ActivityAreaUpdatedUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TActivityAreaUpdated = class(TBaseExample)
  public
    procedure Execute;
  end;

implementation

procedure TActivityAreaUpdated.Execute;
var
  ErrorString: String;
begin
  Route4MeManager.ActivityFeed.AreaUpdated(ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
    WriteLn('ActivityAreaUpdated executed successfully')
  else
    WriteLn(Format('ActivityAreaUpdated error: "%s"', [ErrorString]));
end;

end.
