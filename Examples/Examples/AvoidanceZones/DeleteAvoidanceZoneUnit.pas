unit DeleteAvoidanceZoneUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TDeleteAvoidanceZone = class(TBaseExample)
  public
    procedure Execute(TerritoryId: String);
  end;

implementation

procedure TDeleteAvoidanceZone.Execute(TerritoryId: String);
var
  ErrorString: String;
begin
  Route4MeManager.AvoidanceZone.Delete(TerritoryId, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
  begin
    WriteLn('DeleteAvoidanceZone executed successfully');
    WriteLn(Format('Territory ID: %s', [TerritoryId]));
  end
  else
    WriteLn(Format('DeleteAvoidanceZone error: "%s"', [ErrorString]));
end;

end.
