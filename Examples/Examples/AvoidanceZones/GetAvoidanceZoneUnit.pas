unit GetAvoidanceZoneUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetAvoidanceZone = class(TBaseExample)
  public
    procedure Execute(TerritoryId: String);
  end;

implementation

uses AvoidanceZoneUnit;

procedure TGetAvoidanceZone.Execute(TerritoryId: String);
var
  ErrorString: String;
  AvoidanceZone: TAvoidanceZone;
begin
  AvoidanceZone := Route4MeManager.AvoidanceZone.Get(TerritoryId, ErrorString);
  try
    WriteLn('');

    if (AvoidanceZone <> nil) then
    begin
      WriteLn('GetAvoidanceZone executed successfully');
      WriteLn(Format('Territory ID: %s', [AvoidanceZone.TerritoryId.Value]));
    end
    else
      WriteLn(Format('GetAvoidanceZone error: %s', [ErrorString]));
  finally
    FreeAndNil(AvoidanceZone);
  end;
end;

end.
