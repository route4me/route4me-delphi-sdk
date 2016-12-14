unit UpdateAvoidanceZoneUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TUpdateAvoidanceZone = class(TBaseExample)
  public
    procedure Execute(TerritoryId: String);
  end;

implementation

uses AvoidanceZoneUnit, TerritoryContourUnit;

procedure TUpdateAvoidanceZone.Execute(TerritoryId: String);
var
  ErrorString: String;
  AvoidanceZone: TAvoidanceZone;
  Territory: TTerritoryContour;
  NewAvoidanceZone: TAvoidanceZone;
  TerritoryName, TerritoryColor: String;
begin
  TerritoryName := 'Test Territory Updated';
  TerritoryColor := 'ff00ff';
  Territory := TTerritoryContour.MakeCircleContour(
    38.4132225905681, -78.501953234, 3000);
  AvoidanceZone := TAvoidanceZone.Create(TerritoryName, TerritoryColor, Territory);
  try
    AvoidanceZone.TerritoryId := TerritoryId;

    NewAvoidanceZone := Route4MeManager.AvoidanceZone.Update(AvoidanceZone, ErrorString);
    try
      WriteLn('');

      if (NewAvoidanceZone <> nil) then
      begin
        WriteLn('UpdateAvoidanceZone executed successfully');
        WriteLn(Format('Territory ID: %s', [NewAvoidanceZone.TerritoryId.Value]));
      end
      else
        WriteLn(Format('UpdateAvoidanceZone error: "%s"', [ErrorString]));
    finally
      FreeAndNil(NewAvoidanceZone);
    end;
  finally
    FreeAndNil(AvoidanceZone);
  end;
end;

end.
