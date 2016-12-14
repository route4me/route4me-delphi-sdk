unit UpdateTerritoryUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TUpdateTerritory = class(TBaseExample)
  public
    procedure Execute(TerritoryId: String);
  end;

implementation

uses TerritoryUnit, TerritoryContourUnit;

procedure TUpdateTerritory.Execute(TerritoryId: String);
var
  ErrorString: String;
  Territory: TTerritory;
  TerritoryContour: TTerritoryContour;
  NewTerritory: TTerritory;
  TerritoryName, TerritoryColor: String;
begin
  TerritoryName := 'Test Territory Updated';
  TerritoryColor := 'ff00ff';
  TerritoryContour := TerritoryContour.MakeCircleContour(
    38.4132225905681, -78.501953234, 3000);
  Territory := TTerritory.Create(TerritoryName, TerritoryColor, TerritoryContour);
  try
    Territory.Id := TerritoryId;

    NewTerritory := Route4MeManager.Territory.Update(Territory, ErrorString);
    try
      WriteLn('');

      if (NewTerritory <> nil) then
      begin
        WriteLn('UpdateTerritory executed successfully');
        WriteLn(Format('Territory ID: %s', [NewTerritory.Id.Value]));
      end
      else
        WriteLn(Format('UpdateTerritory error: "%s"', [ErrorString]));
    finally
      FreeAndNil(NewTerritory);
    end;
  finally
    FreeAndNil(Territory);
  end;
end;

end.
