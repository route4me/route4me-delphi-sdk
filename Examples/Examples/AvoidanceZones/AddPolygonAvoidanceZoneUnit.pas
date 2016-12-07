unit AddPolygonAvoidanceZoneUnit;

interface

uses SysUtils, BaseExampleUnit, NullableBasicTypesUnit;

type
  TAddPolygonAvoidanceZone = class(TBaseExample)
  public
    function Execute: NullableString;
  end;

implementation

uses TerritoryUnit, AvoidanceZoneUnit;

function TAddPolygonAvoidanceZone.Execute: NullableString;
var
  ErrorString: String;
  AvoidanceZone: TAvoidanceZone;
  Territory: TPolygonTerritory;
  NewAvoidanceZone: TAvoidanceZone;
  TerritoryName, TerritoryColor: String;
begin
  Result := NullableString.Null;

  TerritoryName := 'Polygon Territory';
  TerritoryColor := 'ff0000';
  Territory := TPolygonTerritory.Create();
  Territory.AddPoint(37.7697528227865, -77.6783325195313);
  Territory.AddPoint(37.7588671630534, -77.6897480010986);
  Territory.AddPoint(37.7476396605445, -77.6917221069336);
  Territory.AddPoint(37.7465508430681, -77.6886322021484);
  Territory.AddPoint(37.7502255383101, -77.6812507629394);
  Territory.AddPoint(37.7479799127443, -77.6749851226806);
  Territory.AddPoint(37.7332796020606, -77.6411678314209);
  Territory.AddPoint(37.7443051067953, -77.6317264556884);
  Territory.AddPoint(37.7664192584704, -77.6684619903564);

  AvoidanceZone := TAvoidanceZone.Create(TerritoryName, TerritoryColor, Territory);
  try
    NewAvoidanceZone := Route4MeManager.AvoidanceZone.Add(AvoidanceZone, ErrorString);
    try
      WriteLn('');

      if (NewAvoidanceZone <> nil) then
      begin
        WriteLn('AddPolygonAvoidanceZone executed successfully');
        WriteLn(Format('Territory ID: %s', [NewAvoidanceZone.TerritoryId.Value]));

        Result := NewAvoidanceZone.TerritoryId;
      end
      else
        WriteLn(Format('AddPolygonAvoidanceZone error: "%s"', [ErrorString]));
    finally
      FreeAndNil(NewAvoidanceZone);
    end;
  finally
    FreeAndNil(AvoidanceZone);
  end;
end;

end.
