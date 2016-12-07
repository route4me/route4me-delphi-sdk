unit RealAvoidanceZoneProviderUnit;

interface

uses
  SysUtils,
  AvoidanceZoneUnit, IAvoidanceZoneProviderUnit;

type
  TRealAvoidanceZoneProvider = class(TInterfacedObject, IAvoidanceZoneProvider)
  protected
  public
    function AvoidanceZones: TAvoidanceZoneList;
  end;

implementation

uses
  DateUtils,
  EnumsUnit, UtilsUnit, JSONDictionaryIntermediateObjectUnit,
  RouteParametersUnit, LinksUnit, AddressUnit, TerritoryUnit;

function TRealAvoidanceZoneProvider.AvoidanceZones: TAvoidanceZoneList;
var
  AvoidanceZone: TAvoidanceZone;
begin
  Result := TAvoidanceZoneList.Create;

  AvoidanceZone := TAvoidanceZone.Create;
  AvoidanceZone.TerritoryId := '1C4EC91F7C83E6D44A39C5EF0ED15422';
  AvoidanceZone.TerritoryName := 'Johny4037200794235010051';
  AvoidanceZone.TerritoryColor := 'ff0000';
  AvoidanceZone.MemberId := '1';
  AvoidanceZone.Territory := TPolygonTerritory.Create;
  TPolygonTerritory(AvoidanceZone.Territory).AddPoint(56.127184156131065,56.93115234375);
  TPolygonTerritory(AvoidanceZone.Territory).AddPoint(58.41322259056806, 59.501953125);
  TPolygonTerritory(AvoidanceZone.Territory).AddPoint(61.53840616716746, 59.315185546875);
  TPolygonTerritory(AvoidanceZone.Territory).AddPoint(61.047650586031104,51.998291015625);
  TPolygonTerritory(AvoidanceZone.Territory).AddPoint(59.254649544483726,53.63525390625);
  TPolygonTerritory(AvoidanceZone.Territory).AddPoint(56.47462805805596, 54.42626953125);
  Result.Add(AvoidanceZone);

  AvoidanceZone := TAvoidanceZone.Create;
  AvoidanceZone.TerritoryId := '1E8516BDBBA2A27753B768B0F628A27B';
  AvoidanceZone.TerritoryName := 'John5577006791947779410';
  AvoidanceZone.TerritoryColor := 'beeeee';
  AvoidanceZone.MemberId := '1';
  AvoidanceZone.Territory := TCircleTerritory.Create(
    37.569752822786455,-77.47833251953125, 5000);
  Result.Add(AvoidanceZone);

  AvoidanceZone := TAvoidanceZone.Create;
  AvoidanceZone.TerritoryId := '2E10E418F0913D4C52C9D6386F41C303';
  AvoidanceZone.TerritoryName := 'Rect Territory';
  AvoidanceZone.TerritoryColor := 'ff0000';
  AvoidanceZone.MemberId := '1';
  AvoidanceZone.Territory := TRectangularTerritory.Create(
    43.51668853502909, -109.3798828125, 46.98025235521883, -101.865234375);
  Result.Add(AvoidanceZone);
end;

end.
