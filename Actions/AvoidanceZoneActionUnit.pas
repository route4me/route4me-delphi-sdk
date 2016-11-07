unit AvoidanceZoneActionUnit;

interface

uses
  SysUtils, BaseActionUnit,
  AvoidanceZoneParametersUnit, AvoidanceZoneUnit, AvoidanceZoneQueryUnit;

type
  TAvoidanceZoneActions = class(TBaseAction)
  public
    function Get(AvoidanceZoneQuery: TAvoidanceZoneQuery;
      out ErrorString: String): TAvoidanceZone;
    function GetList(AvoidanceZoneQuery: TAvoidanceZoneQuery;
      out ErrorString: String): TAvoidanceZoneArray;
    function Add(AvoidanceZoneParameters: TAvoidanceZoneParameters;
      out ErrorString: String): TAvoidanceZone;
    function Delete(Query: TAvoidanceZoneQuery;
      out ErrorString: String): boolean;
    function Update(Parameters: TAvoidanceZoneParameters;
      out ErrorString: String): TAvoidanceZone;
  end;

implementation

{ TAvoidanceZoneActions }

uses
  SettingsUnit;

function TAvoidanceZoneActions.Add(
  AvoidanceZoneParameters: TAvoidanceZoneParameters;
  out ErrorString: String): TAvoidanceZone;
begin
  Result := FConnection.Post(TSettings.Avoidance, AvoidanceZoneParameters,
    TAvoidanceZone, ErrorString) as TAvoidanceZone;
end;

function TAvoidanceZoneActions.Delete(Query: TAvoidanceZoneQuery;
  out ErrorString: String): boolean;
begin
  FConnection.Delete(TSettings.Avoidance, Query, TAvoidanceZone, ErrorString);
  Result := (ErrorString <> EmptyStr);
end;

function TAvoidanceZoneActions.Get(AvoidanceZoneQuery: TAvoidanceZoneQuery;
  out ErrorString: String): TAvoidanceZone;
begin
  Result := FConnection.Get(TSettings.Avoidance, AvoidanceZoneQuery,
    TAvoidanceZone, ErrorString) as TAvoidanceZone;
end;

function TAvoidanceZoneActions.GetList(AvoidanceZoneQuery: TAvoidanceZoneQuery;
  out ErrorString: String): TAvoidanceZoneArray;
var
  List: TAvoidanceZoneList;
begin
  SetLength(Result, 0);
  List := FConnection.Get(TSettings.Avoidance, AvoidanceZoneQuery,
    TAvoidanceZoneList, ErrorString) as TAvoidanceZoneList;
  try
    if (List <> nil) then
      Result := List.ToArray;
  finally
    FreeAndNil(List);
  end;
end;

function TAvoidanceZoneActions.Update(Parameters: TAvoidanceZoneParameters;
  out ErrorString: String): TAvoidanceZone;
begin
  Result := FConnection.Put(TSettings.Avoidance, Parameters,
    TAvoidanceZone, ErrorString) as TAvoidanceZone;
end;

end.
