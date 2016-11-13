unit AvoidanceZoneActionUnit;

interface

uses
  SysUtils, BaseActionUnit,
  AvoidanceZoneParametersUnit, AvoidanceZoneUnit, AvoidanceZoneQueryUnit;

type
  TAvoidanceZoneActions = class(TBaseAction)
  public
    function Get(TerritoryId: String; out ErrorString: String): TAvoidanceZone;
    function GetList(out ErrorString: String): TAvoidanceZoneArray;
    function Add(AvoidanceZoneParameters: TAvoidanceZoneParameters;
      out ErrorString: String): TAvoidanceZone;
    function Delete(TerritoryId: String; out ErrorString: String): boolean;
    function Update(Parameters: TAvoidanceZoneParameters;
      out ErrorString: String): TAvoidanceZone;
  end;

implementation

{ TAvoidanceZoneActions }

uses
  SettingsUnit, GenericParametersUnit;

function TAvoidanceZoneActions.Add(
  AvoidanceZoneParameters: TAvoidanceZoneParameters;
  out ErrorString: String): TAvoidanceZone;
begin
  Result := FConnection.Post(TSettings.Avoidance, AvoidanceZoneParameters,
    TAvoidanceZone, ErrorString) as TAvoidanceZone;
end;

function TAvoidanceZoneActions.Delete(TerritoryId: String;
  out ErrorString: String): boolean;
var
  Query: TGenericParameters;
begin
  Query := TGenericParameters.Create;
  try
    Query.AddParameter('territory_id', TerritoryId);

    FConnection.Delete(TSettings.Avoidance, Query, TAvoidanceZone, ErrorString);
    Result := (ErrorString <> EmptyStr);
  finally
    FreeAndNil(Query);
  end;
end;

function TAvoidanceZoneActions.Get(TerritoryId: String;
  out ErrorString: String): TAvoidanceZone;
var
  Query: TGenericParameters;
begin
  Query := TGenericParameters.Create;
  try
    Query.AddParameter('territory_id', TerritoryId);

    Result := FConnection.Get(TSettings.Avoidance, Query,
      TAvoidanceZone, ErrorString) as TAvoidanceZone;
  finally
    FreeAndNil(Query);
  end;
end;

function TAvoidanceZoneActions.GetList(out ErrorString: String): TAvoidanceZoneArray;
var
  List: TAvoidanceZoneList;
  Query: TGenericParameters;
begin
  Query := TGenericParameters.Create;
  try
    SetLength(Result, 0);
    List := FConnection.Get(TSettings.Avoidance, Query,
      TAvoidanceZoneList, ErrorString) as TAvoidanceZoneList;
    try
      if (List <> nil) then
        Result := List.ToArray;
    finally
      FreeAndNil(List);
    end;
  finally
    FreeAndNil(Query);
  end;
end;

function TAvoidanceZoneActions.Update(Parameters: TAvoidanceZoneParameters;
  out ErrorString: String): TAvoidanceZone;
begin
  Result := FConnection.Put(TSettings.Avoidance, Parameters,
    TAvoidanceZone, ErrorString) as TAvoidanceZone;
end;

end.
