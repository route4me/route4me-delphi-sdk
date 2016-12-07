unit AvoidanceZoneActionUnit;

interface

uses
  SysUtils, BaseActionUnit,
  AvoidanceZoneUnit, AvoidanceZoneQueryUnit;

type
  TAvoidanceZoneActions = class(TBaseAction)
  public
    /// <summary
    ///  Create an Avoidance Zone
    /// </summary
    function Add(AvoidanceZone: TAvoidanceZone; out ErrorString: String): TAvoidanceZone;

    /// <summary
    ///  Get a specified Avoidance Zone by Id.
    /// </summary
    function Get(TerritoryId: String; out ErrorString: String): TAvoidanceZone;

    /// <summary
    ///  Get all of the Avoidance Zones defined by a user.
    /// </summary
    function GetList(out ErrorString: String): TAvoidanceZoneList;

    /// <summary
    ///  Update a specified Avoidance Zone
    /// </summary
    function Update(AvoidanceZone: TAvoidanceZone; out ErrorString: String): TAvoidanceZone;

    /// <summary
    ///  Delete a specified Avoidance Zone.
    /// </summary
    function Remove(TerritoryId: String; out ErrorString: String): boolean; overload;

    /// <summary
    ///  Delete a specified Avoidance Zone.
    /// </summary
    function Remove(TerritoryIds: TArray<String>; out ErrorString: String): boolean; overload;
  end;

implementation

{ TAvoidanceZoneActions }

uses
  SettingsUnit, GenericParametersUnit;

function TAvoidanceZoneActions.Add(
  AvoidanceZone: TAvoidanceZone; out ErrorString: String): TAvoidanceZone;
begin
  Result := FConnection.Post(TSettings.Avoidance, AvoidanceZone,
    TAvoidanceZone, ErrorString) as TAvoidanceZone;
end;

function TAvoidanceZoneActions.Remove(TerritoryId: String;
  out ErrorString: String): boolean;
var
  Query: TGenericParameters;
begin
  Query := TGenericParameters.Create;
  try
    Query.AddParameter('territory_id', TerritoryId);

    FConnection.Delete(TSettings.Avoidance, Query, TAvoidanceZone, ErrorString);
    Result := (ErrorString = EmptyStr);
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

function TAvoidanceZoneActions.GetList(out ErrorString: String): TAvoidanceZoneList;
var
  Query: TGenericParameters;
begin
  Query := TGenericParameters.Create;
  try
    Result := FConnection.Get(TSettings.Avoidance, Query,
      TAvoidanceZoneList, ErrorString) as TAvoidanceZoneList;
    if (Result = nil) then
      Result := TAvoidanceZoneList.Create;
    Result.OwnsObjects := True;
  finally
    FreeAndNil(Query);
  end;
end;

function TAvoidanceZoneActions.Remove(TerritoryIds: TArray<String>;
  out ErrorString: String): boolean;
var
  Query: TGenericParameters;
  i: integer;
  AErrorString: String;
begin
  ErrorString := EmptyStr;

  Query := TGenericParameters.Create;
  try
    for i := 0 to High(TerritoryIds) do
    begin
      Query.ReplaceParameter('territory_id', TerritoryIds[i]);

      FConnection.Delete(TSettings.Avoidance, Query, TAvoidanceZone, AErrorString);
      if (AErrorString <> EmptyStr) then
        ErrorString := ErrorString + '; ' + AErrorString;
    end;

    Result := (ErrorString = EmptyStr);
  finally
    FreeAndNil(Query);
  end;
end;

function TAvoidanceZoneActions.Update(AvoidanceZone: TAvoidanceZone;
  out ErrorString: String): TAvoidanceZone;
begin
  Result := FConnection.Put(TSettings.Avoidance, AvoidanceZone,
    TAvoidanceZone, ErrorString) as TAvoidanceZone;
end;

end.
