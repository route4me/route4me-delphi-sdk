unit TerritoryActionsUnit;

interface

uses
  SysUtils, BaseActionUnit,
  TerritoryUnit, EnumsUnit, NullableBasicTypesUnit, TerritoryContourUnit;

type
  TTerritoryActions = class(TBaseAction)
  public
    function Add(Name: String; Color: String; Contour: TTerritoryContour;
      out ErrorString: String): NullableString;

    function Update(Territory: TTerritory; out ErrorString: String): boolean;

    function Remove(TerritoryId: String; out ErrorString: String): boolean;

    /// <summary>
    ///  GET all of the Territories defined by a user.
    /// </summary>
    function GetList(out ErrorString: String): TTerritoryList;

    /// <summary>
    /// Get all recorded activities associated not only with a specific Route4Me account,
    /// but also with other users of a member’s team.
    /// </summary>
    function Get(TerritoryId: String; GetEnclosedAddresses: boolean;
      out ErrorString: String): TTerritory;
  end;

implementation

uses
  SettingsUnit, GetActivitiesResponseUnit, StatusResponseUnit,
  GenericParametersUnit, GetActivitiesQueryUnit, GetTerritoriesResponseUnit;

function TTerritoryActions.Add(Name: String; Color: String;
  Contour: TTerritoryContour; out ErrorString: String): NullableString;
var
  Response: TTerritory;
  Parameters: TTerritory;
  i: integer;
begin
  Result := NullableString.Null;

  Parameters := TTerritory.Create(Name, Color, Contour);
  try
    Response := FConnection.Post(TSettings.EndPoints.Territory, Parameters,
      TTerritory, ErrorString) as TTerritory;
    try
      if (Response <> nil) then
        Result := Response.TerritoryId
      else
        ErrorString := 'Territory not added';
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

function TTerritoryActions.Get(TerritoryId: String; GetEnclosedAddresses: boolean;
  out ErrorString: String): TTerritory;
var
  Response: TGetActivitiesResponse;
  Request: TGenericParameters;
begin
  Request := TGenericParameters.Create();
  try
    Request.AddParameter('territory_id', TerritoryId);
    if GetEnclosedAddresses then
      Request.AddParameter('addresses', '1');

    Result := FConnection.Get(TSettings.EndPoints.Territory, Request,
      TTerritory, ErrorString) as TTerritory;
    try
      if (Response = nil) and (ErrorString = EmptyStr) then
        ErrorString := 'Get Territory fault';
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

function TTerritoryActions.GetList(out ErrorString: String): TTerritoryList;
var
  Response: TGetTerritoriesResponse;
  Parameters: TGenericParameters;
  i: integer;
begin
  Result := TTerritoryList.Create;

  Parameters := TGenericParameters.Create();
  try
    Response := FConnection.Get(TSettings.EndPoints.Territory, Parameters,
      TGetTerritoriesResponse, ErrorString) as TGetTerritoriesResponse;
    try
      if (Response <> nil) then
      begin
        for i := 0 to Length(Response.Territories) - 1 do
          Result.Add(Response.Territories[i]);
      end;
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

function TTerritoryActions.Remove(TerritoryId: String;
  out ErrorString: String): boolean;
var
  Request: TGenericParameters;
  Response: TStatusResponse;
begin
  Request := TGenericParameters.Create();
  try
    Request.AddParameter('territory_id', TerritoryId);

    Response := FConnection.Delete(TSettings.EndPoints.Territory, Request,
      TStatusResponse, ErrorString) as TStatusResponse;
    try
      Result := (Response <> nil) and (Response.Status);
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

function TTerritoryActions.Update(Territory: TTerritory;
  out ErrorString: String): boolean;
begin

end;

end.
