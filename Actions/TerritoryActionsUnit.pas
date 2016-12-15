unit TerritoryActionsUnit;

interface

uses
  SysUtils, BaseActionUnit,
  TerritoryUnit, NullableBasicTypesUnit, TerritoryContourUnit;

type
  TTerritoryActions = class(TBaseAction)
  public
    /// <summary>
    ///  Create a new territory
    /// </summary>
    function Add(Name: String; Color: String; Contour: TTerritoryContour;
      out ErrorString: String): NullableString; overload;

    // todo: если даже отправлять список адресов на сервер, то в ответе он обнуляется
    function Add(Name: String; Color: String; Contour: TTerritoryContour;
      AddressIds: TArray<integer>; out ErrorString: String): NullableString; overload;

    /// <summary>
    ///  UPDATE a specified Territory
    /// </summary>
    function Update(Territory: TTerritory; out ErrorString: String): TTerritory;

    /// <summary>
    ///  DELETE a specified Territory.
    /// </summary>
    function Remove(TerritoryId: String; out ErrorString: String): boolean; overload;

    function Remove(TerritoryIds: TArray<String>; out ErrorString: String): boolean; overload;

    /// <summary>
    ///  GET all of the Territories defined by a user.
    /// </summary>
    function GetList(out ErrorString: String): TTerritoryList;

    /// <summary>
    ///  Get a specified Territory by ID.
    /// </summary>
    function Get(TerritoryId: String; GetEnclosedAddresses: boolean;
      out ErrorString: String): TTerritory;
  end;

implementation

uses
  SettingsUnit, StatusResponseUnit, GenericParametersUnit, GetTerritoriesResponseUnit,
  UpdateTerritoryRequestUnit;

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
        Result := Response.Id
      else
        ErrorString := 'Territory not added';
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

function TTerritoryActions.Add(Name, Color: String; Contour: TTerritoryContour;
  AddressIds: TArray<integer>; out ErrorString: String): NullableString;
var
  Response: TTerritory;
  Parameters: TTerritory;
  i: integer;
begin
  Result := NullableString.Null;

  Parameters := TTerritory.Create(Name, Color, Contour);
  try
    for i := 0 to High(AddressIds) do
      Parameters.AddAddressId(AddressIds[i]);

    Response := FConnection.Post(TSettings.EndPoints.Territory, Parameters,
      TTerritory, ErrorString) as TTerritory;
    try
      if (Response <> nil) then
        Result := Response.Id
      else
        ErrorString := 'Territory not added';
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;
// {"territory_name":"Circle Territory","territory_color":"ff0000","addresses":[{"value":123},{"value":789}],"territory":{"data":["37.5697528227865,-77.4783325195313","5000"],"type":"circle"}}>
function TTerritoryActions.Get(TerritoryId: String; GetEnclosedAddresses: boolean;
  out ErrorString: String): TTerritory;
var
  Request: TGenericParameters;
begin
  Request := TGenericParameters.Create();
  try
    Request.AddParameter('territory_id', TerritoryId);
    if GetEnclosedAddresses then
      Request.AddParameter('addresses', '1');

    Result := FConnection.Get(TSettings.EndPoints.Territory, Request,
      TTerritory, ErrorString) as TTerritory;

    if (Result = nil) and (ErrorString = EmptyStr) then
      ErrorString := 'Get Territory fault';
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

function TTerritoryActions.Remove(TerritoryIds: TArray<String>;
  out ErrorString: String): boolean;
var
  Query: TGenericParameters;
  i: integer;
  AErrorString: String;
  Response: TStatusResponse;
begin
  Result := True;
  ErrorString := EmptyStr;

  Query := TGenericParameters.Create;
  try
    for i := 0 to High(TerritoryIds) do
    begin
      Query.ReplaceParameter('territory_id', TerritoryIds[i]);

      Response := FConnection.Delete(TSettings.EndPoints.Territory, Query,
        TStatusResponse, AErrorString) as TStatusResponse;
      try
        Result := Result and (Response <> nil) and (Response.Status);

        if (AErrorString <> EmptyStr) then
          ErrorString := ErrorString + '; ' + AErrorString;
      finally
        FreeAndNil(Response);
      end;
    end;
  finally
    FreeAndNil(Query);
  end;
end;

function TTerritoryActions.Remove(TerritoryId: String;
  out ErrorString: String): boolean;
begin
  Result := Remove([TerritoryId], ErrorString);
end;

function TTerritoryActions.Update(Territory: TTerritory;
  out ErrorString: String): TTerritory;
var
  Request: TUpdateTerritoryRequest;
begin
  Request := TUpdateTerritoryRequest.Create(Territory);
  try
    Result := FConnection.Put(TSettings.EndPoints.Territory, Request,
      TTerritory, ErrorString) as TTerritory;
  finally
    FreeAndNil(Request);
  end;
end;

end.
