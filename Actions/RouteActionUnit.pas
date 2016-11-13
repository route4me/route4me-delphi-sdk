unit RouteActionUnit;

interface

uses
  SysUtils, BaseActionUnit,
  DataObjectUnit, RouteParametersUnit, AddressUnit,
  AddressesOrderInfoUnit, RouteParametersQueryUnit,
  CommonTypesUnit, NullableBasicTypesUnit;

type
  TRouteActions = class(TBaseAction)
  private
    function GetRouteId(OptimizationProblemId: NullableString;
      out ErrorString: String): NullableString;
  public
    function Resequence(AddressesOrderInfo: TAddressesOrderInfo;
      out ErrorString: String): TDataObjectRoute;

    /// <summary>
    /// Add address(es) into a route.
    /// </summary>
    /// <param name="RouteId"> Route ID </param>
    /// <param name="Addresses"> Valid array of Address objects. </param>
    /// <param name="OptimalPosition"> If true, an address will be inserted at optimal position of a route </param>
    /// <param name="ErrorString"> out: Error as string </param>
    /// <returns> IDs of added addresses </returns>
    function Add(RouteId: String; Addresses: TAddressesArray;
      OptimalPosition: boolean; out ErrorString: String): TArray<integer>; overload;

    function Remove(RouteId: String; DestinationId: integer;
      out ErrorString: String): boolean;

    function Update(RouteParameters: TRouteParametersQuery;
      out ErrorString: String): TDataObjectRoute;

    function MoveDestinationToRoute(ToRouteId: String;
      RouteDestinationId, AfterDestinationId: integer; out ErrorString: String): boolean;

    function Get(RouteParameters: TRouteParametersQuery;
      out ErrorString: String): TDataObjectRoute;

    function GetList(RouteParameters: TRouteParametersQuery;
      out ErrorString: String): TArray<TDataObjectRoute>;

    function Delete(RouteIds: TStringArray;
      out ErrorString: String): TStringArray;

    function Duplicate(QueryParameters: TRouteParametersQuery;
      out ErrorString: String): NullableString;

    /// <summary>
    ///  Share a route via email.
    /// </summary>
    procedure Share(RouteId: String; RecipientEmail: String; out ErrorString: String);
  end;

implementation

{ TRouteActions }

uses
  System.Generics.Collections,
  SettingsUnit, RemoveRouteDestinationResponseUnit,
  RemoveRouteDestinationRequestUnit, AddRouteDestinationRequestUnit,
  MoveDestinationToRouteResponseUnit,
  GenericParametersUnit, DeleteRouteResponseUnit, DuplicateRouteResponseUnit,
  StatusResponseUnit, EnumsUnit;

function TRouteActions.Add(RouteId: String; Addresses: TAddressesArray;
  OptimalPosition: boolean; out ErrorString: String): TArray<integer>;
var
  Request: TAddRouteDestinationRequest;
  Response: TDataObject;
  Address, AddressResponse: TAddress;
  DestinationIds: TList<integer>;
begin
  Result := TArray<integer>.Create();

  Request := TAddRouteDestinationRequest.Create;
  try
    Request.RouteId := RouteId;
    Request.Addresses := Addresses;
    Request.OptimalPosition := OptimalPosition;

    Response := FConnection.Put(TSettings.RouteHost,
      Request, TDataObject, ErrorString) as TDataObject;

    if (Response = nil) then
      Exit;

    DestinationIds := TList<integer>.Create;
    try
      for Address in Addresses do
        for AddressResponse in Response.Addresses do
          if (AddressResponse.AddressString = Address.AddressString) and
            (AddressResponse.Latitude = Address.Latitude) and
            (AddressResponse.Longitude = Address.Longitude) and
            (AddressResponse.RouteDestinationId.IsNotNull) then
          begin
            DestinationIds.Add(AddressResponse.RouteDestinationId);
            Break;
          end;
      Result := DestinationIds.ToArray;
    finally
      FreeAndNil(DestinationIds);
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

function TRouteActions.Delete(RouteIds: TStringArray;
  out ErrorString: String): TStringArray;
var
  RouteIdsAsString: String;
  RouteId: String;
  Parameters: TGenericParameters;
  Response: TDeleteRouteResponse;
begin
  SetLength(Result, 0);

  RouteIdsAsString := EmptyStr;
  for RouteId in RouteIds do
  begin
    if (RouteIdsAsString.Length > 0) then
      RouteIdsAsString := RouteIdsAsString + ',';
    RouteIdsAsString := RouteIdsAsString + RouteId;
  end;
//  RouteIdsAsString := EncodeURL(RouteIdsAsString);

  Parameters := TGenericParameters.Create;
  try
      Parameters.AddParameter('route_id', RouteIdsAsString);
      Response := FConnection.Delete(TSettings.RouteHost,
        Parameters, TDeleteRouteResponse, ErrorString) as TDeleteRouteResponse;

      if (Response <> nil) then
        Result := Response.RouteIds;
  finally
    FreeAndNil(Parameters);
  end;
end;

function TRouteActions.Duplicate(QueryParameters: TRouteParametersQuery;
  out ErrorString: String): NullableString;
var
  Response: TDuplicateRouteResponse;
begin
  Result := NullableString.Null;

  QueryParameters.ReplaceParameter('to', 'none');
  // Redirect to page or return json for none

  Response := FConnection.Get(TSettings.DuplicateRoute,
    QueryParameters, TDuplicateRouteResponse, ErrorString) as TDuplicateRouteResponse;
  try
      if (Response <> nil) and (Response.Success) then
        Result := GetRouteId(Response.OptimizationProblemId, ErrorString);
  finally
    FreeAndNil(Response);
  end;
end;

function TRouteActions.Get(RouteParameters: TRouteParametersQuery;
  out ErrorString: String): TDataObjectRoute;
begin
  Result := FConnection.Get(TSettings.RouteHost,
    RouteParameters, TDataObjectRoute, ErrorString) as TDataObjectRoute;
end;

function TRouteActions.GetList(RouteParameters: TRouteParametersQuery;
  out ErrorString: String): TArray<TDataObjectRoute>;
var
  List: TDataObjectRouteList;
begin
  List := FConnection.Get(TSettings.RouteHost,
    RouteParameters, TDataObjectRouteList, ErrorString) as TDataObjectRouteList;
  try
    if (List <> nil) then
      Result := List.ToArray
    else
      SetLength(Result, 0);
  finally
    FreeAndNil(List);
  end;
end;

function TRouteActions.GetRouteId(OptimizationProblemId: NullableString;
  out ErrorString: String): NullableString;
var
  GenericParameters: TGenericParameters;
  Response: TDataObject;
begin
  Result := NullableString.Null;

  if OptimizationProblemId.IsNull then
    Exit;

  GenericParameters := TGenericParameters.Create();
  try
    GenericParameters.AddParameter('optimization_problem_id', OptimizationProblemId);
    GenericParameters.AddParameter('wait_for_final_state', '1');
    Response := FConnection.Get(TSettings.ApiHost,
      GenericParameters, TDataObject, ErrorString) as TDataObject;
    try
      if (Response <> nil) and (Length(Response.Routes) > 0) then
        Result := Response.Routes[0].RouteID;
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(GenericParameters);
  end;
end;

function TRouteActions.MoveDestinationToRoute(ToRouteId: String;
  RouteDestinationId, AfterDestinationId: integer;
  out ErrorString: String): boolean;
var
  Response: TMoveDestinationToRouteResponse;
  Request: TGenericParameters;
begin
  Request := TGenericParameters.Create;
  try
    Request.AddBodyParameter('to_route_id', ToRouteId);
    Request.AddBodyParameter('route_destination_id', IntToStr(RouteDestinationId));
    Request.AddBodyParameter('after_destination_id', IntToStr(AfterDestinationId));

    Response := FConnection.Post(TSettings.MoveRouteDestination,
        Request, TMoveDestinationToRouteResponse, ErrorString) as TMoveDestinationToRouteResponse;
    try
      if (Response <> nil) then
      begin
        if (not Response.Success) and (Response.Error <> EmptyStr) then
          ErrorString := Response.Error;
        Result := Response.Success;
      end
      else
        Result := False;
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

function TRouteActions.Remove(RouteId: String;
  DestinationId: integer; out ErrorString: String): boolean;
var
  Request: TRemoveRouteDestinationRequest;
  Response: TRemoveRouteDestinationResponse;
begin
  Request := TRemoveRouteDestinationRequest.Create;
  try
    Request.RouteId := RouteId;
    Request.RouteDestinationId := destinationId;

    Response := FConnection.Delete(TSettings.GetAddress,
      Request, TRemoveRouteDestinationResponse, ErrorString) as TRemoveRouteDestinationResponse;
    try
      Result := (Response <> nil) and (Response.Deleted);
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

function TRouteActions.Resequence(
  AddressesOrderInfo: TAddressesOrderInfo;
  out ErrorString: String): TDataObjectRoute;
begin
  Result := FConnection.Put(TSettings.RouteHost,
    AddressesOrderInfo, TDataObjectRoute, errorString) as TDataObjectRoute;
end;

procedure TRouteActions.Share(RouteId, RecipientEmail: String;
  out ErrorString: String);
var
  Request: TGenericParameters;
  Response: TStatusResponse;
begin
  Request := TGenericParameters.Create;
  try
    Request.AddParameter('route_id', RouteId);
    Request.AddParameter('response_format', TOptimizationParametersFormatDescription[opJson]);
    Request.AddBodyParameter('recipient_email', RecipientEmail);

    Response := FConnection.Post(TSettings.ShareRouteHost,
      Request, TStatusResponse, ErrorString) as TStatusResponse;
    try
      if (Response <> nil) and (Response.Status = False) and (ErrorString = EmptyStr) then
        ErrorString := 'Rote not shared';
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndnil(Request);
  end;
end;

function TRouteActions.Update(RouteParameters: TRouteParametersQuery;
  out ErrorString: String): TDataObjectRoute;
begin
  Result := FConnection.Put(TSettings.RouteHost,
    RouteParameters, TDataObjectRoute, errorString) as TDataObjectRoute;
end;

end.
