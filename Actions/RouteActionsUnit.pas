unit RouteActionsUnit;

interface

uses
  SysUtils, BaseActionUnit,
  DataObjectUnit, RouteParametersUnit, AddressUnit,
  AddressesOrderInfoUnit, RouteParametersQueryUnit, AddOrderToRouteRequestUnit,
  CommonTypesUnit, NullableBasicTypesUnit, EnumsUnit;

type
  TRouteActions = class(TBaseAction)
  private
    function GetRouteId(OptimizationProblemId: NullableString;
      out ErrorString: String): NullableString;
  public
    function Resequence(AddressesOrderInfo: TAddressesOrderInfo;
      out ErrorString: String): TDataObjectRoute;

    procedure ResequenceAll(RouteId: String; DisableOptimization: boolean;
      WhatOptimize: TOptimize; out ErrorString: String);

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

    procedure UpdateCustomFields(RouteId: String; RouteDestinationId: integer;
      CustomFields: TListStringPair; out ErrorString: String);

    function MoveDestinationToRoute(ToRouteId: String;
      RouteDestinationId, AfterDestinationId: integer; out ErrorString: String): boolean;

    function Get(RouteParameters: TRouteParametersQuery;
      out ErrorString: String): TDataObjectRoute; overload;

    function Get(RouteId: String; RoutePathOutput: TRoutePathOutput;
      out ErrorString: String): TDataObjectRoute; overload;

    function GetList(Limit, Offset: integer;
      out ErrorString: String): TDataObjectRouteList;

    function Delete(RouteIds: TStringArray;
      out ErrorString: String): TStringArray;

    function Duplicate(QueryParameters: TRouteParametersQuery;
      out ErrorString: String): NullableString;

    /// <summary>
    ///  Share a route via email.
    /// </summary>
    procedure Share(RouteId: String; RecipientEmail: String; out ErrorString: String);

    procedure Merge(RouteIds: TStringArray; out ErrorString: String);

    /// <summary>
    ///  Insert an existing order into an existing route.
    /// </summary>
    function AddOrder(Parameters: TAddOrderToRouteRequest;
      out ErrorString: String): TDataObjectRoute;
  end;

implementation

{ TRouteActions }

uses
  System.Generics.Collections,
  SettingsUnit, RemoveRouteDestinationResponseUnit,
  RemoveRouteDestinationRequestUnit, AddRouteDestinationRequestUnit,
  MoveDestinationToRouteResponseUnit,
  GenericParametersUnit, DeleteRouteResponseUnit, DuplicateRouteResponseUnit,
  StatusResponseUnit, MergeRouteRequestUnit, UpdateRoutesCustomDataRequestUnit,
  ErrorResponseUnit, ResequenceAllRoutesRequestUnit;

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

    Response := FConnection.Put(TSettings.EndPoints.Route,
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

function TRouteActions.AddOrder(Parameters: TAddOrderToRouteRequest;
  out ErrorString: String): TDataObjectRoute;
begin
  Result := FConnection.Put(TSettings.EndPoints.Route,
    Parameters, TDataObjectRoute, ErrorString) as TDataObjectRoute;

  if (Result = nil) and (ErrorString = EmptyStr) then
    ErrorString := 'Order to a Route not added';
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

  Parameters := TGenericParameters.Create;
  try
      Parameters.AddParameter('route_id', RouteIdsAsString);
      Response := FConnection.Delete(TSettings.EndPoints.Route,
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

  Response := FConnection.Get(TSettings.EndPoints.DuplicateRoute,
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
  Result := FConnection.Get(TSettings.EndPoints.Route,
    RouteParameters, TDataObjectRoute, ErrorString) as TDataObjectRoute;
end;

function TRouteActions.Get(RouteId: String; RoutePathOutput: TRoutePathOutput;
  out ErrorString: String): TDataObjectRoute;
var
  RouteParameters: TRouteParametersQuery;
begin
  RouteParameters := TRouteParametersQuery.Create;
  try
    RouteParameters.RouteId := RouteId;
    RouteParameters.RoutePathOutput := RoutePathOutput;

    Result := Get(RouteParameters, ErrorString);
  finally
    FreeAndNil(RouteParameters);
  end;
end;

function TRouteActions.GetList(Limit, Offset: integer;
  out ErrorString: String): TDataObjectRouteList;
var
  RouteParameters: TRouteParametersQuery;
begin
  RouteParameters := TRouteParametersQuery.Create;
  try
    RouteParameters.Limit := Limit;
    RouteParameters.Offset := Offset;

    Result := FConnection.Get(TSettings.EndPoints.Route,
      RouteParameters, TDataObjectRouteList, ErrorString) as TDataObjectRouteList;
  finally
    FreeAndNil(RouteParameters);
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
    Response := FConnection.Get(TSettings.EndPoints.Optimization,
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

procedure TRouteActions.Merge(RouteIds: TStringArray; out ErrorString: String);
var
  Request: TMergeRouteRequest;
  Response: TStatusResponse;
begin
  Request := TMergeRouteRequest.Create;
  try
    Request.RouteIds := RouteIds;

    Response := FConnection.Post(TSettings.EndPoints.MergeRouteEndPoint,
      Request, TStatusResponse, ErrorString) as TStatusResponse;
    try
      if (Response <> nil) and (Response.Status = False) and (ErrorString = EmptyStr) then
        ErrorString := 'Rotes not merged';
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndnil(Request);
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

    Response := FConnection.Post(TSettings.EndPoints.MoveRouteDestination,
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

    Response := FConnection.Delete(TSettings.EndPoints.GetAddress,
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

function TRouteActions.Resequence(AddressesOrderInfo: TAddressesOrderInfo;
  out ErrorString: String): TDataObjectRoute;
begin
  Result := FConnection.Put(TSettings.EndPoints.Route,
    AddressesOrderInfo, TDataObjectRoute, ErrorString) as TDataObjectRoute;

  if (Result = nil) and (ErrorString = EmptyStr) then
    ErrorString := 'Route not re-sequenced';
end;

procedure TRouteActions.ResequenceAll(RouteId: String;
  DisableOptimization: boolean; WhatOptimize: TOptimize;
  out ErrorString: String);
var
  Request: TResequenceAllRoutesRequest;
  Response: TStatusResponse;
begin
  Request := TResequenceAllRoutesRequest.Create(RouteId, DisableOptimization, WhatOptimize);
  try
    Response := FConnection.Get(TSettings.EndPoints.ResequenceRoute,
      Request, TStatusResponse, ErrorString) as TStatusResponse;
    try
      if (Response <> nil) and (Response.Status = False) and (ErrorString = EmptyStr) then
        ErrorString := 'Routes not resequenced';
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
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

    Response := FConnection.Post(TSettings.EndPoints.ShareRoute,
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

procedure TRouteActions.UpdateCustomFields(RouteId: String; RouteDestinationId: integer;
  CustomFields: TListStringPair; out ErrorString: String);
var
  Request: TUpdateRoutesCustomDataRequest;
  Pair: TStringPair;
  Address: TAddress;
begin
  Request := TUpdateRoutesCustomDataRequest.Create;
  try
    Request.RouteId := RouteId;
    Request.RouteDestinationId := RouteDestinationId;
    for Pair in CustomFields do
      Request.AddCustomField(Pair.Key, Pair.Value);

    Address := FConnection.Put(TSettings.EndPoints.GetAddress, Request,
      TAddress, ErrorString) as TAddress;
    try
      if (Address = nil) and (ErrorString = EmptyStr) then
        ErrorString := 'Custom data of the route destinations not updated';
    finally
      FreeAndNil(Address);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

function TRouteActions.Update(RouteParameters: TRouteParametersQuery;
  out ErrorString: String): TDataObjectRoute;
begin
  Result := FConnection.Put(TSettings.EndPoints.Route,
    RouteParameters, TDataObjectRoute, errorString) as TDataObjectRoute;

  if (Result = nil) and (ErrorString = EmptyStr) then
    ErrorString := 'Route not updated';
end;

end.
