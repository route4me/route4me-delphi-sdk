unit RouteActionUnit;

interface

uses
  SysUtils, BaseActionUnit,
  DataObjectUnit, RouteParametersUnit, AddressUnit,
  AddressesOrderInfoUnit, RouteParametersQueryUnit;

type
  TRouteActions = class(TBaseAction)
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

    /// <summary>
    /// Add address(es) into a route.
    /// </summary>
    /// <param name="RouteId"> Route ID </param>
    /// <param name="Addresses"> Valid array of Address objects. </param>
    /// <param name="ErrorString"> out: Error as string </param>
    /// <returns> IDs of added addresses </returns>
    function Add(RouteId: String; Addresses: TAddressesArray;
      out ErrorString: String): TArray<integer>; overload;

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
  end;

implementation

{ TRouteActions }

uses
  System.Generics.Collections,
  SettingsUnit, RemoveRouteDestinationResponseUnit,
  RemoveRouteDestinationRequestUnit, AddRouteDestinationRequestUnit,
  MoveDestinationToRouteResponseUnit,
  GenericParametersUnit, CommonTypesUnit;

function TRouteActions.Add(RouteId: String; Addresses: TAddressesArray;
  OptimalPosition: boolean; out ErrorString: String): TArray<integer>;
var
  Request: TAddRouteDestinationRequest;
  Responce: TDataObject;
  Address, AddressResponce: TAddress;
  DestinationIds: TList<integer>;
begin
  Result := TArray<integer>.Create();

  Request := TAddRouteDestinationRequest.Create;
  try
    Request.RouteId := RouteId;
    Request.Addresses := Addresses;
    Request.OptimalPosition := OptimalPosition;

    Responce := FConnection.Put(TSettings.RouteHost,
      Request, TDataObject, ErrorString) as TDataObject;

    if (Responce = nil) then
      Exit;

    DestinationIds := TList<integer>.Create;
    try
      for Address in Addresses do
        for AddressResponce in Responce.Addresses do
          if (AddressResponce.AddressString = Address.AddressString) and
            (AddressResponce.Latitude = Address.Latitude) and
            (AddressResponce.Longitude = Address.Longitude) and
            (AddressResponce.RouteDestinationId.IsNotNull) then
          begin
            DestinationIds.Add(AddressResponce.RouteDestinationId);
            Break;
          end;
      Result := DestinationIds.ToArray;
    finally
      FreeAndNil(DestinationIds);
      FreeAndNil(Responce);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

function TRouteActions.Add(RouteId: String; Addresses: TAddressesArray;
  out ErrorString: String): TArray<integer>;
begin
  Result := Add(RouteId, Addresses, True, ErrorString);
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

function TRouteActions.MoveDestinationToRoute(ToRouteId: String;
  RouteDestinationId, AfterDestinationId: integer;
  out ErrorString: String): boolean;
var
  Response: TMoveDestinationToRouteResponse;
  Request: TGenericParameters;
begin
  Request := TGenericParameters.Create;
  try
    Request.AddParameter('to_route_id', ToRouteId);
    Request.AddParameter('route_destination_id', IntToStr(RouteDestinationId));
    Request.AddParameter('after_destination_id', IntToStr(AfterDestinationId));

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

function TRouteActions.Update(RouteParameters: TRouteParametersQuery;
  out ErrorString: String): TDataObjectRoute;
begin
  Result := FConnection.Put(TSettings.RouteHost,
    RouteParameters, TDataObjectRoute, errorString) as TDataObjectRoute;
end;

end.
