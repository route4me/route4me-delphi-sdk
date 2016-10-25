unit RouteActionUnit;

interface

uses
  SysUtils, BaseActionUnit,
  DataObjectUnit, RouteParametersUnit, AddressUnit,
  AddressesOrderInfoUnit;

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

    function MoveDestinationToRoute(ToRouteId: String;
      RouteDestinationId, AfterDestinationId: integer; out ErrorString: String): boolean;
  end;

implementation

{ TRouteActions }

uses
  System.Generics.Collections,
  SettingsUnit, RemoveRouteDestinationResponseUnit,
  RemoveRouteDestinationRequestUnit, AddRouteDestinationRequestUnit,
  MoveDestinationToRouteResponseUnit, MoveDestinationToRouteRequestUnit;

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

function TRouteActions.MoveDestinationToRoute(ToRouteId: String;
  RouteDestinationId, AfterDestinationId: integer;
  out ErrorString: String): boolean;
var
  Response: TMoveDestinationToRouteResponse;
  Request: TMoveDestinationToRouteRequest;
begin
  Request := TMoveDestinationToRouteRequest.Create;
  try
    Request.ToRouteId := ToRouteId;
    Request.RouteDestinationId := RouteDestinationId;
    Request.AfterDestinationId := AfterDestinationId;

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

end.
