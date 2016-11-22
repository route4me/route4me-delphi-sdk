unit MarkAddressAsVisitedRequestUnit;

interface

uses
  REST.Json.Types, SysUtils,
  HttpQueryMemberAttributeUnit,
  GenericParametersUnit;

type
  TMarkAddressAsVisitedRequest = class(TGenericParameters)
  private
    [JSONMarshalled(False)]
    [HttpQueryMember('route_id')]
    FRouteId: String;

    [JSONMarshalled(False)]
    [HttpQueryMember('route_destination_id')]
    FRouteDestinationId: Integer;

    [JSONNameAttribute('is_visited')]
    FIsVisited: boolean;
  public
    constructor Create(RouteId: String; RouteDestinationId: Integer; IsVisited: boolean); reintroduce;
  end;

implementation

{ TMarkAddressAsVisitedRequest }

constructor TMarkAddressAsVisitedRequest.Create(RouteId: String;
  RouteDestinationId: Integer; IsVisited: boolean);
begin
  Inherited Create;

  FRouteId := RouteId;
  FRouteDestinationId := RouteDestinationId;
  FIsVisited := IsVisited;
end;

end.
