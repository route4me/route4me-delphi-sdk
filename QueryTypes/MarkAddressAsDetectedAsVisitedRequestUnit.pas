unit MarkAddressAsDetectedAsVisitedRequestUnit;

interface

uses
  REST.Json.Types, SysUtils,
  HttpQueryMemberAttributeUnit,
  GenericParametersUnit;

type
  TMarkAddressAsDetectedAsVisitedRequest = class(TGenericParameters)
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

{ TMarkAddressAsDetectedAsVisitedRequest }

constructor TMarkAddressAsDetectedAsVisitedRequest.Create(RouteId: String;
  RouteDestinationId: Integer; IsVisited: boolean);
begin
  Inherited Create;

  FRouteId := RouteId;
  FRouteDestinationId := RouteDestinationId;
  FIsVisited := IsVisited;
end;

end.
