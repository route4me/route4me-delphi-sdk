unit MoveDestinationToRouteRequestUnit;

interface

uses
  REST.Json.Types, HttpQueryMemberAttributeUnit,
  GenericParametersUnit;

type
  TMoveDestinationToRouteRequest = class(TGenericParameters)
  private
    [JSONMarshalled(False)]
    [HttpQueryMember('to_route_id')]
    FToRouteId: String;

    [JSONMarshalled(False)]
    [HttpQueryMember('route_destination_id')]
    FRouteDestinationId: integer;

    [JSONMarshalled(False)]
    [HttpQueryMember('after_destination_id')]
    FAfterDestinationId: integer;
  public
    property ToRouteId: String read FToRouteId write FToRouteId;
    property RouteDestinationId: integer read FRouteDestinationId write FRouteDestinationId;
    property AfterDestinationId: integer read FAfterDestinationId write FAfterDestinationId;
  end;

implementation

end.
