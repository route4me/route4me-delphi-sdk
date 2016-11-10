unit RemoveRouteDestinationRequestUnit;

interface

uses
  REST.Json.Types, HttpQueryMemberAttributeUnit,
  GenericParametersUnit;

type
  TRemoveRouteDestinationRequest = class(TGenericParameters)
  private
    [JSONMarshalled(False)]
    [HttpQueryMember('route_id')]
    FRouteId: String;

    [JSONMarshalled(False)]
    [HttpQueryMember('route_destination_id')]
    FRouteDestinationId: integer;
  public
    property RouteId: String read FRouteId write FRouteId;
    property RouteDestinationId: integer read FRouteDestinationId write FRouteDestinationId;
  end;

implementation

end.
