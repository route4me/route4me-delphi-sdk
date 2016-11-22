unit MarkAddressAsDetectedAsDepartedRequestUnit;

interface

uses
  REST.Json.Types, SysUtils,
  HttpQueryMemberAttributeUnit,
  GenericParametersUnit;

type
  TMarkAddressAsDetectedAsDepartedRequest = class(TGenericParameters)
  private
    [JSONMarshalled(False)]
    [HttpQueryMember('route_id')]
    FRouteId: String;

    [JSONMarshalled(False)]
    [HttpQueryMember('route_destination_id')]
    FRouteDestinationId: Integer;

    [JSONNameAttribute('is_departed')]
    FIsDeparted: boolean;
  public
    constructor Create(RouteId: String; RouteDestinationId: Integer; IsDeparted: boolean); reintroduce;
  end;

implementation

{ TMarkAddressAsDetectedAsDepartedRequest }

constructor TMarkAddressAsDetectedAsDepartedRequest.Create(RouteId: String;
  RouteDestinationId: Integer; IsDeparted: boolean);
begin
  Inherited Create;

  FRouteId := RouteId;
  FRouteDestinationId := RouteDestinationId;
  FIsDeparted := IsDeparted;
end;

end.
