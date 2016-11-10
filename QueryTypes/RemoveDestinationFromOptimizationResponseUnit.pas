unit RemoveDestinationFromOptimizationResponseUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit;

type
  TRemoveDestinationFromOptimizationResponse = class(TGenericParameters)
  private
    [JSONName('deleted')]
    FDeleted: boolean;

    [JSONName('route_destination_id')]
    FRouteDestinationId: integer;
  public
    property Deleted: boolean read FDeleted write FDeleted;
    property RouteDestinationId: integer read FRouteDestinationId write FRouteDestinationId;
  end;

implementation

end.
