unit AddRouteDestinationRequestUnit;

interface

uses
  REST.Json.Types, HttpQueryMemberAttributeUnit,
  GenericParametersUnit, AddressUnit;

type
  TAddRouteDestinationRequest = class(TGenericParameters)
  private
    [JSONMarshalled(False)]
    [HttpQueryMember('route_id')]
    FRouteId: String;

    [JSONName('addresses')]
    FAddresses: TAddressesArray;

    [JSONName('optimal_position')]
    FOptimalPosition: boolean;
  public
    property RouteId: String read FRouteId write FRouteId;
    property Addresses: TAddressesArray read FAddresses write FAddresses;

      /// <summary>
      /// If true, an address will be inserted at optimal position of a route
      /// </summary>
    property OptimalPosition: boolean read FOptimalPosition write FOptimalPosition;
  end;

implementation

end.
