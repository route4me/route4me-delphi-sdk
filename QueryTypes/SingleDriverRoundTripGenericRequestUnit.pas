unit SingleDriverRoundTripGenericRequestUnit;

interface

uses
  REST.Json.Types, HttpQueryMemberAttributeUnit,
  GenericParametersUnit, AddressUnit, RouteParametersUnit;

type
  // Inherit from GenericParameters and add any JSON serializable content
  TSingleDriverRoundTripGenericRequest = class(TGenericParameters)
  private
    [JSONNameAttribute('addresses')]
    FAddresses: TAddressesArray;

    [JSONNameAttribute('parameters')]
    FParameters: TRouteParameters;
  public
    // Using the defined "RouteParameters", can use user-defined class instead
    property Parameters: TRouteParameters read FParameters write FParameters;

    // Using the defined class "Address", can use user-defined class instead
    property Addresses: TAddressesArray read FAddresses write FAddresses;
  end;

implementation

end.
