unit SingleDriverRoundTripGenericRequestUnit;

interface

uses
  REST.Json.Types,
  AddressUnit, RouteParametersUnit, GenericParametersUnit;

type
  TSingleDriverRoundTripGenericRequest = class(TGenericParameters)
  private
    [JSONNameAttribute('addresses')]
    FAddresses: TAddressesArray;

    [JSONNameAttribute('parameters')]
    FParameters: TRouteParameters;
  public
    property Parameters: TRouteParameters read FParameters write FParameters;
    property Addresses: TAddressesArray read FAddresses write FAddresses;
  end;

implementation

end.
