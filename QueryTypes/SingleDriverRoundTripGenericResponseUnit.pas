unit SingleDriverRoundTripGenericResponseUnit;

interface

uses
  REST.Json.Types,
  AddressUnit, RouteParametersUnit;

type
  TSingleDriverRoundTripGenericResponse = class
  private
    [JSONNameAttribute('addresses')]
    FAddresses: TAddressesArray;

    [JSONNameAttribute('parameters')]
    FParameters: TRouteParameters;

    [JSONNameAttribute('optimization_problem_id')]
    FOptimizationProblemId: String;

    [JSONNameAttribute('state')]
    FMyState: integer;
  public
    // Using the defined "RouteParameters", can use user-defined class instead
    property Parameters: TRouteParameters read FParameters write FParameters;

    // Using the defined class "Address", can use user-defined class instead
    property Addresses: TAddressesArray read FAddresses write FAddresses;

    property OptimizationProblemId: String read FOptimizationProblemId write FOptimizationProblemId;
    property MyState: integer read FMyState write FMyState;
  end;

implementation

end.
