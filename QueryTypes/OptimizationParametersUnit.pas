unit OptimizationParametersUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections,
  JSONNullableAttributeUnit,
  GenericParametersUnit, RouteParametersUnit, AddressUnit,
  NullableBasicTypesUnit;

type
  TOptimizationParameters = class(TGenericParameters)
  private
    [JSONMarshalled(False)]
    FOptimizationProblemID: String;

    [JSONMarshalled(False)]
    FReOptimize: boolean;

    [JSONMarshalled(False)]
    FShowDirections: boolean;

    [JSONNameAttribute('addresses')]
    FAddresses: TArray<TAddress>;

    [JSONNameAttribute('parameters')]
    FParameters: TRouteParameters;
  public
//    [HttpQueryMemberAttribute(Name = 'optimization_problem_id', EmitDefaultValue = false)]
    property OptimizationProblemID: String read FOptimizationProblemID write FOptimizationProblemID;

//    [HttpQueryMemberAttribute(Name = 'reoptimize', EmitDefaultValue = false)]
    property ReOptimize: boolean read FReOptimize write FReOptimize;

//    [HttpQueryMemberAttribute(Name = 'show_directions', EmitDefaultValue = false)]
    property ShowDirections: boolean read FShowDirections write FShowDirections;

//    [DataMember(Name = 'parameters', EmitDefaultValue = false)]
    property Parameters: TRouteParameters read FParameters write FParameters;

//    [DataMember(Name = 'addresses', EmitDefaultValue = false)]
    property Addresses: TArray<TAddress> read FAddresses write FAddresses;

  end;

implementation

end.
