unit DuplicateRouteResponseUnit;

interface

uses
  REST.Json.Types, JSONNullableAttributeUnit,
  GenericParametersUnit, NullableBasicTypesUnit;

type
  TDuplicateRouteResponse = class(TGenericParameters)
  private
    [JSONName('optimization_problem_id')]
    [Nullable]
    FOptimizationProblemId: NullableString;

    [JSONName('success')]
    FSuccess: boolean;
  public
    constructor Create; override;

    property OptimizationProblemId: NullableString read FOptimizationProblemId write FOptimizationProblemId;
    property Success: boolean read FSuccess write FSuccess;
  end;

implementation

{ TDuplicateRouteResponse }

constructor TDuplicateRouteResponse.Create;
begin
  inherited;
  FOptimizationProblemId := NullableString.Null;
end;

end.
