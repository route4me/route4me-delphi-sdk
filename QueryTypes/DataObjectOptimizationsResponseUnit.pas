unit DataObjectOptimizationsResponseUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit, DataObjectUnit;

type
  TDataObjectOptimizationsResponse = class(TGenericParameters)
  private
    [JSONName('optimizations')]
    FOptimizations: TArray<TDataObject>;
  public
    property Optimizations: TArray<TDataObject> read FOptimizations write FOptimizations;
  end;

implementation

end.
