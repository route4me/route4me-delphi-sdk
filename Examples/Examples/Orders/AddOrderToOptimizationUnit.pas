unit AddOrderToOptimizationUnit;

interface

uses SysUtils, BaseExampleUnit, RouteParametersUnit,
  AddressUnit;

type
  TAddOrderToOptimization = class(TBaseExample)
  public
    procedure Execute(OptimizationId: String; RouteParameters: TRouteParameters;
      OrderedAddresses: TOrderedAddressArray);
  end;

implementation

uses AddOrderToOptimizationRequestUnit;

procedure TAddOrderToOptimization.Execute(OptimizationId: String;
  RouteParameters: TRouteParameters; OrderedAddresses: TOrderedAddressArray);
var
  ErrorString: String;
  Parameters: TAddOrderToOptimizationRequest;
  Address: TOrderedAddress;
begin
  Parameters := TAddOrderToOptimizationRequest.Create;
  try
    Parameters.OptimizationProblemId := OptimizationId;
    Parameters.Redirect := False;
    Parameters.Parameters := RouteParameters;
    for Address in OrderedAddresses do
      Parameters.AddAddress(Address);

    Route4MeManager.Optimization.AddOrder(Parameters, ErrorString);

    WriteLn('');

    if (ErrorString = EmptyStr) then
      WriteLn('AddOrderToOptimization executed successfully')
    else
      WriteLn(Format('AddOrderToOptimization error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
