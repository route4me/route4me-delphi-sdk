unit BaseOptimizationParametersProviderUnit;

interface

uses IOptimizationParametersProviderUnit, OptimizationParametersUnit,
  AddressUnit, RouteParametersUnit;

type
  TBaseOptimizationParametersProvider = class abstract (TInterfacedObject, IOptimizationParametersProvider)
  protected
    procedure AddAddress(Address: TAddress; var AddressArray: TArray<TAddress>);

    function MakeAddresses(): TArray<TAddress>; virtual; abstract;
    function MakeRouteParameters(): TRouteParameters; virtual; abstract;
  public
    function OptimizationParameters: TOptimizationParameters;

  end;
implementation

{ TBaseOptimizationParametersProvider }

procedure TBaseOptimizationParametersProvider.AddAddress(Address: TAddress;
  var AddressArray: TArray<TAddress>);
begin
    SetLength(AddressArray, Length(AddressArray) + 1);
    AddressArray[High(AddressArray)] := Address;
end;

function TBaseOptimizationParametersProvider.OptimizationParameters: TOptimizationParameters;
begin
  Result := TOptimizationParameters.Create;
  Result.Parameters := MakeRouteParameters;
  Result.Addresses := MakeAddresses;
end;

end.
