unit BaseOptimizationParametersProviderUnit;

interface

uses
  IOptimizationParametersProviderUnit, OptimizationParametersUnit,
  AddressUnit, RouteParametersUnit;

type
  TBaseOptimizationParametersProvider = class abstract (TInterfacedObject, IOptimizationParametersProvider)
  protected
    procedure AddAddress(Address: TAddress; var AddressArray: TAddressesArray);

    function MakeAddresses(): TAddressesArray; virtual; abstract;
    function MakeRouteParameters(): TRouteParameters; virtual; abstract;

    /// <summary>
    ///  After responce some fields are changed from request.
    /// </summary>
    procedure CorrectForResponce(OptimizationParameters: TOptimizationParameters); virtual; abstract;
  public
    function OptimizationParameters: TOptimizationParameters;
    function OptimizationParametersForResponce: TOptimizationParameters;
  end;
implementation

{ TBaseOptimizationParametersProvider }

procedure TBaseOptimizationParametersProvider.AddAddress(Address: TAddress;
  var AddressArray: TAddressesArray);
begin
    SetLength(AddressArray, Length(AddressArray) + 1);
    AddressArray[High(AddressArray)] := Address;
end;

function TBaseOptimizationParametersProvider.OptimizationParameters: TOptimizationParameters;
var
  Address: TAddress;
begin
  Result := TOptimizationParameters.Create;
  Result.Parameters := MakeRouteParameters;
  for Address in MakeAddresses do
    Result.AddAddress(Address);
end;

function TBaseOptimizationParametersProvider.OptimizationParametersForResponce: TOptimizationParameters;
begin
  Result := OptimizationParameters;
  CorrectForResponce(Result);
end;

end.
