unit Route4MeManagerStubUnit;

interface

uses IRoute4MeManagerUnit, OptimizationParametersUnit, DataObjectUnit,
  AddressBookContactActionsUnit;

type
  TRoute4MeManagerStub = class(TInterfacedObject, IRoute4MeManager)
  public
    function RunOptimization(optimizationParameters: TOptimizationParameters; out errorString: String): TDataObject;
    function AddressBookContact: TAddressBookContactActions;
  end;

implementation

{ TRoute4MeManagerStub }

function TRoute4MeManagerStub.AddressBookContact: TAddressBookContactActions;
begin

end;

function TRoute4MeManagerStub.RunOptimization(
  optimizationParameters: TOptimizationParameters;
  out errorString: String): TDataObject;
begin

end;

end.
