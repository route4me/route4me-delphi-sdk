unit Route4MeManagerStubUnit;

interface

uses IRoute4MeManagerUnit, OptimizationParametersUnit, DataObjectUnit,
  AddressBookContactActionsUnit, OptimizationActionUnit;

type
  TRoute4MeManagerStub = class(TInterfacedObject, IRoute4MeManager)
  public
    function AddressBookContact: TAddressBookContactActions;
    function Optimization: TOptimizationActions;
  end;

implementation

{ TRoute4MeManagerStub }

function TRoute4MeManagerStub.AddressBookContact: TAddressBookContactActions;
begin

end;

function TRoute4MeManagerStub.Optimization: TOptimizationActions;
begin

end;

end.
