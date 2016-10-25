unit Route4MeManagerStubUnit;

interface

uses
  IRoute4MeManagerUnit, OptimizationParametersUnit, DataObjectUnit,
  AddressBookContactActionsUnit, OptimizationActionUnit,
  RouteActionUnit, ConnectionUnit;

type
  TRoute4MeManagerStub = class(TInterfacedObject, IRoute4MeManager)
  public
    function Optimization: TOptimizationActions;
    function Route: TRouteActions;
    function AddressBookContact: TAddressBookContactActions;

    procedure SetConnectionProxy(Host: String; Port: integer; Username, Password: String);
    function Connection: TConnection;
  end;

implementation

{ TRoute4MeManagerStub }

function TRoute4MeManagerStub.AddressBookContact: TAddressBookContactActions;
begin

end;

function TRoute4MeManagerStub.Connection: TConnection;
begin

end;

function TRoute4MeManagerStub.Optimization: TOptimizationActions;
begin

end;

function TRoute4MeManagerStub.Route: TRouteActions;
begin

end;

procedure TRoute4MeManagerStub.SetConnectionProxy(Host: String; Port: integer;
  Username, Password: String);
begin

end;

end.
