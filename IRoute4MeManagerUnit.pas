unit IRoute4MeManagerUnit;

interface

uses
  OptimizationParametersUnit, DataObjectUnit,
  AddressBookContactActionsUnit,
  OptimizationActionUnit, RouteActionUnit, IConnectionUnit, UserActionUnit;

type
  IRoute4MeManager = interface
    ['{2E31D4E6-C42A-4C9B-9ED5-445C3F6D6690}']
    function Optimization: TOptimizationActions;
    function Route: TRouteActions;
    function AddressBookContact: TAddressBookContactActions;
    function User: TUserActions;

    procedure SetConnectionProxy(Host: String; Port: integer; Username, Password: String);
    function Connection: IConnection;
  end;

implementation

end.
