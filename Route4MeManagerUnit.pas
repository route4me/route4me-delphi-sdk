unit Route4MeManagerUnit;

interface

uses
  Classes, SysUtils,
  OptimizationParametersUnit, DataObjectUnit, IRoute4MeManagerUnit,
  AddressBookContactUnit, AddressBookContactActionsUnit, ConnectionUnit,
  OptimizationActionUnit, RouteActionUnit, IConnectionUnit;

type
  TRoute4MeManager = class(TInterfacedObject, IRoute4MeManager)
  private
    FConnection: IConnection;

    FAddressBookContact: TAddressBookContactActions;
    FOptimization: TOptimizationActions;
    FRoute: TRouteActions;
  public
    constructor Create(Connection: IConnection);
    destructor Destroy; override;

    procedure SetConnectionProxy(Host: String; Port: integer; Username, Password: String);

    function Optimization: TOptimizationActions;
    function Route: TRouteActions;
    function AddressBookContact: TAddressBookContactActions;

    function Connection: IConnection;
  end;

implementation

{ TRoute4MeManager }

uses EnumsUnit;

function TRoute4MeManager.Connection: IConnection;
begin
  Result := FConnection;
end;

constructor TRoute4MeManager.Create(Connection: IConnection);
begin
  FConnection := Connection;

  FAddressBookContact := nil;
  FOptimization := nil;
  FRoute := nil;
end;

destructor TRoute4MeManager.Destroy;
begin
  FreeAndNil(FRoute);
  FreeAndNil(FAddressBookContact);
  FreeAndNil(FOptimization);

  FConnection := nil;
  inherited;
end;

function TRoute4MeManager.Optimization: TOptimizationActions;
begin
  if (FOptimization = nil) then
    FOptimization := TOptimizationActions.Create(FConnection);
  Result := FOptimization;
end;

function TRoute4MeManager.Route: TRouteActions;
begin
  if (FRoute = nil) then
    FRoute := TRouteActions.Create(FConnection);
  Result := FRoute;
end;

procedure TRoute4MeManager.SetConnectionProxy(Host: String; Port: integer; Username, Password: String);
begin
  FConnection.SetProxy(Host, Port, Username, Password);
end;

function TRoute4MeManager.AddressBookContact: TAddressBookContactActions;
begin
  if (FAddressBookContact = nil) then
    FAddressBookContact := TAddressBookContactActions.Create(FConnection);
  Result := FAddressBookContact;
end;

end.
