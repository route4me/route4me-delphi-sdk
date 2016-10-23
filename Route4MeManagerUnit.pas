unit Route4MeManagerUnit;

interface

uses
  Classes, SysUtils,
  OptimizationParametersUnit, DataObjectUnit, IRoute4MeManagerUnit,
  AddressBookContactUnit, AddressBookContactActionsUnit, ConnectionUnit,
  OptimizationActionUnit, RouteActionUnit;

type
  TRoute4MeManager = class(TInterfacedObject, IRoute4MeManager)
  private
    FApiKey: String;
    FConnection: TConnection;

    FAddressBookContact: TAddressBookContactActions;
    FOptimization: TOptimizationActions;
    FRoute: TRouteActions;
  public
    constructor Create(ApiKey: String);
    destructor Destroy; override;

    procedure SetConnectionProxy(Host: String; Port: integer; Username, Password: String);

    function Optimization: TOptimizationActions;
    function Route: TRouteActions;
    function AddressBookContact: TAddressBookContactActions;
  end;

implementation

{ TRoute4MeManager }

uses EnumsUnit;

constructor TRoute4MeManager.Create(ApiKey: String);
begin
  FApiKey := ApiKey;

  FAddressBookContact := nil;
  FOptimization := nil;
  FRoute := nil;

  FConnection := TConnection.Create(FApiKey);
end;

destructor TRoute4MeManager.Destroy;
begin
  if (FRoute <> nil) then
    FreeAndNil(FRoute);
  if (FAddressBookContact <> nil) then
    FreeAndNil(FAddressBookContact);
  if (FOptimization <> nil) then
    FreeAndNil(FOptimization);

  FConnection.Free;
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
