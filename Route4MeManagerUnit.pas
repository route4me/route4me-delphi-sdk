unit Route4MeManagerUnit;

interface

uses
  Classes, SysUtils,
  OptimizationParametersUnit, DataObjectUnit, IRoute4MeManagerUnit,
  AddressBookContactUnit, AddressBookContactActionsUnit,
  OptimizationActionUnit, RouteActionUnit, IConnectionUnit, UserActionUnit,
  AddressNoteActionUnit, AddressActionUnit, AvoidanceZoneUnit,
  AvoidanceZoneActionUnit, OrderActionUnit, ActivityActionsUnit,
  TrackingActionsUnit;

type
  TRoute4MeManager = class(TInterfacedObject, IRoute4MeManager)
  private
    FConnection: IConnection;

    FAddressBookContact: TAddressBookContactActions;
    FOptimization: TOptimizationActions;
    FRoute: TRouteActions;
    FUser: TUserActions;
    FAddressNote: TAddressNoteActions;
    FAddress: TAddressActions;
    FAvoidanceZone: TAvoidanceZoneActions;
    FOrder: TOrderActions;
    FActivity: TActivityActions;
    FTracking: TTrackingActions;
  public
    constructor Create(Connection: IConnection);
    destructor Destroy; override;

    procedure SetConnectionProxy(Host: String; Port: integer; Username, Password: String);

    function Optimization: TOptimizationActions;
    function Route: TRouteActions;
    function AddressBookContact: TAddressBookContactActions;
    function User: TUserActions;
    function AddressNote: TAddressNoteActions;
    function Address: TAddressActions;
    function AvoidanceZone: TAvoidanceZoneActions;
    function Order: TOrderActions;
    function Activity: TActivityActions;
    function Tracking: TTrackingActions;

    function Connection: IConnection;
  end;

implementation

{ TRoute4MeManager }

uses EnumsUnit;

function TRoute4MeManager.AddressNote: TAddressNoteActions;
begin
  if (FAddressNote = nil) then
    FAddressNote := TAddressNoteActions.Create(FConnection, Self);
  Result := FAddressNote;
end;

function TRoute4MeManager.AvoidanceZone: TAvoidanceZoneActions;
begin
  if (FAvoidanceZone = nil) then
    FAvoidanceZone := TAvoidanceZoneActions.Create(FConnection);
  Result := FAvoidanceZone;
end;

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
  FUser := nil;
  FAddressNote := nil;
  FAddress := nil;
  FAvoidanceZone := nil;
  FOrder := nil;
  FActivity := nil;
  FTracking := nil;
end;

destructor TRoute4MeManager.Destroy;
begin
  FreeAndNil(FTracking);
  FreeAndNil(FActivity);
  FreeAndNil(FOrder);
  FreeAndNil(FAvoidanceZone);
  FreeAndNil(FAddress);
  FreeAndNil(FAddressNote);
  FreeAndNil(FUser);
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

function TRoute4MeManager.Order: TOrderActions;
begin
  if (FOrder = nil) then
    FOrder := TOrderActions.Create(FConnection);
  Result := FOrder;
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

function TRoute4MeManager.Tracking: TTrackingActions;
begin
  if (FTracking = nil) then
    FTracking := TTrackingActions.Create(FConnection);
  Result := FTracking;
end;

function TRoute4MeManager.User: TUserActions;
begin
  if (FUser = nil) then
    FUser := TUserActions.Create(FConnection);
  Result := FUser;
end;

function TRoute4MeManager.Activity: TActivityActions;
begin
  if (FActivity = nil) then
    FActivity := TActivityActions.Create(FConnection);
  Result := FActivity;
end;

function TRoute4MeManager.Address: TAddressActions;
begin
  if (FAddress = nil) then
    FAddress := TAddressActions.Create(FConnection);
  Result := FAddress;
end;

function TRoute4MeManager.AddressBookContact: TAddressBookContactActions;
begin
  if (FAddressBookContact = nil) then
    FAddressBookContact := TAddressBookContactActions.Create(FConnection);
  Result := FAddressBookContact;
end;

end.
