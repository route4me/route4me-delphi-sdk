unit Route4MeManagerUnit;

interface

uses
  Classes, SysUtils,
  OptimizationParametersUnit, DataObjectUnit, IRoute4MeManagerUnit,
  AddressBookContactUnit, AddressBookContactActionsUnit,
  OptimizationActionsUnit, RouteActionsUnit, IConnectionUnit, UserActionsUnit,
  AddressNoteActionsUnit, AddressActionsUnit, AvoidanceZoneUnit,
  AvoidanceZoneActionsUnit, OrderActionsUnit, ActivityActionsUnit,
  TrackingActionsUnit, GeocodingActionsUnit, TerritoryActionsUnit,
  VehicleActionsUnit, FileUploadingActionsUnit, TelematicActionsUnit;

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
    FGeocoding: TGeocodingActions;
    FOrder: TOrderActions;
    FActivity: TActivityActions;
    FTracking: TTrackingActions;
    FTerritory: TTerritoryActions;
    FVehicle: TVehicleActions;
    FUploading: TFileUploadingActions;
    FTelematics: TTelematicActions;
  public
    constructor Create(Connection: IConnection);
    destructor Destroy; override;

    procedure Clear;

    procedure SetConnectionProxy(Host: String; Port: integer; Username, Password: String);

    function Optimization: TOptimizationActions;
    function Route: TRouteActions;
    function AddressBookContact: TAddressBookContactActions;
    function User: TUserActions;
    function AddressNote: TAddressNoteActions;
    function Address: TAddressActions;
    function AvoidanceZone: TAvoidanceZoneActions;
    function Geocoding: TGeocodingActions;
    function Order: TOrderActions;
    function ActivityFeed: TActivityActions;
    function Tracking: TTrackingActions;
    function Territory: TTerritoryActions;
    function Vehicle: TVehicleActions;
    function Uploading: TFileUploadingActions;
    function Telematics: TTelematicActions;

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

procedure TRoute4MeManager.Clear;
begin
  FreeAndNil(FTelematics);
  FreeAndNil(FUploading);
  FreeAndNil(FVehicle);
  FreeAndNil(FTerritory);
  FreeAndNil(FTracking);
  FreeAndNil(FActivity);
  FreeAndNil(FOrder);
  FreeAndNil(FAvoidanceZone);
  FreeAndNil(FGeocoding);
  FreeAndNil(FAddress);
  FreeAndNil(FAddressNote);
  FreeAndNil(FUser);
  FreeAndNil(FRoute);
  FreeAndNil(FAddressBookContact);
  FreeAndNil(FOptimization);

  FConnection := nil;
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
  FGeocoding := nil;
  FOrder := nil;
  FActivity := nil;
  FTracking := nil;
  FTerritory := nil;
  FVehicle := nil;
  FUploading := nil;
  FTelematics := nil;
end;

destructor TRoute4MeManager.Destroy;
begin
  Clear;

  inherited;
end;

function TRoute4MeManager.Geocoding: TGeocodingActions;
begin
  if (FGeocoding = nil) then
    FGeocoding := TGeocodingActions.Create(FConnection);
  Result := FGeocoding;
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

function TRoute4MeManager.Telematics: TTelematicActions;
begin
  if (FTelematics = nil) then
    FTelematics := TTelematicActions.Create(FConnection);
  Result := FTelematics;
end;

function TRoute4MeManager.Territory: TTerritoryActions;
begin
  if (FTerritory = nil) then
    FTerritory := TTerritoryActions.Create(FConnection);
  Result := FTerritory;
end;

function TRoute4MeManager.Tracking: TTrackingActions;
begin
  if (FTracking = nil) then
    FTracking := TTrackingActions.Create(FConnection);
  Result := FTracking;
end;

function TRoute4MeManager.Uploading: TFileUploadingActions;
begin
  if (FUploading = nil) then
    FUploading := TFileUploadingActions.Create(FConnection);
  Result := FUploading;
end;

function TRoute4MeManager.User: TUserActions;
begin
  if (FUser = nil) then
    FUser := TUserActions.Create(FConnection);
  Result := FUser;
end;

function TRoute4MeManager.Vehicle: TVehicleActions;
begin
  if (FVehicle = nil) then
    FVehicle := TVehicleActions.Create(FConnection);
  Result := FVehicle;
end;

function TRoute4MeManager.ActivityFeed: TActivityActions;
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
