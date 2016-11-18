unit Route4MeExamplesUnit;

interface

uses
  SysUtils, System.Generics.Collections,
  Route4MeManagerUnit, OutputUnit, NullableBasicTypesUnit,
  CommonTypesUnit, IConnectionUnit,
  DataObjectUnit, AddressBookContactUnit, OrderUnit, RouteParametersUnit,
  AddOrderToRouteRequestUnit, AddressUnit;

type
  TRoute4MeExamples = class
  strict private
    FOutput: IOutput;
    FConnection: IConnection;

    function MakeExample(Clazz: TClass): TObject;
  public
    constructor Create(Output: IOutput; Connection: IConnection);
    destructor Destroy; override;

    function SingleDriverRoute10Stops: TDataObject;
    function SingleDriverRoundTrip: TDataObject;
    function SingleDriverRoundTripGeneric: NullableString;
    function MultipleDepotMultipleDriver: TDataObject;
    function MultipleDepotMultipleDriverTimeWindow: TDataObject;
    function SingleDepotMultipleDriverNoTimeWindow: TDataObject;
    function MultipleDepotMultipleDriverWith24StopsTimeWindow: TDataObject;
    function SingleDriverMultipleTimeWindows: TDataObject;
    procedure ResequenceRouteDestinations(Route: TDataObjectRoute);
    procedure ResequenceAllRouteDestinations(RouteId: String);
    function AddRouteDestinations(RouteId: String): TArray<integer>;
    function AddRouteDestinationsOptimally(RouteId: String): TArray<integer>;
    procedure RemoveRouteDestination(RouteId: String; DestinationId: integer);
    procedure MoveDestinationToRoute(ToRouteId: String;
      RouteDestinationId, AfterDestinationId: integer);
    procedure GetOptimization(OptimizationProblemId: String);
    procedure GetOptimizations;
    procedure RemoveOptimization(OptimizationProblemId: String);
    function AddDestinationToOptimization(OptimizationId: String;
      AndReOptimize: boolean): TDataObject;
    procedure RemoveDestinationFromOptimization(OptimizationId: String;
      DestinationId: integer; AndReOptimize: boolean);
    procedure ReOptimization(OptimizationProblemId: String);
    procedure ReoptimizeRoute(RouteId: String);
    procedure MergeRoutes(RouteIds: TListString);
    procedure UpdateRoute(RouteId: String);
    procedure UpdateRoutesCustomFields(RouteId: String; RouteDestinationId: integer);
    procedure GetRoute(RouteId: String; GetRouteDirections, GetRoutePathPoints: boolean);
    function GetRoutes: TDataObjectRouteArray;
    procedure GetUsers();
    function LogCustomActivity(Message: String; RouteId: String): boolean;
    procedure GetActivities(RouteId: String);
    procedure GetAddress(RouteId: String; RouteDestinationId: integer);
    procedure AddAddressNote(RouteId: String; AddressId: integer);
    procedure GetAddressNotes(RouteId: String; RouteDestinationId: integer);
    function DuplicateRoute(RouteId: String): NullableString;
    procedure ShareRoute(RouteId: String; RecipientEmail: String);
    procedure DeleteRoutes(RouteIds: TStringArray);
    function AddAddressBookContact(FirstName, Address: String): TAddressBookContact;
    procedure GetAddressBookContacts;
    procedure UpdateAddressBookContact(Contact: TAddressBookContact);
    procedure RemoveAddressBookContacts(AddressIds: TArray<integer>);
    function AddAvoidanceZone: NullableString;
    procedure GetAvoidanceZones;
    procedure GetAvoidanceZone(TerritoryId: String);
    procedure UpdateAvoidanceZone(TerritoryId: String);
    procedure DeleteAvoidanceZone(TerritoryId: String);
    function AddOrder(): TOrder;
    procedure GetOrders; overload;
    procedure GetOrder(OrderId: String);
    procedure GetOrders(Date: TDate); overload;
    procedure GetOrdersScheduledFor(Date: TDate);
    procedure UpdateOrder(Order: TOrder);
    procedure RemoveOrders(OrderIds: TStringArray);
    procedure AddOrderToRoute(RouteId: String; Parameters: TRouteParameters;
      OrderedAddresses: TOrderedAddressArray);
    procedure AddOrderToOptimization(OptimizationId: String;
      Parameters: TRouteParameters; OrderedAddresses: TOrderedAddressArray);
    procedure SetGPSPosition(RouteId: String);
    procedure TrackDeviceLastLocationHistory(RouteId: String);
    procedure GenericExample(Connection: IConnection);
    procedure GenericExampleShortcut(Connection: IConnection);
  end;

implementation

uses
  BaseExampleUnit,
  SingleDriverRoute10StopsUnit, MultipleDepotMultipleDriverUnit,
  MultipleDepotMultipleDriverTimeWindowUnit,
  SingleDepotMultipleDriverNoTimeWindowUnit,
  SingleDriverMultipleTimeWindowsUnit, SingleDriverRoundTripUnit,
  SingleDriverRoundTripGenericUnit,
  MultipleDepotMultipleDriverWith24StopsTimeWindowUnit,
  ResequenceRouteDestinationsUnit, AddRouteDestinationsUnit,
  AddRouteDestinationsOptimallyUnit, RemoveRouteDestinationUnit,
  MoveDestinationToRouteUnit, GetOptimizationsUnit, GetOptimizationUnit,
  AddDestinationToOptimizationUnit, RemoveDestinationFromOptimizationUnit,
  ReoptimizeRouteUnit, ReOptimizationUnit, UpdateRouteUnit, GetRoutesUnit,
  GetRouteUnit, GetUsersUnit, LogCustomActivityUnit, GetActivitiesUnit,
  GetAddressUnit, GetAddressNotesUnit, AddAddressNoteUnit, DuplicateRouteUnit,
  ShareRouteUnit, AddAddressBookContactUnit, GetAddressBookContactsUnit,
  UpdateAddressBookContactUnit, RemoveAddressBookContactsUnit,
  AddAvoidanceZoneUnit, GetAvoidanceZoneUnit, GetAvoidanceZonesUnit,
  UpdateAvoidanceZoneUnit, DeleteAvoidanceZoneUnit, AddOrderUnit, GetOrdersUnit,
  UpdateOrderUnit, RemoveOrdersUnit, SetGPSPositionUnit,
  TrackDeviceLastLocationHistoryUnit, GenericExampleShortcutUnit,
  GenericExampleUnit, MergeRoutesUnit, UpdateRoutesCustomFieldsUnit,
  DeleteRoutesUnit, RemoveOptimizationUnit, ResequenceAllRouteDestinationsUnit,
  AddOrderToRouteUnit, AddOrderToOptimizationUnit, GetOrderUnit,
  GetOrdersByDateUnit, GetOrdersScheduledForUnit;

function TRoute4MeExamples.AddAddressBookContact(FirstName,
  Address: String): TAddressBookContact;
var
  Example: TAddAddressBookContact;
begin
  Example := MakeExample(TAddAddressBookContact) as TAddAddressBookContact;
  try
    Result := Example.Execute(FirstName, Address);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.AddAddressNote(RouteId: String; AddressId: integer);
var
  Example: TAddAddressNote;
begin
  Example := MakeExample(TAddAddressNote) as TAddAddressNote;
  try
    Example.Execute(RouteId, AddressId);
  finally
    FreeAndNil(Example);
  end;
end;

function TRoute4MeExamples.AddAvoidanceZone: NullableString;
var
  Example: TAddAvoidanceZone;
begin
  Example := MakeExample(TAddAvoidanceZone) as TAddAvoidanceZone;
  try
    Result := Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

function TRoute4MeExamples.AddDestinationToOptimization(OptimizationId: String;
  AndReOptimize: boolean): TDataObject;
var
  Example: TAddDestinationToOptimization;
begin
  Example := MakeExample(TAddDestinationToOptimization) as TAddDestinationToOptimization;
  try
    Result := Example.Execute(OptimizationId, AndReOptimize);
  finally
    FreeAndNil(Example);
  end;
end;

function TRoute4MeExamples.AddOrder: TOrder;
var
  Example: TAddOrder;
begin
  Example := MakeExample(TAddOrder) as TAddOrder;
  try
    Result := Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.AddOrderToOptimization(OptimizationId: String;
  Parameters: TRouteParameters; OrderedAddresses: TOrderedAddressArray);
var
  Example: TAddOrderToOptimization;
begin
  Example := MakeExample(TAddOrderToOptimization) as TAddOrderToOptimization;
  try
    Example.Execute(OptimizationId, Parameters, OrderedAddresses);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.AddOrderToRoute(RouteId: String;
  Parameters: TRouteParameters; OrderedAddresses: TOrderedAddressArray);
var
  Example: TAddOrderToRoute;
begin
  Example := MakeExample(TAddOrderToRoute) as TAddOrderToRoute;
  try
    Example.Execute(RouteId, Parameters, OrderedAddresses);
  finally
    FreeAndNil(Example);
  end;
end;

function TRoute4MeExamples.AddRouteDestinations(RouteId: String): TArray<integer>;
var
  Example: TAddRouteDestinations;
begin
  Example := MakeExample(TAddRouteDestinations) as TAddRouteDestinations;
  try
    Result := Example.Execute(RouteId);
  finally
    FreeAndNil(Example);
  end;
end;

function TRoute4MeExamples.AddRouteDestinationsOptimally(
  RouteId: String): TArray<integer>;
var
  Example: TAddRouteDestinationsOptimally;
begin
  Example := MakeExample(TAddRouteDestinationsOptimally) as TAddRouteDestinationsOptimally;
  try
    Result := Example.Execute(RouteId);
  finally
    FreeAndNil(Example);
  end;
end;

constructor TRoute4MeExamples.Create(Output: IOutput; Connection: IConnection);
begin
  FOutput := Output;
  FConnection := Connection;
end;

procedure TRoute4MeExamples.DeleteAvoidanceZone(TerritoryId: String);
var
  Example: TDeleteAvoidanceZone;
begin
  Example := MakeExample(TDeleteAvoidanceZone) as TDeleteAvoidanceZone;
  try
    Example.Execute(TerritoryId);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.DeleteRoutes(RouteIds: TStringArray);
var
  Example: TDeleteRoutes;
begin
  Example := MakeExample(TDeleteRoutes) as TDeleteRoutes;
  try
    Example.Execute(RouteIds);
  finally
    FreeAndNil(Example);
  end;
end;

destructor TRoute4MeExamples.Destroy;
begin
  FOutput := nil;
  FConnection := nil;

  inherited;
end;

function TRoute4MeExamples.DuplicateRoute(RouteId: String): NullableString;
var
  Example: TDuplicateRoute;
begin
  Example := MakeExample(TDuplicateRoute) as TDuplicateRoute;
  try
    Result := Example.Execute(RouteId);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GenericExample(Connection: IConnection);
var
  Example: TGenericExample;
begin
  Example := MakeExample(TGenericExample) as TGenericExample;
  try
    Example.Execute(Connection);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GenericExampleShortcut(Connection: IConnection);
var
  Example: TGenericExampleShortcut;
begin
  Example := MakeExample(TGenericExampleShortcut) as TGenericExampleShortcut;
  try
    Example.Execute(Connection);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetActivities(RouteId: String);
var
  Example: TGetActivities;
begin
  Example := MakeExample(TGetActivities) as TGetActivities;
  try
    Example.Execute(RouteId);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetAddress(RouteId: String;
  RouteDestinationId: integer);
var
  Example: TGetAddress;
begin
  Example := MakeExample(TGetAddress) as TGetAddress;
  try
    Example.Execute(RouteId, RouteDestinationId);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetAddressBookContacts;
var
  Example: TGetAddressBookContacts;
begin
  Example := MakeExample(TGetAddressBookContacts) as TGetAddressBookContacts;
  try
    Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetAddressNotes(RouteId: String;
  RouteDestinationId: integer);
var
  Example: TGetAddressNotes;
begin
  Example := MakeExample(TGetAddressNotes) as TGetAddressNotes;
  try
    Example.Execute(RouteId, RouteDestinationId);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetAvoidanceZone(TerritoryId: String);
var
  Example: TGetAvoidanceZone;
begin
  Example := MakeExample(TGetAvoidanceZone) as TGetAvoidanceZone;
  try
    Example.Execute(TerritoryId);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetAvoidanceZones;
var
  Example: TGetAvoidanceZones;
begin
  Example := MakeExample(TGetAvoidanceZones) as TGetAvoidanceZones;
  try
    Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetOptimization(OptimizationProblemId: String);
var
  Example: TGetOptimization;
begin
  Example := MakeExample(TGetOptimization) as TGetOptimization;
  try
    Example.Execute(OptimizationProblemId);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetOptimizations;
var
  Example: TGetOptimizations;
begin
  Example := MakeExample(TGetOptimizations) as TGetOptimizations;
  try
    Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetOrder(OrderId: String);
var
  Example: TGetOrder;
begin
  Example := MakeExample(TGetOrder) as TGetOrder;
  try
    Example.Execute(OrderId);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetOrders(Date: TDate);
var
  Example: TGetOrdersByDate;
begin
  Example := MakeExample(TGetOrdersByDate) as TGetOrdersByDate;
  try
    Example.Execute(Date);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetOrdersScheduledFor(Date: TDate);
var
  Example: TGetOrdersScheduledFor;
begin
  Example := MakeExample(TGetOrdersScheduledFor) as TGetOrdersScheduledFor;
  try
    Example.Execute(Date);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetOrders;
var
  Example: TGetOrders;
begin
  Example := MakeExample(TGetOrders) as TGetOrders;
  try
    Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetRoute(RouteId: String; GetRouteDirections,
  GetRoutePathPoints: boolean);
var
  Example: TGetRoute;
begin
  Example := MakeExample(TGetRoute) as TGetRoute;
  try
    Example.Execute(RouteId, GetRouteDirections, GetRoutePathPoints);
  finally
    FreeAndNil(Example);
  end;
end;

function TRoute4MeExamples.GetRoutes: TDataObjectRouteArray;
var
  Example: TGetRoutes;
begin
  Example := MakeExample(TGetRoutes) as TGetRoutes;
  try
    Result := Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetUsers;
var
  Example: TGetUsers;
begin
  Example := MakeExample(TGetUsers) as TGetUsers;
  try
    Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

function TRoute4MeExamples.LogCustomActivity(Message, RouteId: String): boolean;
var
  Example: TLogCustomActivity;
begin
  Example := MakeExample(TLogCustomActivity) as TLogCustomActivity;
  try
    Result := Example.Execute(Message, RouteId);
  finally
    FreeAndNil(Example);
  end;
end;

function TRoute4MeExamples.MakeExample(Clazz: TClass): TObject;
var
  Example: TBaseExample;
begin
  Example := Clazz.Create as TBaseExample;
  Example.Init(FOutput, FConnection);
  Result := Example;
end;

procedure TRoute4MeExamples.MergeRoutes(RouteIds: TListString);
var
  Example: TMergeRoutes;
begin
  Example := MakeExample(TMergeRoutes) as TMergeRoutes;
  try
    Example.Execute(RouteIds);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.MoveDestinationToRoute(ToRouteId: String;
  RouteDestinationId, AfterDestinationId: integer);
var
  Example: TMoveDestinationToRoute;
begin
  Example := MakeExample(TMoveDestinationToRoute) as TMoveDestinationToRoute;
  try
    Example.Execute(ToRouteId, RouteDestinationId, AfterDestinationId);
  finally
    FreeAndNil(Example);
  end;
end;

function TRoute4MeExamples.MultipleDepotMultipleDriver: TDataObject;
var
  Example: TMultipleDepotMultipleDriver;
begin
  Example := MakeExample(TMultipleDepotMultipleDriver) as TMultipleDepotMultipleDriver;
  try
    Result := Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

function TRoute4MeExamples.MultipleDepotMultipleDriverTimeWindow: TDataObject;
var
  Example: TMultipleDepotMultipleDriverTimeWindow;
begin
  Example := MakeExample(TMultipleDepotMultipleDriverTimeWindow) as TMultipleDepotMultipleDriverTimeWindow;
  try
    Result := Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

function TRoute4MeExamples.MultipleDepotMultipleDriverWith24StopsTimeWindow: TDataObject;
var
  Example: TMultipleDepotMultipleDriverWith24StopsTimeWindow;
begin
  Example := MakeExample(TMultipleDepotMultipleDriverWith24StopsTimeWindow) as TMultipleDepotMultipleDriverWith24StopsTimeWindow;
  try
    Result := Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.RemoveAddressBookContacts(
  AddressIds: TArray<integer>);
var
  Example: TRemoveAddressBookContacts;
begin
  Example := MakeExample(TRemoveAddressBookContacts) as TRemoveAddressBookContacts;
  try
    Example.Execute(AddressIds);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.RemoveDestinationFromOptimization(
  OptimizationId: String; DestinationId: integer; AndReOptimize: boolean);
var
  Example: TRemoveDestinationFromOptimization;
begin
  Example := MakeExample(TRemoveDestinationFromOptimization) as TRemoveDestinationFromOptimization;
  try
    Example.Execute(OptimizationId, DestinationId, AndReOptimize);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.RemoveOptimization(OptimizationProblemId: String);
var
  Example: TRemoveOptimization;
begin
  Example := MakeExample(TRemoveOptimization) as TRemoveOptimization;
  try
    Example.Execute(OptimizationProblemId);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.RemoveOrders(OrderIds: TStringArray);
var
  Example: TRemoveOrders;
begin
  Example := MakeExample(TRemoveOrders) as TRemoveOrders;
  try
    Example.Execute(OrderIds);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.RemoveRouteDestination(RouteId: String;
  DestinationId: integer);
var
  Example: TRemoveRouteDestination;
begin
  Example := MakeExample(TRemoveRouteDestination) as TRemoveRouteDestination;
  try
    Example.Execute(RouteId, DestinationId);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.ReOptimization(OptimizationProblemId: String);
var
  Example: TReOptimization;
begin
  Example := MakeExample(TReOptimization) as TReOptimization;
  try
    Example.Execute(OptimizationProblemId);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.ReoptimizeRoute(RouteId: String);
var
  Example: TReoptimizeRoute;
begin
  Example := MakeExample(TReoptimizeRoute) as TReoptimizeRoute;
  try
    Example.Execute(RouteId);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.ResequenceAllRouteDestinations(RouteId: String);
var
  Example: TResequenceAllRouteDestinations;
begin
  Example := MakeExample(TResequenceAllRouteDestinations) as TResequenceAllRouteDestinations;
  try
    Example.Execute(RouteId);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.ResequenceRouteDestinations(Route: TDataObjectRoute);
var
  Example: TResequenceRouteDestinations;
begin
  Example := MakeExample(TResequenceRouteDestinations) as TResequenceRouteDestinations;
  try
    Example.Execute(Route);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.SetGPSPosition(RouteId: String);
var
  Example: TSetGPSPosition;
begin
  Example := MakeExample(TSetGPSPosition) as TSetGPSPosition;
  try
    Example.Execute(RouteId);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.ShareRoute(RouteId, RecipientEmail: String);
var
  Example: TShareRoute;
begin
  Example := MakeExample(TShareRoute) as TShareRoute;
  try
    Example.Execute(RouteId, RecipientEmail);
  finally
    FreeAndNil(Example);
  end;
end;

function TRoute4MeExamples.SingleDepotMultipleDriverNoTimeWindow: TDataObject;
var
  Example: TSingleDepotMultipleDriverNoTimeWindow;
begin
  Example := MakeExample(TSingleDepotMultipleDriverNoTimeWindow) as TSingleDepotMultipleDriverNoTimeWindow;
  try
    Result := Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

function TRoute4MeExamples.SingleDriverMultipleTimeWindows: TDataObject;
var
  Example: TSingleDriverMultipleTimeWindows;
begin
  Example := MakeExample(TSingleDriverMultipleTimeWindows) as TSingleDriverMultipleTimeWindows;
  try
    Result := Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

function TRoute4MeExamples.SingleDriverRoundTrip: TDataObject;
var
  Example: TSingleDriverRoundTrip;
begin
  Example := MakeExample(TSingleDriverRoundTrip) as TSingleDriverRoundTrip;
  try
    Result := Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

function TRoute4MeExamples.SingleDriverRoundTripGeneric: NullableString;
var
  Example: TSingleDriverRoundTripGeneric;
begin
  Example := MakeExample(TSingleDriverRoundTripGeneric) as TSingleDriverRoundTripGeneric;
  try
    Result := Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

function TRoute4MeExamples.SingleDriverRoute10Stops: TDataObject;
var
  Example: TSingleDriverRoute10Stops;
begin
  Example := MakeExample(TSingleDriverRoute10Stops) as TSingleDriverRoute10Stops;
  try
    Result := Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.TrackDeviceLastLocationHistory(RouteId: String);
var
  Example: TTrackDeviceLastLocationHistory;
begin
  Example := MakeExample(TTrackDeviceLastLocationHistory) as TTrackDeviceLastLocationHistory;
  try
    Example.Execute(RouteId);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.UpdateAddressBookContact(
  Contact: TAddressBookContact);
var
  Example: TUpdateAddressBookContact;
begin
  Example := MakeExample(TUpdateAddressBookContact) as TUpdateAddressBookContact;
  try
    Example.Execute(Contact);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.UpdateAvoidanceZone(TerritoryId: String);
var
  Example: TUpdateAvoidanceZone;
begin
  Example := MakeExample(TUpdateAvoidanceZone) as TUpdateAvoidanceZone;
  try
    Example.Execute(TerritoryId);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.UpdateRoutesCustomFields(RouteId: String;
  RouteDestinationId: integer);
var
  Example: TUpdateRoutesCustomFields;
begin
  Example := MakeExample(TUpdateRoutesCustomFields) as TUpdateRoutesCustomFields;
  try
    Example.Execute(RouteId, RouteDestinationId);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.UpdateOrder(Order: TOrder);
var
  Example: TUpdateOrder;
begin
  Example := MakeExample(TUpdateOrder) as TUpdateOrder;
  try
    Example.Execute(Order);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.UpdateRoute(RouteId: String);
var
  Example: TUpdateRoute;
begin
  Example := MakeExample(TUpdateRoute) as TUpdateRoute;
  try
    Example.Execute(RouteId);
  finally
    FreeAndNil(Example);
  end;
end;

end.
