unit Route4MeExamplesUnit;

interface

uses
  SysUtils, System.Generics.Collections,
  Route4MeManagerUnit, DataObjectUnit, NullableBasicTypesUnit, UtilsUnit,
  CommonTypesUnit, AddressBookContactUnit,
  SingleDriverRoundTripTestDataProviderUnit, IRoute4MeManagerUnit, OutputUnit,
  IConnectionUnit, RouteParametersUnit;

type
  TRoute4MeExamples = class
  private
    FOutput: IOutput;

    procedure WriteLn(Message: String);
  protected
    Route4MeManager: IRoute4MeManager;

    procedure PrintExampleOptimizationResult(ExampleName: String;
      DataObject: TDataObject; ErrorString: String);
  public
    constructor Create(Output: IOutput; Connection: IConnection); reintroduce;
    destructor Destroy; override;

    function SingleDriverRoute10Stops: TDataObject;
    procedure ResequenceRouteDestinations(Route: TDataObjectRoute);
    function AddRouteDestinations(RouteId: String): TArray<integer>;
    procedure RemoveRouteDestination(RouteId: String; DestinationId: integer);
    function SingleDriverRoundTrip: TDataObject;
{-}
    procedure MoveDestinationToRoute(ToRouteId: String;
      RouteDestinationId, AfterDestinationId: integer);
{+}
    function SingleDriverRoundTripGeneric: NullableString;
    function MultipleDepotMultipleDriver: TDataObject;
    function MultipleDepotMultipleDriverTimeWindow: TDataObject;
{- conflict}
    function SingleDepotMultipleDriverNoTimeWindow: TDataObject;
{+}
    function MultipleDepotMultipleDriverWith24StopsTimeWindow: TDataObject;
    function SingleDriverMultipleTimeWindows: TDataObject;
    procedure GetOptimization(OptimizationProblemId: String);
    procedure GetOptimizations;
    function AddDestinationToOptimization(OptimizationId: String;
      AndReOptimize: boolean): TDataObject;
    procedure RemoveDestinationFromOptimization(OptimizationId: String;
      DestinationId: integer; AndReOptimize: boolean);
{-}
    procedure ReOptimization(OptimizationProblemId: String);
    procedure UpdateRoute(RouteId: String);
    procedure ReoptimizeRoute(RouteId: String);
    procedure GetRoute(RouteId: String; GetRouteDirections, GetRoutePathPoints: boolean);
    procedure GetRoutes();

    procedure GetUsers();
    procedure LogCustomActivity(Message: String; RouteId: String);
    procedure GetActivities(RouteId: String);
    procedure GetAddress(RouteId: String; RouteDestinationId: integer);
    procedure AddAddressNote(RouteId: String; RouteDestinationId: integer);
    procedure GetAddressNotes(RouteId: String; RouteDestinationId: integer);
    function DuplicateRoute(RouteId: String): NullableString;
    procedure SetGPSPosition(RouteId: String);
    procedure TrackDeviceLastLocationHistory(RouteId: String);
    procedure DeleteRoutes(RouteIds: TStringArray);
    procedure RemoveOptimization(OptimizationProblemId: String);
    function AddAddressBookContact(): TAddressBookContact;
    procedure GetAddressBookContacts;
    procedure UpdateAddressBookContact(Contact: TAddressBookContact);
    procedure RemoveAddressBookContacts(ContactIds: TStringArray);
    function AddAvoidanceZone: NullableString;
    procedure GetAvoidanceZones;
    procedure GetAvoidanceZone(TerritoryId: String);
    procedure UpdateAvoidanceZone(TerritoryId: String);
    procedure DeleteAvoidanceZone(TerritoryId: String);
    function AddOrder: TOrder;
    procedure GetOrders;
    procedure UpdateOrder(Order: TOrder);
    procedure RemoveOrders(OrderIds: TStringArray);
    procedure GenericExample();
    procedure GenericExampleShortcut();
  end;

implementation

{ TRoute4MeExamples }

uses
  AddressUnit, EnumsUnit, IOptimizationParametersProviderUnit,
  OptimizationParametersUnit, SingleDriverRoute10StopsTestDataProviderUnit,
  GenericParametersUnit, AddressesOrderInfoUnit,
  MultipleDepotMultipleDriverTestDataProviderUnit,
  MultipleDepotMultipleDriverTimeWindowTestDataProviderUnit,
  SingleDepotMultipleDriverNoTimeWindowTestDataProviderUnit,
  MultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProviderUnit,
  SingleDriverMultipleTimeWindowsTestDataProviderUnit,
  SingleDriverRoundTripGenericTestDataProviderUnit,
  SingleDriverRoundTripGenericRequestUnit,
  SingleDriverRoundTripGenericResponseUnit, SettingsUnit,
  RouteParametersQueryUnit, UserUnit;

function TRoute4MeExamples.AddAddressBookContact: TAddressBookContact;
var
  ErrorString: String;
begin
  AddressBookContact contact = new AddressBookContact()
  {
    FirstName = "Test FirstName " + (new Random()).Next().ToString(),
    Address1 = "Test Address1 " + (new Random()).Next().ToString(),
    CachedLat = 38.024654,
    CachedLng = -77.338814
  };

  // Run the query
  AddressBookContact resultContact = Route4MeManager.AddAddressBookContact(contact, ErrorString);

  WriteLn('');

  if (resultContact != null) then
  begin
    WriteLn('AddAddressBookContact executed successfully');

    WriteLn(Format('AddressId: {0}", resultContact.AddressId);

    return resultContact;
  end
  else
  begin
    WriteLn(Format('AddAddressBookContact error: "%s"', [ErrorString]));

    return null;
  end
end;

procedure TRoute4MeExamples.AddAddressNote(RouteId: String; RouteDestinationId: integer);
var
  ErrorString: String;
begin
  NoteParameters noteParameters = new NoteParameters()
  {
    RouteId = routeId,
    AddressId = addressId,
    Latitude = 33.132675170898,
    Longitude = -83.244743347168,
    DeviceType = DeviceType.Web.Description(),
    ActivityType = StatusUpdateType.DropOff.Description()
  };

  // Run the query
  string contents = "Test Note Contents " + DateTime.Now.ToString();
  AddressNote note = Route4MeManager.AddAddressNote(noteParameters, contents, ErrorString);

  WriteLn('');

  if (note != null) then
  begin
    WriteLn('AddAddressNote executed successfully');
    WriteLn(Format('Note ID: {0}", note.NoteId);
  end
  else
    WriteLn(Format('AddAddressNote error: "%s"', [ErrorString]));
end;

function TRoute4MeExamples.AddAvoidanceZone: NullableString;
var
  ErrorString: String;
begin
  AvoidanceZoneParameters avoidanceZoneParameters = new AvoidanceZoneParameters()
  {
    TerritoryName = "Test Territory",
    TerritoryColor = "ff0000",
    Territory = new Territory()
    {
      Type = TerritoryType.Circle.Description(),
      Data = new string[] { "37.569752822786455,-77.47833251953125",
                            "5000"}
    }
  };

  // Run the query
  AvoidanceZone avoidanceZone = Route4MeManager.AddAvoidanceZone(avoidanceZoneParameters, ErrorString);

  WriteLn('');

  if (avoidanceZone != null) then
  begin
    WriteLn('AddAvoidanceZone executed successfully');

    WriteLn(Format('Territory ID: {0}", avoidanceZone.TerritoryId);

    return avoidanceZone.TerritoryId;
  end
  else
  begin
    WriteLn(Format('AddAvoidanceZone error: "%s"', [ErrorString]));

    return null;
  end
end;

function TRoute4MeExamples.AddDestinationToOptimization(
  OptimizationId: String; AndReOptimize: boolean): TDataObject;
var
  Address: TAddress;
  OptimizationParameters: TOptimizationParameters;
  ErrorString: String;
begin
  // Prepare the address that we are going to add to an existing route optimization
  Address := TAddress.Create;
  Address.AddressString := '717 5th Ave New York, NY 10021';
  Address.Alias := 'Giorgio Armani';
  Address.Latitude := 40.7669692;
  Address.Longitude := -73.9693864;
  Address.Time := 0;

  //Optionally change any route parameters, such as maximum route duration, maximum cubic constraints, etc.
  OptimizationParameters := TOptimizationParameters.Create;
  try
    OptimizationParameters.OptimizationProblemID := OptimizationId;
    OptimizationParameters.AddAddress(Address);
    OptimizationParameters.ReOptimize := AndReOptimize;

    // Execute the optimization to re-optimize and rebalance all the routes in this optimization
    Result := Route4MeManager.Optimization.Update(OptimizationParameters, ErrorString);

    WriteLn('');

    if (Result <> nil) then
    begin
      WriteLn('AddDestinationToOptimization executed successfully');

      WriteLn(Format('Optimization Problem ID: %s', [Result.OptimizationProblemId]));
      WriteLn(Format('State: %s', [TOptimizationDescription[TOptimizationState(Result.State)]]));
    end
    else
      WriteLn(Format('AddDestinationToOptimization error: %s', [ErrorString]));
  finally
    FreeAndNil(OptimizationParameters);
  end;
end;

function TRoute4MeExamples.AddOrder: TOrder;
var
  ErrorString: String;
begin
  Order order = new Order()
  {
    Address1 = "Test Address1 " + (new Random()).Next().ToString(),
    AddressAlias = "Test AddressAlias " + (new Random()).Next().ToString(),
    CachedLatitude = 37.773972,
    CachedLongitude = -122.431297
  };

  // Run the query
  Order resultOrder = Route4MeManager.AddOrder(order, ErrorString);

  WriteLn('');

  if (resultOrder != null) then
  begin
    WriteLn('AddOrder executed successfully');

    WriteLn(Format('Order ID: {0}", resultOrder.OrderId);

    return resultOrder;
  end
  else
  begin
    WriteLn(Format('AddOrder error: "%s"', [ErrorString]));

    return null;
  end
end;

function TRoute4MeExamples.AddRouteDestinations(
  RouteId: String): TArray<integer>;
var
  Addresses: TAddressesArray;
  OptimalPosition: boolean;
  ErrorString: String;
  AddressIds: TStringArray;
  i: integer;
begin
  // Prepare the addresses
  SetLength(Addresses, 2);
  try
    Addresses[0] := TAddress.Create(
      '146 Bill Johnson Rd NE Milledgeville GA 31061',
      33.143526, -83.240354, 0);
    Addresses[1] := TAddress.Create(
      '222 Blake Cir Milledgeville GA 31061',
      33.177852, -83.263535, 0);

    // Run the query
    OptimalPosition := True;
    Result := Route4MeManager.Route.Add(RouteId, Addresses, OptimalPosition, ErrorString);

    WriteLn('');

    if (Length(Result) > 0) then
    begin
      WriteLn('AddRouteDestinations executed successfully');

      SetLength(AddressIds, Length(Result));
      for i := 0 to Length(Result) - 1 do
        AddressIds[i] := IntToStr(Result[i]);
      WriteLn(Format('Destination IDs: %s', [String.Join(' ', AddressIds)]));
    end
    else
      WriteLn(Format('AddRouteDestinations error: "%s"', [errorString]));
  finally
    for i := Length(Addresses) - 1 downto 0 do
      FreeAndNil(Addresses[i]);
  end;
end;

constructor TRoute4MeExamples.Create(Output: IOutput; Connection: IConnection);
begin
  FOutput := Output;

  // Create the manager with the api key
  Route4MeManager := TRoute4MeManager.Create(Connection);
end;

procedure TRoute4MeExamples.DeleteAvoidanceZone(TerritoryId: String);
var
  ErrorString: String;
begin
  AvoidanceZoneQuery avoidanceZoneQuery = new AvoidanceZoneQuery()
  {
    TerritoryId = territoryId
  };

  // Run the query
  Route4MeManager.DeleteAvoidanceZone(avoidanceZoneQuery, ErrorString);

  WriteLn('');

  if (errorString == "") then
  begin
    WriteLn('DeleteAvoidanceZone executed successfully');

    WriteLn(Format('Territory ID: {0}", territoryId);
  end
  else
    WriteLn(Format('DeleteAvoidanceZone error: "%s"', [ErrorString]));
end;

procedure TRoute4MeExamples.DeleteRoutes(RouteIds: TStringArray);
var
  ErrorString: String;
begin
  // Create the manager with the api key
  Route4MeManager route4Me = new Route4MeManager(c_ApiKey);

  //routeIds = new string[] { "1" };

  // Run the query
  string[] deletedRouteIds = Route4MeManager.DeleteRoutes(routeIds, ErrorString);

  WriteLn('');

  if (deletedRouteIds != null) then
  begin
    WriteLn(Format('DeleteRoutes executed successfully, {0} routes deleted", deletedRouteIds.Length);
    WriteLn('');
  end
  else
    WriteLn(Format('DeleteRoutes error "%s"', [ErrorString]));
end;

destructor TRoute4MeExamples.Destroy;
begin
  FOutput := nil;
  Route4MeManager := nil;

  inherited;
end;

function TRoute4MeExamples.DuplicateRoute(RouteId: String): NullableString;
var
  ErrorString: String;
begin
  RouteParametersQuery routeParameters = new RouteParametersQuery()
  {
    RouteId = routeId
  };

  // Run the query
  string duplicatedRouteId = Route4MeManager.DuplicateRoute(routeParameters, ErrorString);

  WriteLn('');

  if (duplicatedRouteId != null) then
  begin
    WriteLn(Format('DuplicateRoute executed successfully, duplicated route ID: {0}", duplicatedRouteId);
    WriteLn('');
  end
  else
    WriteLn(Format('DuplicateRoute error "%s"', [ErrorString]));

  return duplicatedRouteId;
end;

procedure TRoute4MeExamples.GenericExample;
var
  ErrorString: String;
begin
  const string uri = R4MEInfrastructureSettings.MainHost + "/api.v4/route.php";

  //the api key of the account
  //the api key must have hierarchical ownership of the route being viewed (api key can't view routes of others)
  const string myApiKey = "11111111111111111111111111111111";

  Route4MeManager route4Me = new Route4MeManager(myApiKey);

  GenericParameters genericParameters = new GenericParameters();

  //number of records per page
  genericParameters.ParametersCollection.Add("limit", "10');

  //the page offset starting at zero
  genericParameters.ParametersCollection.Add("Offset", "5');

  DataObjectRoute[] dataObjects = Route4MeManager.GetJsonObjectFromAPI<DataObjectRoute[]>(genericParameters,
                                                                                   uri,
                                                                                   HttpMethodType.Get,
                                                                                   out errorMessage);

  WriteLn('');

  if (dataObjects != null) then
  begin
    WriteLn(Format('GenericExample executed successfully, {0} routes returned", dataObjects.Length);
    WriteLn('');

    dataObjects.ForEach(dataObject =>
    {
      WriteLn('Optimization Problem ID: {0}", dataObject.OptimizationProblemId);
      WriteLn(Format('RouteID: {0}", dataObject.RouteID);
      WriteLn('');
    });
  end
  else
    WriteLn(Format('GenericExample error "%s"', [ErrorMessage);
end;

procedure TRoute4MeExamples.GenericExampleShortcut;
var
  ErrorString: String;
begin
  Route4MeManager route4Me = new Route4MeManager(c_ApiKey);

  RouteParametersQuery routeQueryParameters = new RouteParametersQuery()
  {
    Limit = 10,
    Offset = 5
  };

  DataObjectRoute[] dataObjects = Route4MeManager.GetRoutes(routeQueryParameters, ErrorMessage);

  if (dataObjects != null) then
  begin
    WriteLn(Format('GenericExampleShortcut executed successfully, {0} routes returned", dataObjects.Length);
    WriteLn('');

    dataObjects.ForEach(dataObject =>
    {
      WriteLn('Optimization Problem ID: {0}", dataObject.OptimizationProblemId);
      WriteLn(Format('RouteID: {0}", dataObject.RouteID);
      WriteLn('');
    });
  end
  else
    WriteLn(Format('GenericExampleShortcut error "%s"', [ErrorMessage);
end;

procedure TRoute4MeExamples.GetActivities(RouteId: String);
var
  ErrorString: String;
begin
  ActivityParameters activityParameters = new ActivityParameters()
  {
    RouteId = routeId,
    Limit = 10,
    Offset = 0
  };

  // Run the query
  Activity[] activities = Route4MeManager.GetActivityFeed(activityParameters, ErrorString);

  WriteLn('');

  if (activities != null) then
  begin
    WriteLn(Format('GetActivities executed successfully, {0} activities returned", activities.Length);
    WriteLn('');

    activities.ForEach(activity =>
    {
      WriteLn('Activity ID: {0}", activity.ActivityId);
    });
    WriteLn('');
  end
  else
    WriteLn(Format('GetActivities error: "%s"', [ErrorString]));
end;

procedure TRoute4MeExamples.GetAddress(RouteId: String; RouteDestinationId: integer);
var
  ErrorString: String;
begin
  AddressParameters addressParameters = new AddressParameters()
  {
    RouteId = routeId,
    RouteDestinationId = routeDestinationId,
    Notes = true
  };

  // Run the query
  Address dataObject = Route4MeManager.GetAddress(addressParameters, ErrorString);

  WriteLn('');

  if (dataObject != null) then
  begin
    WriteLn('GetAddress executed successfully');
    WriteLn(Format('RouteId: {0}; RouteDestinationId: {1}", dataObject.RouteId, dataObject.RouteDestinationId);
  end
  else
    WriteLn(Format('GetAddress error: "%s"', [ErrorString]));

  WriteLn('');
end;

procedure TRoute4MeExamples.GetAddressBookContacts;
var
  ErrorString: String;
begin
  AddressBookParameters addressBookParameters = new AddressBookParameters()
  {
    Limit = 10,
    Offset = 0
  };

  // Run the query
  uint total;
  AddressBookContact[] contacts = Route4MeManager.GetAddressBookContacts(addressBookParameters, out total, ErrorString);

  WriteLn('');

  if (contacts != null) then
  begin
    WriteLn(Format('GetAddressBookContacts executed successfully, {0} contacts returned, total = {1}", contacts.Length, total);
    WriteLn('');
  end
  else
    WriteLn(Format('GetAddressBookContacts error: "%s"', [ErrorString]));
end;

procedure TRoute4MeExamples.GetAddressNotes(RouteId: String;
  RouteDestinationId: integer);
var
  ErrorString: String;
begin
  NoteParameters noteParameters = new NoteParameters()
  {
    RouteId = routeId,
    AddressId = routeDestinationId
  };

  // Run the query
  AddressNote[] notes = Route4MeManager.GetAddressNotes(noteParameters, ErrorString);

  WriteLn('');

  if (notes != null) then
    WriteLn(Format('GetAddressNotes executed successfully, {0} notes returned", notes.Length)
  else
    WriteLn(Format('GetAddressNotes error: "%s"', [ErrorString]));

  WriteLn('');
end;

procedure TRoute4MeExamples.GetAvoidanceZone(TerritoryId: String);
var
  ErrorString: String;
begin
  AvoidanceZoneQuery avoidanceZoneQuery = new AvoidanceZoneQuery()
  {

  };

  // Run the query
  AvoidanceZone[] avoidanceZones = Route4MeManager.GetAvoidanceZones(avoidanceZoneQuery, ErrorString);

  WriteLn('');

  if (avoidanceZones != null) then
    WriteLn(Format('GetAvoidanceZones executed successfully, {0} zones returned", avoidanceZones.Length);
  else
    WriteLn(Format('GetAvoidanceZones error: "%s"', [ErrorString]));
end;

procedure TRoute4MeExamples.GetAvoidanceZones;
var
  ErrorString: String;
begin
  AvoidanceZoneQuery avoidanceZoneQuery = new AvoidanceZoneQuery()
  {

  };

  // Run the query
  AvoidanceZone[] avoidanceZones = Route4MeManager.GetAvoidanceZones(avoidanceZoneQuery, ErrorString);

  WriteLn('');

  if (avoidanceZones != null) then
    WriteLn(Format('GetAvoidanceZones executed successfully, {0} zones returned", avoidanceZones.Length);
  else
    WriteLn(Format('GetAvoidanceZones error: "%s"', [ErrorString]));
end;

procedure TRoute4MeExamples.GetOptimization(OptimizationProblemId: String);
var
  OptimizationParameters: TOptimizationParameters;
  DataObject: TDataObject;
  ErrorString: String;
begin
  OptimizationParameters := TOptimizationParameters.Create;
  try
    OptimizationParameters.OptimizationProblemID := OptimizationProblemId;

    // Run the query
    DataObject := Route4MeManager.Optimization.Get(OptimizationParameters, ErrorString);
    try
      WriteLn('');

      if (DataObject <> nil) then
      begin
          WriteLn('GetOptimization executed successfully');

          WriteLn(Format('Optimization Problem ID: %s', [DataObject.OptimizationProblemId]));
          WriteLn(Format('State: %s', [TOptimizationDescription[TOptimizationState(DataObject.State)]]));
      end
      else
        WriteLn(Format('GetOptimization error: "%s"', [ErrorString]));
    finally
      FreeAndNil(DataObject);
    end;
  finally
    FreeAndNil(OptimizationParameters);
  end;
end;

procedure TRoute4MeExamples.GetOptimizations;
var
  QueryParameters: TRouteParametersQuery;
  DataObjects: TArray<TDataObject>;
  DataObject: TDataObject;
  ErrorString: String;
  i: integer;
begin
  QueryParameters := TRouteParametersQuery.Create;
  try
    QueryParameters.Limit := 10;
    QueryParameters.Offset := 5;

    // Run the query
    DataObjects := Route4MeManager.Optimization.Get(QueryParameters, ErrorString);
    try
      WriteLn('');

      if Length(DataObjects) > 0 then
      begin
          WriteLn(Format('GetOptimizations executed successfully, %d optimizations returned', [Length(DataObjects)]));
          WriteLn('');

          for DataObject in DataObjects do
          begin
            WriteLn(Format('Optimization Problem ID: %s', [DataObject.OptimizationProblemId]));
            WriteLn('');
          end;
      end
      else
        WriteLn(Format('GetOptimizations error: "%s"', [ErrorString]));
    finally
      for i := Length(DataObjects) - 1 downto 0 do
        FreeAndNil(DataObjects[i]);
    end;
  finally
    FreeAndNil(QueryParameters);
  end;
end;

procedure TRoute4MeExamples.GetOrders;
var
  ErrorString: String;
begin
  OrderParameters orderParameters = new OrderParameters()
  {
    Limit = 10
  };

  uint total;
  Order[] orders = Route4MeManager.GetOrders(orderParameters, out total, ErrorString);

  WriteLn('');

  if (orders != null) then
    WriteLn(Format('GetOrders executed successfully, {0} orders returned, total = {1}", orders.Length, total);
  else
    WriteLn(Format('GetOrders error: "%s"', [ErrorString]));
end;

procedure TRoute4MeExamples.GetRoute(RouteId: String; GetRouteDirections,
  GetRoutePathPoints: boolean);
var
  RouteParameters: TRouteParametersQuery;
  ErrorString: String;
  DataObject: TDataObjectRoute;
begin
  RouteParameters := TRouteParametersQuery.Create;
  try
    RouteParameters.RouteId := RouteId;

    if (GetRouteDirections) then
      RouteParameters.Directions := True;

    if (GetRoutePathPoints) then
      RouteParameters.RoutePathOutput := TRoutePathOutputDescription[TRoutePathOutput.Points];

    // Run the query
    DataObject := Route4MeManager.Route.Get(RouteParameters, ErrorString);

    WriteLn('');

    if (DataObject <> nil) then
    begin
      WriteLn('GetRoute executed successfully');

      WriteLn(Format('Route ID: %s', [DataObject.RouteId]));
      WriteLn(Format('State: %s', [TOptimizationDescription[TOptimizationState(DataObject.State)]]));
      if (Length(DataObject.Directions) > 0) then
        WriteLn(Format('Directions: length = %d', [Length(DataObject.Directions)]));
      if (Length(DataObject.Path) > 0) then
        WriteLn(Format('Path: length = %d', [Length(DataObject.Path)]));
    end
    else
      WriteLn(Format('GetRoute error: "%s"', [ErrorString]));
  finally
    FreeAndNil(RouteParameters);
  end;
end;

procedure TRoute4MeExamples.GetRoutes;
var
  RouteParameters: TRouteParametersQuery;
  ErrorString: String;
  DataObjects: TArray<TDataObjectRoute>;
  DataObject: TDataObjectRoute;
begin
  RouteParameters := TRouteParametersQuery.Create();
  RouteParameters.Limit := 10;
  RouteParameters.Offset := 5;

  // Run the query
  DataObjects := Route4MeManager.Route.GetList(RouteParameters, ErrorString);

  WriteLn('');

  if (Length(DataObjects) > 0) then
  begin
    WriteLn(Format('GetRoutes executed successfully, %d routes returned', [Length(DataObjects)]));
    WriteLn('');

    for DataObject in DataObjects do
    begin
      WriteLn(Format('RouteId: %s', [DataObject.RouteId]));
      WriteLn('');
    end;
  end
  else
    WriteLn(Format('GetRoutes error "%s"', [ErrorString]));
end;

procedure TRoute4MeExamples.GetUsers;
var
  Parameters: TGenericParameters;
  ErrorString: String;
  DataObjects: TArray<TUser>;
begin
  Parameters := TGenericParameters.Create();
  try
    // Run the query
    DataObjects := Route4MeManager.User.Get(Parameters, ErrorString);

    WriteLn('');

    if (Length(DataObjects) > 0) then
    begin
      WriteLn(Format('GetUsers executed successfully, %d users returned', [Length(DataObjects)]));
      WriteLn('');
    end
    else
      WriteLn(Format('GetUsers error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Parameters);
  end;
end;

procedure TRoute4MeExamples.LogCustomActivity(Message, RouteId: String);
var
  Activity: TActivity;
  ErrorString: String;
  Added: boolean;
begin
  Activity := TActivity.Create();
  Activity.ActivityType := 'user_message';
  Activity.ActivityMessage := Message;
  Activity.RouteId := routeId;

  // Run the query
  Added := Route4MeManager.LogCustomActivity(activity, ErrorString);

  WriteLn('');

  if (Added) then
  begin
    WriteLn('LogCustomActivity executed successfully');
    return added;
  end
  else
  begin
    WriteLn(Format('LogCustomActivity error: "%s"', [ErrorString]));
    return added;
  end;
end;

procedure TRoute4MeExamples.MoveDestinationToRoute(ToRouteId: String;
  RouteDestinationId, AfterDestinationId: integer);
var
  ErrorString: String;
  Success: boolean;
begin
  // Run the query
  Success := Route4MeManager.Route.MoveDestinationToRoute(ToRouteId,
    RouteDestinationId, AfterDestinationId, ErrorString);

  WriteLn('');

  if (success) then
  begin
    WriteLn('MoveDestinationToRoute executed successfully');
    WriteLn(Format(
      'Destination %d moved to Route %s after Destination %d',
      [RouteDestinationId, ToRouteId, AfterDestinationId]));
  end
  else
    WriteLn(Format('MoveDestinationToRoute error: "%s"', [ErrorString]));
end;

function TRoute4MeExamples.MultipleDepotMultipleDriver: TDataObject;
var
  DataProvider: IOptimizationParametersProvider;
  ErrorString: String;
  DataObject: TDataObject;
  OptimizationParameters: TOptimizationParameters;
begin
  DataProvider := TMultipleDepotMultipleDriverTestDataProvider.Create;

  // Run the query
  OptimizationParameters := DataProvider.OptimizationParameters;
  try
    DataObject := Route4MeManager.Optimization.Run(OptimizationParameters, ErrorString);
  finally
    FreeAndNil(OptimizationParameters);
  end;

  // Output the result
  PrintExampleOptimizationResult('MultipleDepotMultipleDriver', DataObject, ErrorString);

  Result := DataObject;
end;

function TRoute4MeExamples.MultipleDepotMultipleDriverTimeWindow: TDataObject;
var
  DataProvider: IOptimizationParametersProvider;
  ErrorString: String;
  DataObject: TDataObject;
  OptimizationParameters: TOptimizationParameters;
begin
  DataProvider := TMultipleDepotMultipleDriverTimeWindowTestDataProvider.Create;

  // Run the query
  OptimizationParameters := DataProvider.OptimizationParameters;
  try
    DataObject := Route4MeManager.Optimization.Run(OptimizationParameters, ErrorString);
  finally
    FreeAndNil(OptimizationParameters);
  end;

  // Output the result
  PrintExampleOptimizationResult('MultipleDepotMultipleDriverTimeWindow', DataObject, ErrorString);

  Result := DataObject;
end;

function TRoute4MeExamples.MultipleDepotMultipleDriverWith24StopsTimeWindow: TDataObject;
var
  DataProvider: IOptimizationParametersProvider;
  ErrorString: String;
  DataObject: TDataObject;
  OptimizationParameters: TOptimizationParameters;
begin
  DataProvider := TMultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProvider.Create;

  // Run the query
  OptimizationParameters := DataProvider.OptimizationParameters;
  try
    DataObject := Route4MeManager.Optimization.Run(OptimizationParameters, ErrorString);
  finally
    FreeAndNil(OptimizationParameters);
  end;

  // Output the result
  PrintExampleOptimizationResult('MultipleDepotMultipleDriverWith24StopsTimeWindow', DataObject, ErrorString);

  Result := DataObject;
end;

procedure TRoute4MeExamples.PrintExampleOptimizationResult(ExampleName: String;
  DataObject: TDataObject; ErrorString: String);
var
  UserError: String;
  Address: TAddress;
  AddressesSorted: TAddressesArray;
begin
  Writeln('');

  if (DataObject <> nil) then
  begin
    WriteLn(Format('%s executed successfully', [ExampleName]));
    WriteLn('');

    WriteLn(Format('Optimization Problem ID: %s', [DataObject.OptimizationProblemId]));
    WriteLn(Format('State: %s', [TOptimizationDescription[TOptimizationState(DataObject.State)]]));

    for UserError in DataObject.UserErrors do
      WriteLn(Format('UserError : "$s"', [UserError]));

    WriteLn('');

    // Sort addresses by sequence order
    AddressesSorted := AddressUnit.SortAddresses(DataObject.Addresses);
    for Address in AddressesSorted do
    begin
      WriteLn(Format('Address: %s', [Address.AddressString]));
      WriteLn(Format('Route ID: %s', [Address.RouteId.ToString]));
    end;
  end
  else
    WriteLn(Format('%s error: "%s"', [ExampleName, ErrorString]));
end;

procedure TRoute4MeExamples.RemoveAddressBookContacts(ContactIds: TStringArray);
var
  ErrorString: String;
begin
  // Run the query
  bool removed = Route4MeManager.RemoveAddressBookContacts(addressIds, ErrorString);

  WriteLn('');

  if (Removed) then
    WriteLn(Format('RemoveAddressBookContacts executed successfully, {0} contacts deleted", addressIds.Length);
  else
    WriteLn(Format('RemoveAddressBookContacts error: "%s"', [ErrorString]));
end;

procedure TRoute4MeExamples.RemoveDestinationFromOptimization(
  OptimizationId: String; DestinationId: integer; AndReOptimize: boolean);
var
  ErrorString: String;
  Removed: boolean;
begin
  // Run the query
  Removed := Route4MeManager.Optimization.RemoveDestination(OptimizationId, DestinationId, ErrorString);

  WriteLn('');

  if (Removed) then
  begin
    WriteLn('RemoveAddressFromOptimization executed successfully');

    WriteLn(Format('Optimization Problem ID: %s, Destination ID: %d', [OptimizationId, DestinationId]));
  end
  else
    WriteLn(Format('RemoveAddressFromOptimization error: %s', [ErrorString]));
end;

procedure TRoute4MeExamples.RemoveOptimization(OptimizationProblemId: String);
var
  ErrorString: String;
begin
  // Run the query
  bool removed = Route4MeManager.RemoveOptimization(optimizationProblemID, ErrorString);

  WriteLn('');

  if (removed) then
  begin
    WriteLn('RemoveOptimization executed successfully');

    WriteLn(Format('Optimization Problem ID: {0}", optimizationProblemID);
  end
  else
    WriteLn(Format('RemoveOptimization error: "%s"', [ErrorString]));
end;

procedure TRoute4MeExamples.RemoveOrders(OrderIds: TStringArray);
var
  ErrorString: String;
begin
  // Run the query
  bool removed = Route4MeManager.RemoveOrders(orderIds, ErrorString);

  WriteLn('');

  if (removed) then
    WriteLn(Format('RemoveOrders executed successfully, {0} orders removed", orderIds.Length);
  else
    WriteLn(Format('RemoveOrders error: "%s"', [ErrorString]));
end;

procedure TRoute4MeExamples.RemoveRouteDestination(RouteId: String;
  DestinationId: integer);
var
  ErrorString: String;
  Deleted: boolean;
begin
  // Run the query
  Deleted := Route4MeManager.Route.Remove(RouteId, DestinationId, ErrorString);

  WriteLn('');

  if (deleted) then
  begin
    WriteLn('RemoveRouteDestination executed successfully');
    WriteLn(Format('Destination ID: %d', [DestinationId]));
  end
  else
    WriteLn(Format('RemoveRouteDestination error: "%s"', [ErrorString]));
end;

procedure TRoute4MeExamples.ReOptimization(OptimizationProblemId: String);
var
  OptimizationParameters: TOptimizationParameters;
  ErrorString: String;
  DataObject: TDataObject;
begin
  OptimizationParameters := TOptimizationParameters.Create;
  OptimizationParameters.OptimizationProblemID := OptimizationProblemId;
  OptimizationParameters.ReOptimize := True;

  // Run the query
  DataObject := Route4MeManager.Optimization.Update(OptimizationParameters, ErrorString);

  WriteLn('');

  if (DataObject <> nil) then
  begin
    WriteLn('ReOptimization executed successfully');

    WriteLn(Format('Optimization Problem ID: %s', [DataObject.OptimizationProblemId]));
    WriteLn(Format('State: %s', [TOptimizationDescription[TOptimizationState(DataObject.State)]]));
  end
  else
    WriteLn(Format('ReOptimization error: "%s"', [ErrorString]));
end;

procedure TRoute4MeExamples.ReoptimizeRoute(RouteId: String);
var
  RouteParameters: TRouteParametersQuery;
  ErrorString: String;
  DataObject: TDataObjectRoute;
begin
  RouteParameters := TRouteParametersQuery.Create;
  try
    RouteParameters.RouteId := RouteId;
    RouteParameters.ReOptimize := True;

    // Run the query
    DataObject := Route4MeManager.Route.Update(RouteParameters, ErrorString);

    WriteLn('');

    if (DataObject <> nil) then
    begin
      WriteLn('ReoptimizeRoute executed successfully');

      WriteLn(Format('Route ID: %s', [DataObject.RouteId]));
      WriteLn(Format('State: %s', [TOptimizationDescription[TOptimizationState(DataObject.State)]]));
    end
    else
      WriteLn(Format('ReoptimizeRoute error: "%s"', [ErrorString]));
  finally
    FreeAndNil(RouteParameters);
  end;
end;

procedure TRoute4MeExamples.ResequenceRouteDestinations(
  Route: TDataObjectRoute);
var
  AddressesOrderInfo: TAddressesOrderInfo;
  Address: TAddress;
  AddressInfo: TAddressInfo;
  i: integer;
  SequenceNo: integer;
  ErrorString: String;
  NewRoute: TDataObjectRoute;
begin
  if (Length(Route.Addresses) < 3) then
  begin
    WriteLn(Format('ResequenceRouteDestinations error: "%s"',
      ['Route.Addresses.Length < 3. Number of addresses should be >= 3']));
    Exit;
   end;

  // Switch 2 addresses after departure address:
  AddressesOrderInfo := TAddressesOrderInfo.Create(Route.RouteID);
  try
    for i := 0 to Length(Route.Addresses) - 1 do
    begin
      Address := Route.Addresses[i];

      SequenceNo := i;
      if (i = 1) then
        SequenceNo := 2
      else
        if (i = 2) then
          SequenceNo := 1;

      AddressInfo := TAddressInfo.Create;
      AddressInfo.DestinationId := Address.RouteDestinationId.Value;
      AddressInfo.SequenceNo := SequenceNo;
      AddressInfo.IsDepot := (AddressInfo.SequenceNo = 0);

      AddressesOrderInfo.AddAddress(AddressInfo);
    end;

    // Run the query
    NewRoute := Route4MeManager.Route.Resequence(AddressesOrderInfo, ErrorString);
    try
      // Output the result
      PrintExampleOptimizationResult('ResequenceRouteDestinations, switch 2 addresses.', NewRoute, ErrorString);
      WriteLn('');
    finally
      FreeAndNil(NewRoute);
    end;
  finally
    FreeAndNil(AddressesOrderInfo);
  end;
end;

procedure TRoute4MeExamples.SetGPSPosition(RouteId: String);
var
  ErrorString: String;
begin
  // Create the gps parametes
  GPSParameters gpsParameters = new GPSParameters()
  {
    Format = Format.Csv.Description(),
    RouteId = routeId,
    Latitude = 33.14384,
    Longitude = -83.22466,
    Course = 1,
    Speed = 120,
    DeviceType = DeviceType.IPhone.Description(),
    MemberId = 1,
    DeviceGuid = "TEST_GPS",
    DeviceTimestamp = "2014-06-14 17:43:35"
  };

  string response = Route4MeManager.SetGPS(gpsParameters, ErrorString);

  WriteLn('');

  if (string.IsNullOrEmpty(errorString)) then
    WriteLn(Format('SetGps response: {0}", response)
  else
    WriteLn(Format('SetGps error: "%s"', [ErrorString]));
end;

function TRoute4MeExamples.SingleDepotMultipleDriverNoTimeWindow: TDataObject;
var
  DataProvider: IOptimizationParametersProvider;
  ErrorString: String;
  DataObject: TDataObject;
  OptimizationParameters: TOptimizationParameters;
begin
  DataProvider := TSingleDepotMultipleDriverNoTimeWindowTestDataProvider.Create;

  // Run the query
  OptimizationParameters := DataProvider.OptimizationParameters;
  try
    DataObject := Route4MeManager.Optimization.Run(OptimizationParameters, ErrorString);
  finally
    FreeAndNil(OptimizationParameters);
  end;

  // Output the result
  PrintExampleOptimizationResult('SingleDepotMultipleDriverNoTimeWindow', DataObject, ErrorString);

  Result := DataObject;
end;

function TRoute4MeExamples.SingleDriverMultipleTimeWindows: TDataObject;
var
  DataProvider: IOptimizationParametersProvider;
  ErrorString: String;
  DataObject: TDataObject;
  OptimizationParameters: TOptimizationParameters;
begin
  DataProvider := TSingleDriverMultipleTimeWindowsTestDataProvider.Create;

  // Run the query
  OptimizationParameters := DataProvider.OptimizationParameters;
  try
    DataObject := Route4MeManager.Optimization.Run(OptimizationParameters, ErrorString);
  finally
    FreeAndNil(OptimizationParameters);
  end;

  // Output the result
  PrintExampleOptimizationResult('SingleDriverMultipleTimeWindows', DataObject, ErrorString);

  Result := DataObject;
end;

function TRoute4MeExamples.SingleDriverRoundTrip: TDataObject;
var
  DataProvider: IOptimizationParametersProvider;
  ErrorString: String;
  DataObject: TDataObject;
  OptimizationParameters: TOptimizationParameters;
begin
  DataProvider := TSingleDriverRoundTripTestDataProvider.Create;

  // Run the query
  OptimizationParameters := DataProvider.OptimizationParameters;
  try
    DataObject := Route4MeManager.Optimization.Run(OptimizationParameters, ErrorString);
  finally
    FreeAndNil(OptimizationParameters);
  end;

  // Output the result
  PrintExampleOptimizationResult('SingleDriverRoundTrip', DataObject, ErrorString);

  Result := DataObject;
end;

function TRoute4MeExamples.SingleDriverRoundTripGeneric: NullableString;
var
  DataProvider: IOptimizationParametersProvider;
  ErrorString: String;
  Request: TSingleDriverRoundTripGenericRequest;
  Responce: TSingleDriverRoundTripGenericResponse;
  Address: TAddress;
  OptimizationParameters: TOptimizationParameters;
begin
  Result := NullableString.Null;

  DataProvider := TSingleDriverRoundTripGenericTestDataProvider.Create;
  Request := TSingleDriverRoundTripGenericRequest.Create;
  try
    OptimizationParameters := DataProvider.OptimizationParameters;
    try
      Request.Parameters := OptimizationParameters.Parameters;
      Request.Addresses := OptimizationParameters.Addresses;

      // Run the query
      Responce := Route4MeManager.Connection.Post(TSettings.ApiHost, Request,
        TSingleDriverRoundTripGenericResponse, errorString) as TSingleDriverRoundTripGenericResponse;

      WriteLn('');

      if (Responce <> nil) then
      begin
        WriteLn('SingleDriverRoundTripGeneric executed successfully');
        WriteLn('');

        WriteLn(Format('Optimization Problem ID: %s', [Responce.OptimizationProblemId]));
        WriteLn(Format('State: %s', [TOptimizationDescription[TOptimizationState(Responce.MyState)]]));
        WriteLn('');

        for Address in Responce.Addresses do
        begin
          WriteLn(Format('Address: %s', [Address.AddressString]));
          WriteLn(Format('Route ID: %s', [Address.RouteId.ToString]));
        end;

        Result := Responce.OptimizationProblemId;
      end
      else
        WriteLn(Format('SingleDriverRoundTripGeneric error "%s"', [ErrorString]));
    finally
      FreeAndNil(OptimizationParameters);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

function TRoute4MeExamples.SingleDriverRoute10Stops: TDataObject;
var
  DataProvider: IOptimizationParametersProvider;
  ErrorString: String;
  DataObject: TDataObject;
  OptimizationParameters: TOptimizationParameters;
begin
  DataProvider := TSingleDriverRoute10StopsTestDataProvider.Create;

  // Run the query
  OptimizationParameters := DataProvider.OptimizationParameters;
  try
    DataObject := Route4MeManager.Optimization.Run(OptimizationParameters, ErrorString);
  finally
    FreeAndNil(OptimizationParameters);
  end;

  // Output the result
  PrintExampleOptimizationResult('SingleDriverRoute10Stops', DataObject, ErrorString);

  Result := DataObject;
end;

procedure TRoute4MeExamples.TrackDeviceLastLocationHistory(RouteId: String);
var
  ErrorString: String;
begin
  // Create the gps parametes
  GPSParameters gpsParameters = new GPSParameters()
  {
    Format          = Format.Csv.Description(),
    RouteId         = routeId,
    Latitude        = 33.14384,
    Longitude       = -83.22466,
    Course          = 1,
    Speed           = 120,
    DeviceType      = DeviceType.IPhone.Description(),
    MemberId        = 1,
    DeviceGuid      = "TEST_GPS",
    DeviceTimestamp = "2014-06-14 17:43:35"
  };

  string response = Route4MeManager.SetGPS(gpsParameters, ErrorString);

  if (!string.IsNullOrEmpty(errorString)) then
  begin
    WriteLn('SetGps error: "%s"', [ErrorString]));
    return;
  end;

  WriteLn(Format('SetGps response: {0}", response);

  GenericParameters genericParameters = new GenericParameters();
  genericParameters.ParametersCollection.Add("route_id", routeId);
  genericParameters.ParametersCollection.Add("device_tracking_history", "1');

  var dataObject = Route4MeManager.GetLastLocation(genericParameters, ErrorString);

  WriteLn('');

  if (dataObject != null) then
  begin
    WriteLn('TrackDeviceLastLocationHistory executed successfully');
    WriteLn('');

    WriteLn(Format('Optimization Problem ID: {0}", dataObject.OptimizationProblemId);
    WriteLn(Format('State: {0}", dataObject.State);
    WriteLn('');

    dataObject.TrackingHistory.ForEach(th =>
    {
      WriteLn(Format('Speed: {0}",      th.Speed);
      WriteLn(Format('Longitude: {0}",  th.Longitude);
      WriteLn(Format('Latitude: {0}",   th.Latitude);
      WriteLn(Format('Time Stamp: {0}", th.TimeStampFriendly);
      WriteLn('');
    });
  end
  else
    WriteLn(Format('TrackDeviceLastLocationHistory error: "%s"', [ErrorString]));
end;

procedure TRoute4MeExamples.UpdateAddressBookContact(
  Contact: TAddressBookContact);
var
  ErrorString: String;
begin
  // Run the query
  AddressBookContact updatedContact = Route4MeManager.UpdateAddressBookContact(
    Contact, ErrorString);

  WriteLn('');

  if (updatedContact != null) then
    WriteLn('UpdateAddressBookContact executed successfully')
  else
    WriteLn(Format('UpdateAddressBookContact error: "%s"', [ErrorString]));
end;

procedure TRoute4MeExamples.UpdateAvoidanceZone(TerritoryId: String);
var
  ErrorString: String;
begin
  AvoidanceZoneParameters avoidanceZoneParameters = new AvoidanceZoneParameters()
  {
    TerritoryId = territoryId,
    TerritoryName = "Test Territory Updated",
    TerritoryColor = "ff00ff",
    Territory = new Territory()
    {
      Type = TerritoryType.Circle.Description(),
      Data = new string[] { "38.41322259056806,-78.501953234",
                            "3000"}
    }
  };

  // Run the query
  AvoidanceZone avoidanceZone = Route4MeManager.UpdateAvoidanceZone(avoidanceZoneParameters, ErrorString);

  WriteLn('');

  if (avoidanceZone != null) then
  begin
    WriteLn('UpdateAvoidanceZone executed successfully');

    WriteLn(Format('Territory ID: {0}", avoidanceZone.TerritoryId);
  end
  else
    WriteLn(Format('UpdateAvoidanceZone error: "%s"', [ErrorString]));
end;

procedure TRoute4MeExamples.UpdateOrder(Order: TOrder);
var
  ErrorString: String;
begin
  // Run the query
  Order updatedOrder = Route4MeManager.UpdateOrder(order, ErrorString);

  WriteLn('');

  if (updatedOrder != null) then
    WriteLn('UpdateOrder executed successfully');
  else
    WriteLn(Format('UpdateOrder error: "%s"', [ErrorString]));
end;

procedure TRoute4MeExamples.UpdateRoute(RouteId: String);
var
  ParametersNew: TRouteParameters;
  RouteParameters: TRouteParametersQuery;
  ErrorString: String;
  DataObject: TDataObjectRoute;
begin
  ParametersNew := TRouteParameters.Create;
  ParametersNew.RouteName := 'New name of the route';

  RouteParameters := TRouteParametersQuery.Create;
  RouteParameters.RouteId := RouteId;
  RouteParameters.Parameters := ParametersNew;

  // Run the query
  DataObject := Route4MeManager.Route.Update(RouteParameters, ErrorString);

  WriteLn('');

  if (DataObject <> nil) then
  begin
    WriteLn('UpdateRoute executed successfully');

    WriteLn(Format('Route ID: %s', [DataObject.RouteId]));
    WriteLn(Format('State: %s', [TOptimizationDescription[TOptimizationState(DataObject.State)]]));
  end
  else
    WriteLn(Format('UpdateRoute error: %s', [ErrorString]));
end;

procedure TRoute4MeExamples.WriteLn(Message: String);
begin
  FOutput.Writeln(Message);
end;

end.
