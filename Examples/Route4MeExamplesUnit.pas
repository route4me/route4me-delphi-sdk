unit Route4MeExamplesUnit;

interface

uses
  SysUtils, System.Generics.Collections,
  Route4MeManagerUnit, DataObjectUnit, NullableBasicTypesUnit, UtilsUnit,
  CommonTypesUnit, AddressBookContactUnit,
  SingleDriverRoundTripTestDataProviderUnit, IRoute4MeManagerUnit, OutputUnit,
  IConnectionUnit, RouteParametersUnit, OrderUnit, AddressUnit;

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
    function AddRouteDestinationsOptimally(RouteId: String): TArray<integer>;
    procedure RemoveRouteDestination(RouteId: String; DestinationId: integer);
    function SingleDriverRoundTrip: TDataObject;
    procedure MoveDestinationToRoute(ToRouteId: String;
      RouteDestinationId, AfterDestinationId: integer);
    function SingleDriverRoundTripGeneric: NullableString;
    function MultipleDepotMultipleDriver: TDataObject;
    function MultipleDepotMultipleDriverTimeWindow: TDataObject;
    function SingleDepotMultipleDriverNoTimeWindow: TDataObject;
    function MultipleDepotMultipleDriverWith24StopsTimeWindow: TDataObject;
    function SingleDriverMultipleTimeWindows: TDataObject;
    procedure GetOptimization(OptimizationProblemId: String);
    procedure GetOptimizations;
    function AddDestinationToOptimization(OptimizationId: String;
      AndReOptimize: boolean): TDataObject;
    procedure RemoveDestinationFromOptimization(OptimizationId: String;
      DestinationId: integer; AndReOptimize: boolean);
    procedure ReOptimization(OptimizationProblemId: String);
    procedure UpdateRoute(RouteId: String);
    procedure ReoptimizeRoute(RouteId: String);
    procedure GetRoute(RouteId: String; GetRouteDirections, GetRoutePathPoints: boolean);
    procedure GetRoutes();
    procedure GetUsers();
    function LogCustomActivity(Message: String; RouteId: String): boolean;
    procedure GetActivities(RouteId: String);
    procedure GetAddress(RouteId: String; RouteDestinationId: integer);
    procedure AddAddressNote(RouteId: String; AddressId: integer);
    procedure GetAddressNotes(RouteId: String; RouteDestinationId: integer);
    function DuplicateRoute(RouteId: String): NullableString;
    procedure ShareRoute(RouteId: String; RecipientEmail: String);
    procedure SetGPSPosition(RouteId: String);
    procedure TrackDeviceLastLocationHistory(RouteId: String);
    procedure DeleteRoutes(RouteIds: TStringArray);
    procedure RemoveOptimization(OptimizationProblemId: String);
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
    procedure GetOrders;
    procedure UpdateOrder(Order: TOrder);
    procedure RemoveOrders(OrderIds: TStringArray);
    procedure GenericExample(Connection: IConnection);
    procedure GenericExampleShortcut(Connection: IConnection);
  end;

implementation

{ TRoute4MeExamples }

uses
  EnumsUnit, IOptimizationParametersProviderUnit,
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
  RouteParametersQueryUnit, UserUnit, NoteParametersUnit, AddressNoteUnit,
  AvoidanceZoneParametersUnit, TerritoryUnit, AvoidanceZoneUnit,
  AvoidanceZoneQueryUnit, ConnectionUnit, ActivityParametersUnit, ActivityUnit,
  AddressParametersUnit, AddressBookParametersUnit, OrderParametersUnit,
  GPSParametersUnit, TrackingHistoryUnit;

function TRoute4MeExamples.AddAddressBookContact(
  FirstName, Address: String): TAddressBookContact;
var
  ErrorString: String;
  Contact: TAddressBookContact;
begin
  Contact := TAddressBookContact.Create();
  try
    Contact.FirstName := FirstName;
    Contact.Address := Address;
    Contact.Latitude := 38.024654;
    Contact.Longitude := -77.338814;

    // Run the query
    Result := Route4MeManager.AddressBookContact.Add(Contact, ErrorString);

    WriteLn('');

    if (Result <> nil) then
    begin
      WriteLn('AddAddressBookContact executed successfully');
      WriteLn(Format('AddressId: %d', [Result.Id.Value]));
    end
    else
      WriteLn(Format('AddAddressBookContact error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Contact);
  end;
end;

procedure TRoute4MeExamples.AddAddressNote(RouteId: String; AddressId: integer);
var
  ErrorString: String;
  Parameters: TNoteParameters;
  Contents: String;
  Note: TAddressNote;
begin
  Parameters := TNoteParameters.Create();
  try
    Parameters.RouteId := RouteId;
    Parameters.AddressId := AddressId;
    Parameters.Latitude := 33.132675170898;
    Parameters.Longitude := -83.244743347168;
    Parameters.DeviceType := TDeviceTypeDescription[TDeviceType.Web];
    Parameters.ActivityType := TStatusUpdateTypeDescription[TStatusUpdateType.DropOff];

    // Run the query
    Contents := 'Test Note Contents ' + DateTimeToStr(Now);
    Note := Route4MeManager.AddressNote.Add(Parameters, Contents, ErrorString);
    try
      WriteLn('');

      if (Note <> nil) then
      begin
        WriteLn('AddAddressNote executed successfully');
        WriteLn(Format('Note ID: %d', [Note.NoteId.Value]));
      end
      else
        WriteLn(Format('AddAddressNote error: "%s"', [ErrorString]));
    finally
      FreeAndNil(Note);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

function TRoute4MeExamples.AddAvoidanceZone: NullableString;
var
  ErrorString: String;
  Parameters: TAvoidanceZoneParameters;
  Territory: TTerritory;
  AvoidanceZone: TAvoidanceZone;
begin
  Result := NullableString.Null;

  Parameters := TAvoidanceZoneParameters.Create();
  try
    Parameters.TerritoryName := 'Test Territory';
    Parameters.TerritoryColor := 'ff0000';
    Territory := TTerritory.Create;
    Territory.TerritoryType := TTerritoryType.ttCircle;
    Territory.AddDataItem('37.569752822786455,-77.47833251953125');
    Territory.AddDataItem('5000');
    Parameters.Territory := Territory;

    // Run the query
    AvoidanceZone := Route4MeManager.AvoidanceZone.Add(Parameters, ErrorString);
    try
      WriteLn('');

      if (AvoidanceZone <> nil) then
      begin
        WriteLn('AddAvoidanceZone executed successfully');
        WriteLn(Format('Territory ID: %s', [AvoidanceZone.TerritoryId.Value]));

        Result := AvoidanceZone.TerritoryId;
      end
      else
        WriteLn(Format('AddAvoidanceZone error: "%s"', [ErrorString]));
    finally
      FreeAndNil(AvoidanceZone);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

function TRoute4MeExamples.AddDestinationToOptimization(
  OptimizationId: String; AndReOptimize: boolean): TDataObject;
var
  Address: TAddress;
  Parameters: TOptimizationParameters;
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
  Parameters := TOptimizationParameters.Create;
  try
    Parameters.OptimizationProblemID := OptimizationId;
    Parameters.AddAddress(Address);
    Parameters.ReOptimize := AndReOptimize;

    // Execute the optimization to re-optimize and rebalance all the routes in this optimization
    Result := Route4MeManager.Optimization.Update(Parameters, ErrorString);

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
    FreeAndNil(Parameters);
  end;
end;

function TRoute4MeExamples.AddOrder(): TOrder;
var
  ErrorString: String;
  Order: TOrder;
begin
  Order := TOrder.Create();
  try
    Order.Address1 := 'Test Address1';
    Order.AddressAlias := 'Test AddressAlias';
    Order.CachedLatitude := 37.773972;
    Order.CachedLongitude := -122.431297;

    // Run the query
    Result := Route4MeManager.Order.Add(Order, ErrorString);

    WriteLn('');

    if (Result <> nil) then
    begin
      WriteLn('AddOrder executed successfully');
      WriteLn(Format('Order ID: %s', [Result.OrderId.Value]));
    end
    else
      WriteLn(Format('AddOrder error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Order);
  end;
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
  SetLength(Addresses, 1);
  try
    Addresses[0] := TAddress.Create(
      '146 Bill Johnson Rd NE Milledgeville GA 31061',
      33.143526, -83.240354, 0);
    Addresses[0].SequenceNo := 4;

    // Run the query
    OptimalPosition := False;
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

function TRoute4MeExamples.AddRouteDestinationsOptimally(
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
  // Run the query
  Route4MeManager.AvoidanceZone.Delete(TerritoryId, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
  begin
    WriteLn('DeleteAvoidanceZone executed successfully');
    WriteLn(Format('Territory ID: %s', [TerritoryId]));
  end
  else
    WriteLn(Format('DeleteAvoidanceZone error: "%s"', [ErrorString]));
end;

procedure TRoute4MeExamples.DeleteRoutes(RouteIds: TStringArray);
var
  ErrorString: String;
  DeletedRouteIds: TStringArray;
begin
  // Run the query
  DeletedRouteIds := Route4MeManager.Route.Delete(RouteIds, ErrorString);

  WriteLn('');

  if (Length(DeletedRouteIds) > 0) then
  begin
    WriteLn(Format('DeleteRoutes executed successfully, %d routes deleted', [Length(DeletedRouteIds)]));
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
  Parameters: TRouteParametersQuery;
begin
  Parameters := TRouteParametersQuery.Create;
  try
    Parameters.RouteId := RouteId;

    // Run the query
    Result := Route4MeManager.Route.Duplicate(Parameters, ErrorString);

    WriteLn('');

    if (Result.IsNotNull) then
    begin
      WriteLn(Format('DuplicateRoute executed successfully, duplicated route ID: %s', [Result.Value]));
      WriteLn('');
    end
    else
      WriteLn(Format('DuplicateRoute error "%s"', [ErrorString]));
  finally
    FreeAndNil(Parameters);
  end;
end;

procedure TRoute4MeExamples.GenericExample(Connection: IConnection);
var
  Parameters: TGenericParameters;
  DataObjects: TDataObjectRouteList;
  Route: TDataObjectRoute;
  Uri: String;
  Route4Me: TRoute4MeManager;
  ErrorMessage: String;
begin
  Route4Me := TRoute4MeManager.Create(Connection);
  try
    Parameters := TGenericParameters.Create();
    try
      //number of records per page
      Parameters.AddParameter('limit', '10');
      //the page offset starting at zero
      Parameters.AddParameter('Offset', '5');

      Uri := TSettings.MainHost + '/api.v4/route.php';
      DataObjects := Route4Me.Connection.Get(
        Uri, Parameters, TDataObjectRouteList, ErrorMessage) as TDataObjectRouteList;
      try
        WriteLn('');

        if (DataObjects <> nil) then
        begin
          WriteLn(Format('GenericExample executed successfully, %d routes returned', [DataObjects.Count]));
          WriteLn('');

          for Route in DataObjects do
            WriteLn(Format('RouteID: %s', [Route.RouteID]));
        end
        else
          WriteLn(Format('GenericExample error "%s"', [ErrorMessage]));
      finally
        FreeAndNil(DataObjects);
      end;
    finally
      FreeAndNil(Parameters);
    end;
  finally
    FreeAndNil(Route4Me);
  end;
end;

procedure TRoute4MeExamples.GenericExampleShortcut(Connection: IConnection);
var
  ErrorMessage: String;
  Parameters: TRouteParametersQuery;
  Routes: TArray<TDataObjectRoute>;
  Route: TDataObjectRoute;
  Route4Me: TRoute4MeManager;
begin
  Route4Me := TRoute4MeManager.Create(Connection);
  try
    Parameters := TRouteParametersQuery.Create();
    try
      Parameters.Limit := 10;
      Parameters.Offset := 5;

      Routes := Route4Me.Route.GetList(Parameters, ErrorMessage);

      if (Length(Routes) > 0) then
      begin
        WriteLn(Format('GenericExampleShortcut executed successfully, %d routes returned', [Length(Routes)]));
        WriteLn('');

        for Route in Routes do
          WriteLn(Format('RouteID: %s', [Route.RouteId]));
      end
      else
        WriteLn(Format('GenericExampleShortcut error "%s"', [ErrorMessage]));
    finally
      FreeAndNil(Parameters);
    end;
  finally
    FreeAndNil(Route4Me);
  end;
end;

procedure TRoute4MeExamples.GetActivities(RouteId: String);
var
  ErrorString: String;
  Parameters: TActivityParameters;
  Activities: TActivityArray;
  Activity: TActivity;
  i: integer;
begin
  Parameters := TActivityParameters.Create();
  try
    Parameters.RouteId := routeId;
    Parameters.Limit := 10;
    Parameters.Offset := 0;

    // Run the query
    Activities := Route4MeManager.Activity.GetActivityFeed(Parameters, ErrorString);
    try
      WriteLn('');

      if (Length(Activities) > 0) then
      begin
        WriteLn(Format('GetActivities executed successfully, %d activities returned', [Length(Activities)]));
        WriteLn('');

        for Activity in Activities do
          WriteLn(Format('Activity id: %s', [Activity.ActivityId.Value]));

        WriteLn('');
      end
      else
        WriteLn(Format('GetActivities error: "%s"', [ErrorString]));
    finally
      for i := Length(Activities) - 1 downto 0 do
        FreeAndNil(Activities[i]);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

procedure TRoute4MeExamples.GetAddress(RouteId: String; RouteDestinationId: integer);
var
  ErrorString: String;
  Parameters: TAddressParameters;
  Address: TAddress;
begin
  Parameters := TAddressParameters.Create();
  try
    Parameters.RouteId := RouteId;
    Parameters.RouteDestinationId := RouteDestinationId;
    Parameters.Notes := True;

    // Run the query
    Address := Route4MeManager.Address.Get(Parameters, ErrorString);
    try
      WriteLn('');

      if (Address <> nil) then
      begin
        WriteLn('GetAddress executed successfully');
        WriteLn(Format('RouteId: %s; RouteDestinationId: %d', [Address.RouteId.Value, Address.RouteDestinationId.Value]));
      end
      else
        WriteLn(Format('GetAddress error: "%s"', [ErrorString]));
    finally
      FreeAndNil(Address);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

procedure TRoute4MeExamples.GetAddressBookContacts;
var
  ErrorString: String;
  Parameters: TAddressBookParameters;
  Total: integer;
  Contacts: TAddressBookContactArray;
  i: integer;
begin
  Parameters := TAddressBookParameters.Create();
  try
    Parameters.Limit := 10;
    Parameters.Offset := 0;

    // Run the query
    Contacts := Route4MeManager.AddressBookContact.Get(Parameters, Total, ErrorString);
    try
      WriteLn('');

      if (Length(Contacts) > 0) then
      begin
        WriteLn(Format('GetAddressBookContacts executed successfully, %d contacts returned, total = %d',
          [Length(Contacts), Total]));
        WriteLn('');
      end
      else
        WriteLn(Format('GetAddressBookContacts error: "%s"', [ErrorString]));
    finally
      for i := Length(Contacts) - 1 downto 0 do
        FreeAndNil(Contacts[i]);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

procedure TRoute4MeExamples.GetAddressNotes(RouteId: String;
  RouteDestinationId: integer);
var
  ErrorString: String;
  Parameters: TNoteParameters;
  Notes: TAddressNoteArray;
  i: integer;
begin
  Parameters := TNoteParameters.Create();
  try
    Parameters.RouteId := RouteId;
    Parameters.AddressId := RouteDestinationId;

    // Run the query
    Notes := Route4MeManager.AddressNote.Get(Parameters, ErrorString);
    try
      WriteLn('');

      if (Notes <> nil) then
        WriteLn(Format('GetAddressNotes executed successfully, %d notes returned', [Length(Notes)]))
      else
        WriteLn(Format('GetAddressNotes error: "%s"', [ErrorString]));

      WriteLn('');
    finally
      for i := Length(Notes) - 1 downto 0 do
        FreeAndNil(Notes[i]);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

procedure TRoute4MeExamples.GetAvoidanceZone(TerritoryId: String);
var
  ErrorString: String;
  AvoidanceZone: TAvoidanceZone;
begin
  // Run the query
  AvoidanceZone := Route4MeManager.AvoidanceZone.Get(TerritoryId, ErrorString);
  try
    WriteLn('');

    if (AvoidanceZone <> nil) then
    begin
      WriteLn('GetAvoidanceZone executed successfully');
      WriteLn(Format('Territory ID: %s', [AvoidanceZone.TerritoryId.Value]));
    end
    else
      WriteLn(Format('GetAvoidanceZone error: %s', [ErrorString]));
  finally
    FreeAndNil(AvoidanceZone);
  end;
end;

procedure TRoute4MeExamples.GetAvoidanceZones;
var
  ErrorString: String;
  AvoidanceZones: TAvoidanceZoneArray;
  i: integer;
begin
  // Run the query
  AvoidanceZones := Route4MeManager.AvoidanceZone.GetList(ErrorString);
  try
    WriteLn('');

    if (Length(AvoidanceZones) > 0) then
      WriteLn(Format('GetAvoidanceZones executed successfully, %d zones returned', [Length(AvoidanceZones)]))
    else
      WriteLn(Format('GetAvoidanceZones error: "%s"', [ErrorString]));
  finally
    for i := Length(AvoidanceZones) - 1 downto 0 do
      FreeAndNil(AvoidanceZones[i]);
  end;
end;

procedure TRoute4MeExamples.GetOptimization(OptimizationProblemId: String);
var
  Parameters: TOptimizationParameters;
  DataObject: TDataObject;
  ErrorString: String;
begin
  Parameters := TOptimizationParameters.Create;
  try
    Parameters.OptimizationProblemID := OptimizationProblemId;

    // Run the query
    DataObject := Route4MeManager.Optimization.Get(Parameters, ErrorString);
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
    FreeAndNil(Parameters);
  end;
end;

procedure TRoute4MeExamples.GetOptimizations;
var
  Parameters: TRouteParametersQuery;
  DataObjects: TArray<TDataObject>;
  DataObject: TDataObject;
  ErrorString: String;
  i: integer;
begin
  Parameters := TRouteParametersQuery.Create;
  try
    Parameters.Limit := 10;
    Parameters.Offset := 5;

    // Run the query
    DataObjects := Route4MeManager.Optimization.Get(Parameters, ErrorString);
    try
      WriteLn('');

      if Length(DataObjects) > 0 then
      begin
          WriteLn(Format('GetOptimizations executed successfully, %d optimizations returned', [Length(DataObjects)]));
          WriteLn('');

          for DataObject in DataObjects do
            WriteLn(Format('Optimization Problem ID: %s', [DataObject.OptimizationProblemId]));
      end
      else
        WriteLn(Format('GetOptimizations error: "%s"', [ErrorString]));
    finally
      for i := Length(DataObjects) - 1 downto 0 do
        FreeAndNil(DataObjects[i]);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

procedure TRoute4MeExamples.GetOrders;
var
  ErrorString: String;
  Parameters: TOrderParameters;
  Total: integer;
  Orders: TOrderArray;
  i: integer;
begin
  Parameters := TOrderParameters.Create();
  try
    Parameters.Limit := 10;

    Orders := Route4MeManager.Order.Get(Parameters, Total, ErrorString);
    try
      WriteLn('');

      if (Length(Orders) > 0) then
        WriteLn(Format('GetOrders executed successfully, %d orders returned, total = %d',
          [Length(Orders), Total]))
      else
        WriteLn(Format('GetOrders error: "%s"', [ErrorString]));
    finally
      for i := Length(Orders) - 1 downto 0 do
        FreeAndNil(Orders[i]);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

procedure TRoute4MeExamples.GetRoute(RouteId: String; GetRouteDirections,
  GetRoutePathPoints: boolean);
var
  Parameters: TRouteParametersQuery;
  ErrorString: String;
  Route: TDataObjectRoute;
begin
  Parameters := TRouteParametersQuery.Create;
  try
    Parameters.RouteId := RouteId;

    if (GetRouteDirections) then
      Parameters.Directions := True;

    if (GetRoutePathPoints) then
      Parameters.RoutePathOutput := TRoutePathOutputDescription[TRoutePathOutput.rpoPoints];

    // Run the query
    Route := Route4MeManager.Route.Get(Parameters, ErrorString);

    WriteLn('');
    try
      if (Route <> nil) then
      begin
        WriteLn('GetRoute executed successfully');
        WriteLn(Format('Route ID: %s', [Route.RouteId]));
        WriteLn(Format('State: %s', [TOptimizationDescription[TOptimizationState(Route.State)]]));
        if (Length(Route.Directions) > 0) then
          WriteLn(Format('Directions: length = %d', [Length(Route.Directions)]));
        if (Length(Route.Path) > 0) then
          WriteLn(Format('Path: length = %d', [Length(Route.Path)]));
      end
      else
        WriteLn(Format('GetRoute error: "%s"', [ErrorString]));
    finally
      FreeAndNil(Route);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

procedure TRoute4MeExamples.GetRoutes;
var
  Parameters: TRouteParametersQuery;
  ErrorString: String;
  Routes: TArray<TDataObjectRoute>;
  Route: TDataObjectRoute;
  i: integer;
begin
  Parameters := TRouteParametersQuery.Create();
  try
    Parameters.Limit := 10;
    Parameters.Offset := 5;

    // Run the query
    Routes := Route4MeManager.Route.GetList(Parameters, ErrorString);
    WriteLn('');
    try
      if (Length(Routes) > 0) then
      begin
        WriteLn(Format('GetRoutes executed successfully, %d routes returned', [Length(Routes)]));
        WriteLn('');

        for Route in Routes do
          WriteLn(Format('RouteId: %s', [Route.RouteId]));
      end
      else
        WriteLn(Format('GetRoutes error "%s"', [ErrorString]));
    finally
      for i := Length(Routes) - 1 downto 0 do
        FreeAndNil(Routes[i]);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

procedure TRoute4MeExamples.GetUsers;
var
  ErrorString: String;
  Users: TArray<TUser>;
  i: integer;
begin
  // Run the query
  Users := Route4MeManager.User.Get(ErrorString);
  try
    WriteLn('');

    if (Length(Users) > 0) then
    begin
      WriteLn(Format('GetUsers executed successfully, %d users returned', [Length(Users)]));
      WriteLn('');
    end
    else
      WriteLn(Format('GetUsers error: "%s"', [ErrorString]));
  finally
    for i := Length(Users) - 1 downto 0 do
      FreeAndNil(Users[i]);
  end;
end;

function TRoute4MeExamples.LogCustomActivity(Message, RouteId: String): boolean;
var
  Activity: TActivity;
  ErrorString: String;
begin
  Activity := TActivity.Create();
  try
    Activity.ActivityType := 'user_message';
    Activity.ActivityMessage := Message;
    Activity.RouteId := routeId;

    // Run the query
    Result := Route4MeManager.Activity.LogCustomActivity(Activity, ErrorString);

    WriteLn('');

    if (Result) then
      WriteLn('LogCustomActivity executed successfully')
    else
      WriteLn(Format('LogCustomActivity error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Activity);
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
  Parameters: TOptimizationParameters;
begin
  DataProvider := TMultipleDepotMultipleDriverTestDataProvider.Create;

  // Run the query
  Parameters := DataProvider.OptimizationParameters;
  try
    DataObject := Route4MeManager.Optimization.Run(Parameters, ErrorString);
  finally
    FreeAndNil(Parameters);
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
  Parameters: TOptimizationParameters;
begin
  DataProvider := TMultipleDepotMultipleDriverTimeWindowTestDataProvider.Create;

  // Run the query
  Parameters := DataProvider.OptimizationParameters;
  try
    DataObject := Route4MeManager.Optimization.Run(Parameters, ErrorString);
  finally
    FreeAndNil(Parameters);
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
  Parameters: TOptimizationParameters;
begin
  DataProvider := TMultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProvider.Create;

  // Run the query
  Parameters := DataProvider.OptimizationParameters;
  try
    DataObject := Route4MeManager.Optimization.Run(Parameters, ErrorString);
  finally
    FreeAndNil(Parameters);
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

procedure TRoute4MeExamples.RemoveAddressBookContacts(AddressIds: TArray<integer>);
var
  ErrorString: String;
  Removed: boolean;
begin
  // Run the query
  Removed := Route4MeManager.AddressBookContact.Remove(AddressIds, ErrorString);

  WriteLn('');

  if (Removed) then
    WriteLn(Format('RemoveAddressBookContacts executed successfully, %d contacts deleted',
      [Length(AddressIds)]))
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
  Removed: boolean;
begin
  // Run the query
  Removed := Route4MeManager.Optimization.Remove(OptimizationProblemID, ErrorString);

  WriteLn('');

  if (Removed) then
  begin
    WriteLn('RemoveOptimization executed successfully');
    WriteLn(Format('Optimization Problem ID: %s', [OptimizationProblemID]));
  end
  else
    WriteLn(Format('RemoveOptimization error: "%s"', [ErrorString]));
end;

procedure TRoute4MeExamples.RemoveOrders(OrderIds: TStringArray);
var
  ErrorString: String;
  Removed: boolean;
begin
  // Run the query
  Removed := Route4MeManager.Order.Remove(OrderIds, ErrorString);

  WriteLn('');

  if (Removed) then
    WriteLn(Format('RemoveOrders executed successfully, %d orders removed', [Length(OrderIds)]))
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
  Parameters: TOptimizationParameters;
  ErrorString: String;
  DataObject: TDataObject;
begin
  Parameters := TOptimizationParameters.Create;
  try
    Parameters.OptimizationProblemID := OptimizationProblemId;
    Parameters.ReOptimize := True;

    // Run the query
    DataObject := Route4MeManager.Optimization.Update(Parameters, ErrorString);

    WriteLn('');
    try
      if (DataObject <> nil) then
      begin
        WriteLn('ReOptimization executed successfully');
        WriteLn(Format('Optimization Problem ID: %s', [DataObject.OptimizationProblemId]));
        WriteLn(Format('State: %s', [TOptimizationDescription[TOptimizationState(DataObject.State)]]));
      end
      else
        WriteLn(Format('ReOptimization error: "%s"', [ErrorString]));
    finally
      FreeAndNil(DataObject);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

procedure TRoute4MeExamples.ReoptimizeRoute(RouteId: String);
var
  Parameters: TRouteParametersQuery;
  ErrorString: String;
  Route: TDataObjectRoute;
begin
  Parameters := TRouteParametersQuery.Create;
  try
    Parameters.RouteId := RouteId;
    Parameters.ReOptimize := True;

    // Run the query
    Route := Route4MeManager.Route.Update(Parameters, ErrorString);

    WriteLn('');
    try
      if (Route <> nil) then
      begin
        WriteLn('ReoptimizeRoute executed successfully');
        WriteLn(Format('Route ID: %s', [Route.RouteId]));
        WriteLn(Format('State: %s', [TOptimizationDescription[TOptimizationState(Route.State)]]));
      end
      else
        WriteLn(Format('ReoptimizeRoute error: "%s"', [ErrorString]));
    finally
      FreeAndNil(Route);
    end;
  finally
    FreeAndNil(Parameters);
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
  Parameters: TGPSParameters;
  Response: String;
begin
  // Create the gps parametes
  Parameters := TGPSParameters.Create();
  try
    Parameters.Format := TFormatDescription[TFormatEnum.Csv];
    Parameters.RouteId := RouteId;
    Parameters.Latitude := 33.14384;
    Parameters.Longitude := -83.22466;
    Parameters.Course := 1;
    Parameters.Speed := 120;
    Parameters.DeviceType := TDeviceTypeDescription[TDeviceType.IPhone];
    Parameters.MemberId := 1;
    Parameters.DeviceGuid := 'TEST_GPS';
    Parameters.DeviceTimestamp := '2014-06-14 17:43:35';

    Response := Route4MeManager.Tracking.SetGPS(Parameters, ErrorString);

    WriteLn('');

    if (ErrorString = EmptyStr) then
      WriteLn(Format('SetGps response: %s', [Response]))
    else
      WriteLn(Format('SetGps error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Parameters);
  end;
end;

procedure TRoute4MeExamples.ShareRoute(RouteId, RecipientEmail: String);
var
  ErrorString: String;
begin
  Route4MeManager.Route.Share(RouteId, RecipientEmail, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
    WriteLn('ShareRoute executed successfully')
  else
    WriteLn(Format('ShareRoute error: "%s"', [ErrorString]));
end;

function TRoute4MeExamples.SingleDepotMultipleDriverNoTimeWindow: TDataObject;
var
  DataProvider: IOptimizationParametersProvider;
  ErrorString: String;
  DataObject: TDataObject;
  Parameters: TOptimizationParameters;
begin
  DataProvider := TSingleDepotMultipleDriverNoTimeWindowTestDataProvider.Create;

  // Run the query
  Parameters := DataProvider.OptimizationParameters;
  try
    DataObject := Route4MeManager.Optimization.Run(Parameters, ErrorString);
  finally
    FreeAndNil(Parameters);
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
  Parameters: TOptimizationParameters;
begin
  DataProvider := TSingleDriverMultipleTimeWindowsTestDataProvider.Create;

  // Run the query
  Parameters := DataProvider.OptimizationParameters;
  try
    DataObject := Route4MeManager.Optimization.Run(Parameters, ErrorString);
  finally
    FreeAndNil(Parameters);
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
  Parameters: TOptimizationParameters;
begin
  DataProvider := TSingleDriverRoundTripTestDataProvider.Create;

  // Run the query
  Parameters := DataProvider.OptimizationParameters;
  try
    DataObject := Route4MeManager.Optimization.Run(Parameters, ErrorString);
  finally
    FreeAndNil(Parameters);
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
  Response: TSingleDriverRoundTripGenericResponse;
  Address: TAddress;
  Parameters: TOptimizationParameters;
begin
  Result := NullableString.Null;

  DataProvider := TSingleDriverRoundTripGenericTestDataProvider.Create;
  Request := TSingleDriverRoundTripGenericRequest.Create;
  try
    Parameters := DataProvider.OptimizationParameters;
    try
      Request.Parameters := Parameters.Parameters.Value as TRouteParameters;
      Request.Addresses := Parameters.Addresses;

      // Run the query
      Response := Route4MeManager.Connection.Post(TSettings.ApiHost, Request,
        TSingleDriverRoundTripGenericResponse, errorString) as TSingleDriverRoundTripGenericResponse;
      try
        WriteLn('');

        if (Response <> nil) then
        begin
          WriteLn('SingleDriverRoundTripGeneric executed successfully');
          WriteLn('');

          WriteLn(Format('Optimization Problem ID: %s', [Response.OptimizationProblemId]));
          WriteLn(Format('State: %s', [TOptimizationDescription[TOptimizationState(Response.MyState)]]));
          WriteLn('');

          for Address in Response.Addresses do
          begin
            WriteLn(Format('Address: %s', [Address.AddressString]));
            WriteLn(Format('Route ID: %s', [Address.RouteId.ToString]));
          end;

          Result := Response.OptimizationProblemId;
        end
        else
          WriteLn(Format('SingleDriverRoundTripGeneric error "%s"', [ErrorString]));
      finally
        FreeAndNil(Response);
      end;
    finally
      FreeAndNil(Parameters);
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
  Parameters: TOptimizationParameters;
begin
  DataProvider := TSingleDriverRoute10StopsTestDataProvider.Create;

  // Run the query
  Parameters := DataProvider.OptimizationParameters;
  try
    DataObject := Route4MeManager.Optimization.Run(Parameters, ErrorString);
  finally
    FreeAndNil(Parameters);
  end;

  // Output the result
  PrintExampleOptimizationResult('SingleDriverRoute10Stops', DataObject, ErrorString);

  Result := DataObject;
end;

procedure TRoute4MeExamples.TrackDeviceLastLocationHistory(RouteId: String);
var
  ErrorString: String;
  Parameters: TGPSParameters;
  GenericParameters: TGenericParameters;
  Response: String;
  DataObject: TDataObject;
  HistoryStep: TTrackingHistory;
begin
  // Create the gps parametes
  Parameters := TGPSParameters.Create();
  try
    Parameters.Format := TFormatDescription[TFormatEnum.Csv];
    Parameters.RouteId := RouteId;
    Parameters.Latitude := 33.14384;
    Parameters.Longitude := -83.22466;
    Parameters.Course := 1;
    Parameters.Speed := 120;
    Parameters.DeviceType := TDeviceTypeDescription[TDeviceType.IPhone];
    Parameters.MemberId := 1;
    Parameters.DeviceGuid := 'TEST_GPS';
    Parameters.DeviceTimestamp := '2014-06-14 17:43:35';

    Response := Route4MeManager.Tracking.SetGPS(Parameters, ErrorString);

    if (ErrorString <> EmptyStr) then
    begin
      WriteLn(Format('SetGps error: "%s"', [ErrorString]));
      Exit;
    end;

    WriteLn(Format('SetGps response: %s', [Response]));

    GenericParameters := TGenericParameters.Create();
    try
      GenericParameters.AddParameter('route_id', RouteId);
      GenericParameters.AddParameter('device_tracking_history', '1');

      DataObject := Route4MeManager.Tracking.GetLastLocation(GenericParameters, ErrorString);
      try
        WriteLn('');

        if (DataObject <> nil) then
        begin
          WriteLn('TrackDeviceLastLocationHistory executed successfully');
          WriteLn('');

          WriteLn(Format('Optimization Problem ID: %s', [DataObject.OptimizationProblemId]));
            WriteLn(Format('State: %s', [TOptimizationDescription[TOptimizationState(DataObject.State)]]));
          WriteLn('');

          for HistoryStep in DataObject.TrackingHistories do
          begin
            WriteLn(Format('Speed: %f', [HistoryStep.Speed.Value]));
            WriteLn(Format('Longitude: %f', [HistoryStep.Longitude.Value]));
            WriteLn(Format('Latitude: %f', [HistoryStep.Latitude.Value]));
            WriteLn(Format('Time Stamp: %s', [HistoryStep.TimeStamp.Value]));
            WriteLn('');
          end;
        end
        else
          WriteLn(Format('TrackDeviceLastLocationHistory error: "%s"', [ErrorString]));
      finally
        FreeAndNil(DataObject);
      end;
    finally
      FreeAndNil(GenericParameters);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

procedure TRoute4MeExamples.UpdateAddressBookContact(
  Contact: TAddressBookContact);
var
  ErrorString: String;
  UpdatedContact: TAddressBookContact;
begin
  // Run the query
  UpdatedContact := Route4MeManager.AddressBookContact.Update(Contact, ErrorString);
  try
    WriteLn('');

    if (UpdatedContact <> nil) then
      WriteLn('UpdateAddressBookContact executed successfully')
    else
      WriteLn(Format('UpdateAddressBookContact error: "%s"', [ErrorString]));
  finally
    FreeAndNil(UpdatedContact);
  end;
end;

procedure TRoute4MeExamples.UpdateAvoidanceZone(TerritoryId: String);
var
  ErrorString: String;
  Parameters: TAvoidanceZoneParameters;
  Territory: TTerritory;
  AvoidanceZone: TAvoidanceZone;
begin
  Parameters := TAvoidanceZoneParameters.Create();
  try
    Parameters.TerritoryId := TerritoryId;
    Parameters.TerritoryName := 'Test Territory Updated';
    Parameters.TerritoryColor := 'ff00ff';
    Territory := TTerritory.Create();
    Territory.TerritoryType := TTerritoryType.ttCircle;
    Territory.AddDataItem('38.41322259056806,-78.501953234');
    Territory.AddDataItem('3000');
    Parameters.Territory := Territory;

    // Run the query
    AvoidanceZone := Route4MeManager.AvoidanceZone.Update(Parameters, ErrorString);
    try
      WriteLn('');

      if (AvoidanceZone <> nil) then
      begin
        WriteLn('UpdateAvoidanceZone executed successfully');
        WriteLn(Format('Territory ID: %s', [AvoidanceZone.TerritoryId.Value]));
      end
      else
        WriteLn(Format('UpdateAvoidanceZone error: "%s"', [ErrorString]));
    finally
      FreeAndNil(AvoidanceZone);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

procedure TRoute4MeExamples.UpdateOrder(Order: TOrder);
var
  ErrorString: String;
  UpdatedOrder: TOrder;
begin
  // Run the query
  UpdatedOrder := Route4MeManager.Order.Update(Order, ErrorString);
  try
    WriteLn('');

    if (UpdatedOrder <> nil) then
      WriteLn('UpdateOrder executed successfully')
    else
      WriteLn(Format('UpdateOrder error: "%s"', [ErrorString]));
  finally
    FreeAndNil(UpdatedOrder);
  end;
end;

procedure TRoute4MeExamples.UpdateRoute(RouteId: String);
var
  ParametersNew: TRouteParameters;
  RouteParameters: TRouteParametersQuery;
  ErrorString: String;
  Route: TDataObjectRoute;
begin
  ParametersNew := TRouteParameters.Create; // not need destroy
  ParametersNew.RouteName := 'New name of the route';

  RouteParameters := TRouteParametersQuery.Create;
  try
    RouteParameters.RouteId := RouteId;
    RouteParameters.Parameters := ParametersNew;

    // Run the query
    Route := Route4MeManager.Route.Update(RouteParameters, ErrorString);

    WriteLn('');
    try
      if (Route <> nil) then
      begin
        WriteLn('UpdateRoute executed successfully');
        WriteLn(Format('Route ID: %s', [Route.RouteId]));
        WriteLn(Format('State: %s', [TOptimizationDescription[TOptimizationState(Route.State)]]));
      end
      else
        WriteLn(Format('UpdateRoute error: %s', [ErrorString]));
    finally
      FreeAndNil(Route);
    end;
  finally
    FreeAndNil(RouteParameters);
  end;
end;

procedure TRoute4MeExamples.WriteLn(Message: String);
begin
  FOutput.Writeln(Message);
end;

end.
