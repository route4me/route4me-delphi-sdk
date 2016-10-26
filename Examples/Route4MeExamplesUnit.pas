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
    procedure GetRoute(RouteId: String; RouteDirections, RoutePathPoints: boolean);
    procedure GetRoutes();
    procedure GetUsers();
    procedure LogCustomActivity(Caption: String; RouteId: String);
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
//    function AddOrder: TOrder;
    procedure GetOrders;
//    procedure UpdateOrder(Order: TOrder);
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
  RouteParametersQueryUnit;

function TRoute4MeExamples.AddAddressBookContact: TAddressBookContact;
begin

end;

procedure TRoute4MeExamples.AddAddressNote(RouteId: String; RouteDestinationId: integer);
begin

end;

function TRoute4MeExamples.AddAvoidanceZone: NullableString;
begin

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
begin

end;

procedure TRoute4MeExamples.DeleteRoutes(RouteIds: TStringArray);
begin

end;

destructor TRoute4MeExamples.Destroy;
begin
  FOutput := nil;
  Route4MeManager := nil;

  inherited;
end;

function TRoute4MeExamples.DuplicateRoute(RouteId: String): NullableString;
begin

end;

procedure TRoute4MeExamples.GenericExample;
begin

end;

procedure TRoute4MeExamples.GenericExampleShortcut;
begin

end;

procedure TRoute4MeExamples.GetActivities(RouteId: String);
begin

end;

procedure TRoute4MeExamples.GetAddress(RouteId: String; RouteDestinationId: integer);
begin

end;

procedure TRoute4MeExamples.GetAddressBookContacts;
begin

end;

procedure TRoute4MeExamples.GetAddressNotes(RouteId: String;
  RouteDestinationId: integer);
begin

end;

procedure TRoute4MeExamples.GetAvoidanceZone(TerritoryId: String);
begin

end;

procedure TRoute4MeExamples.GetAvoidanceZones;
begin

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
begin

end;

procedure TRoute4MeExamples.GetRoute(RouteId: String; RouteDirections,
  RoutePathPoints: boolean);
begin

end;

procedure TRoute4MeExamples.GetRoutes;
begin

end;

procedure TRoute4MeExamples.GetUsers;
begin

end;

procedure TRoute4MeExamples.LogCustomActivity(Caption, RouteId: String);
begin

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
begin

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
begin

end;

procedure TRoute4MeExamples.RemoveOrders(OrderIds: TStringArray);
begin

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
begin

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
begin

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
begin

end;

procedure TRoute4MeExamples.UpdateAddressBookContact(
  Contact: TAddressBookContact);
begin

end;

procedure TRoute4MeExamples.UpdateAvoidanceZone(TerritoryId: String);
begin

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
