unit Route4MeExamplesUnit;

interface

uses
  SysUtils, System.Generics.Collections,
  Route4MeManagerUnit, DataObjectUnit, NullableBasicTypesUnit, UtilsUnit,
  CommonTypesUnit, AddressBookContactUnit,
  SingleDriverRoundTripTestDataProviderUnit;

type
  TRoute4MeExamples = class
    //your api key
    const c_ApiKey = '11111111111111111111111111111111';
  protected
    Route4MeManager: TRoute4MeManager;

    procedure PrintExampleOptimizationResult(ExampleName: String;
      DataObject: TDataObject; ErrorString: String);
  public
    constructor Create;
    constructor CreateDebug;

    destructor Destroy; override;

    function SingleDriverRoute10Stops: TDataObject;
    procedure ResequenceRouteDestinations(Route: TDataObjectRoute);
    function AddRouteDestinations(RouteId: String): TArray<integer>;
    procedure RemoveRouteDestination(RouteId: String; DestinationId: integer);
    function SingleDriverRoundTrip: TDataObject;

    // TODO:
    procedure MoveDestinationToRoute(RouteIdToMoveTo: String;
      RouteDestinationIdToMove, AfterDestinationIdToMoveAfter: integer);
    function SingleDriverRoundTripGeneric: NullableString;
    function MultipleDepotMultipleDriver: TDataObject;
    function MultipleDepotMultipleDriverTimeWindow: TDataObject;
    function SingleDepotMultipleDriverNoTimeWindow: TDataObject;
    function MultipleDepotMultipleDriverWith24StopsTimeWindow: TDataObject;
    function SingleDriverMultipleTimeWindows: TDataObject;
    procedure GetOptimization(optimizationProblemId: String);
    procedure GetOptimizations;
    function AddDestinationToOptimization(OptimizationProblemId: String; b: boolean): TDataObject;
    procedure RemoveDestinationFromOptimization(OptimizationProblemId: String;
      RouteDestinationId: integer; b: boolean);
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

uses AddressUnit, EnumsUnit, IOptimizationParametersProviderUnit,
  OptimizationParametersUnit, SingleDriverRoute10StopsTestDataProviderUnit,
  GenericParametersUnit, AddressesOrderInfoUnit;

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
  OptimizationProblemId: String; b: boolean): TDataObject;
begin

end;

function TRoute4MeExamples.AddRouteDestinations(
  RouteId: String): TArray<integer>;
var
  Addresses: TAddressesArray;
  OptimalPosition: boolean;
  ErrorString: String;
  ResultAsString: String;
  AddressIds: TStringArray;
  i: integer;
begin
  // Prepare the addresses
  SetLength(Addresses, 2);
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
end;

constructor TRoute4MeExamples.Create;
begin
  // Create the manager with the api key
  Route4MeManager := TRoute4MeManager.Create(c_ApiKey);
end;

constructor TRoute4MeExamples.CreateDebug;
begin
  Create;
  Route4MeManager.SetConnectionProxy('irr-px01.rzdp.ru', 8080, 'BorzenkovIS', 'DocsVision33');
end;

procedure TRoute4MeExamples.DeleteAvoidanceZone(TerritoryId: String);
begin

end;

procedure TRoute4MeExamples.DeleteRoutes(RouteIds: TStringArray);
begin

end;

destructor TRoute4MeExamples.Destroy;
begin
  Route4MeManager.Free;
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

procedure TRoute4MeExamples.GetOptimization(optimizationProblemId: String);
begin

end;

procedure TRoute4MeExamples.GetOptimizations;
begin

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

procedure TRoute4MeExamples.MoveDestinationToRoute(RouteIdToMoveTo: String;
  RouteDestinationIdToMove, AfterDestinationIdToMoveAfter: integer);
begin

end;

function TRoute4MeExamples.MultipleDepotMultipleDriver: TDataObject;
begin

end;

function TRoute4MeExamples.MultipleDepotMultipleDriverTimeWindow: TDataObject;
begin

end;

function TRoute4MeExamples.MultipleDepotMultipleDriverWith24StopsTimeWindow: TDataObject;
begin

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
  OptimizationProblemId: String; RouteDestinationId: integer; b: boolean);
begin

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
begin

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

  // Output the result
  PrintExampleOptimizationResult('ResequenceRouteDestinations, switch 2 addresses.', NewRoute, ErrorString);
  WriteLn('');
end;

procedure TRoute4MeExamples.SetGPSPosition(RouteId: String);
begin

end;

function TRoute4MeExamples.SingleDepotMultipleDriverNoTimeWindow: TDataObject;
begin

end;

function TRoute4MeExamples.SingleDriverMultipleTimeWindows: TDataObject;
begin

end;

function TRoute4MeExamples.SingleDriverRoundTrip: TDataObject;
var
  DataProvider: IOptimizationParametersProvider;
  ErrorString: String;
  DataObject: TDataObject;
begin
  DataProvider := TSingleDriverRoundTripTestDataProvider.Create;

  // Run the query
  DataObject := Route4MeManager.Optimization.Run(
    DataProvider.OptimizationParameters, ErrorString);

  // Output the result
  PrintExampleOptimizationResult('SingleDriverRoundTrip', DataObject, ErrorString);

  Result := DataObject;
end;

function TRoute4MeExamples.SingleDriverRoundTripGeneric: NullableString;
begin

end;

function TRoute4MeExamples.SingleDriverRoute10Stops: TDataObject;
var
  DataProvider: IOptimizationParametersProvider;
  ErrorString: String;
  DataObject: TDataObject;
begin
  DataProvider := TSingleDriverRoute10StopsTestDataProvider.Create;

  // Run the query
  DataObject := Route4MeManager.Optimization.Run(
    DataProvider.OptimizationParameters, ErrorString);

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
begin

end;

end.
