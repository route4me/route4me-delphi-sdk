unit TestRoute4MeExamplesUnit;

interface

uses
  TestFramework, REST.Types, Classes,
  SysUtils, TestBaseJsonMarshalUnit, Route4MeExamplesUnit, IRoute4MeManagerUnit,
  ConnectionStubUnit;

type
  TTestRoute4MeExamples = class(TTestCase)
  private
    FRoute4MeExamples: TRoute4MeExamples;
    FConnection: TConnectionStub;

    procedure SaveString(s: String);

    procedure CheckEqualsBody(TestName: String; Actual: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure SingleDriverRoute10Stops;
    procedure ResequenceRouteDestinations;
    procedure AddRouteDestinations;
    procedure RemoveRouteDestination;
    procedure SingleDriverRoundTrip;
    procedure MoveDestinationToRoute;
    procedure SingleDriverRoundTripGeneric;
    procedure MultipleDepotMultipleDriver;
    procedure MultipleDepotMultipleDriverTimeWindow;
    procedure SingleDepotMultipleDriverNoTimeWindow;
    procedure MultipleDepotMultipleDriverWith24StopsTimeWindow;
    procedure SingleDriverMultipleTimeWindows;
    procedure GetOptimization;
    procedure GetOptimizations;
    procedure AddDestinationToOptimization;
    procedure RemoveDestinationFromOptimization;
    procedure ReOptimization;
    procedure UpdateRoute;



    procedure ReoptimizeRoute;
    procedure GetRoute;
    procedure GetRoutes;
    procedure GetUsers;
    procedure LogCustomActivity;
    procedure GetActivities;
    procedure GetAddress;
    procedure AddAddressNote;
    procedure GetAddressNotes;
    procedure DuplicateRoute;
    procedure SetGPSPosition;
    procedure TrackDeviceLastLocationHistory;
    procedure DeleteRoutes;
    procedure RemoveOptimization;
    procedure AddAddressBookContact;
    procedure GetAddressBookContacts;
    procedure UpdateAddressBookContact;
    procedure RemoveAddressBookContacts;
    procedure AddAvoidanceZone;
    procedure GetAvoidanceZones;
    procedure GetAvoidanceZone;
    procedure UpdateAvoidanceZone;
    procedure DeleteAvoidanceZone;
    procedure AddOrder;
    procedure GetOrders;
    procedure UpdateOrder;
    procedure RemoveOrders;
    procedure GenericExample;
    procedure GenericExampleShortcut;
  end;

implementation

uses
  OutputUnit, DataObjectUnit, NullableBasicTypesUnit;

procedure TTestRoute4MeExamples.SaveString(s: String);
var
  st: TStringList;
begin
  st := TStringList.Create;
  try
    st.Text := s;
    st.SaveToFile('Debug.txt');
  finally
    FreeAndNil(st);
  end;
end;

procedure TTestRoute4MeExamples.SetGPSPosition;
begin

end;

procedure TTestRoute4MeExamples.SetUp;
begin
  inherited;

  FConnection := TConnectionStub.Create;
  FRoute4MeExamples := TRoute4MeExamples.Create(TOutputDummy.Create, FConnection);
end;

procedure TTestRoute4MeExamples.SingleDepotMultipleDriverNoTimeWindow;
var
  DataObject: TDataObject;
begin
  DataObject := FRoute4MeExamples.SingleDepotMultipleDriverNoTimeWindow;
  try
    CheckEqualsBody('SingleDepotMultipleDriverNoTimeWindow', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  finally
    FreeAndNil(DataObject);
  end;
end;

procedure TTestRoute4MeExamples.SingleDriverMultipleTimeWindows;
var
  DataObject: TDataObject;
begin
  DataObject := FRoute4MeExamples.SingleDriverMultipleTimeWindows;
  try
    CheckEqualsBody('SingleDriverMultipleTimeWindows', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  finally
    FreeAndNil(DataObject);
  end;
end;

procedure TTestRoute4MeExamples.SingleDriverRoundTrip;
var
  DataObject: TDataObject;
begin
  DataObject := FRoute4MeExamples.SingleDriverRoundTrip;
  try
    CheckEqualsBody('SingleDriverRoundTrip', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  finally
    FreeAndNil(DataObject);
  end;
end;

procedure TTestRoute4MeExamples.SingleDriverRoundTripGeneric;
var
  OptimizationProblemId: NullableString;
begin
  OptimizationProblemId := FRoute4MeExamples.SingleDriverRoundTripGeneric;

  CheckEqualsBody('SingleDriverRoundTripGeneric', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
end;

procedure TTestRoute4MeExamples.SingleDriverRoute10Stops;
var
  DataObject: TDataObject;
begin
  DataObject := FRoute4MeExamples.SingleDriverRoute10Stops;
  try
    CheckEqualsBody('SingleDriverRoute10Stops', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  finally
    FreeAndNil(DataObject);
  end;
end;

procedure TTestRoute4MeExamples.TearDown;
begin
  FreeAndNil(FRoute4MeExamples);

  inherited;
end;

procedure TTestRoute4MeExamples.AddAddressBookContact;
begin

end;

procedure TTestRoute4MeExamples.AddAddressNote;
begin

end;

procedure TTestRoute4MeExamples.AddAvoidanceZone;
begin

end;

procedure TTestRoute4MeExamples.AddDestinationToOptimization;
var
  OptimizationProblemId: String;
  DataObject: TDataObject;
  AndReOptimize: boolean;
begin
  OptimizationProblemId := '6084D999940BDCF13A568724DBE8FFE4';
  AndReOptimize := True;

  DataObject := FRoute4MeExamples.AddDestinationToOptimization(OptimizationProblemId, AndReOptimize);
  try
    CheckEqualsBody('AddDestinationToOptimization', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111&optimization_problem_id=6084D999940BDCF13A568724DBE8FFE4&reoptimize=1', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
  finally
    FreeAndNil(DataObject);
  end;
end;

procedure TTestRoute4MeExamples.AddOrder;
begin

end;

procedure TTestRoute4MeExamples.AddRouteDestinations;
var
  DataObject: TDataObject;
  RouteId: String;
  DestinationIds: TArray<integer>;
begin
  RouteId := '5BCEACC31C444BCF9D8AB604DA4DFCA7';
  DestinationIds := FRoute4MeExamples.AddRouteDestinations(RouteId);
  try
    CheckEqualsBody('AddRouteDestinations', FConnection.RequestBody);
    CheckEquals('http://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&route_id=5BCEACC31C444BCF9D8AB604DA4DFCA7', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
  finally
    FreeAndNil(DataObject);
  end;
end;

procedure TTestRoute4MeExamples.TrackDeviceLastLocationHistory;
begin

end;

procedure TTestRoute4MeExamples.UpdateAddressBookContact;
begin

end;

procedure TTestRoute4MeExamples.UpdateAvoidanceZone;
begin

end;

procedure TTestRoute4MeExamples.UpdateOrder;
begin

end;

procedure TTestRoute4MeExamples.UpdateRoute;
var
  RouteId: String;
begin
  RouteId := '5BCEACC31C444BCF9D8AB604DA4DFCA7';

  FRoute4MeExamples.UpdateRoute(RouteId);

  CheckEqualsBody('UpdateRoute', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&route_id=5BCEACC31C444BCF9D8AB604DA4DFCA7', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
end;

procedure TTestRoute4MeExamples.CheckEqualsBody(TestName, Actual: String);
var
  EtalonFilename: String;
  EtalonList: TStringList;
begin
  EtalonFilename := '..\..\Etalons\Route4MeExamples\' + TestName + '.json';

  SaveString(Actual);

  EtalonList := TStringList.Create;
  try
    EtalonList.LoadFromFile(EtalonFilename);
    CheckEquals(EtalonList[0], Actual);
  finally
    FreeAndNil(EtalonList);
  end;
end;

procedure TTestRoute4MeExamples.DeleteAvoidanceZone;
begin

end;

procedure TTestRoute4MeExamples.DeleteRoutes;
begin

end;

procedure TTestRoute4MeExamples.DuplicateRoute;
begin

end;

procedure TTestRoute4MeExamples.GenericExample;
begin

end;

procedure TTestRoute4MeExamples.GenericExampleShortcut;
begin

end;

procedure TTestRoute4MeExamples.GetActivities;
begin

end;

procedure TTestRoute4MeExamples.GetAddress;
begin

end;

procedure TTestRoute4MeExamples.GetAddressBookContacts;
begin

end;

procedure TTestRoute4MeExamples.GetAddressNotes;
begin

end;

procedure TTestRoute4MeExamples.GetAvoidanceZone;
begin

end;

procedure TTestRoute4MeExamples.GetAvoidanceZones;
begin

end;

procedure TTestRoute4MeExamples.GetOptimization;
var
  OptimizationProblemId: String;
begin
  OptimizationProblemId := '6084D999940BDCF13A568724DBE8FFE4';
  FRoute4MeExamples.GetOptimization(OptimizationProblemId);

  CheckEquals('', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111&optimization_problem_id=6084D999940BDCF13A568724DBE8FFE4', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
end;

procedure TTestRoute4MeExamples.GetOptimizations;
begin
  FRoute4MeExamples.GetOptimizations;

  CheckEquals('', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111&limit=10&offset=5', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
end;

procedure TTestRoute4MeExamples.GetOrders;
begin

end;

procedure TTestRoute4MeExamples.GetRoute;
begin

end;

procedure TTestRoute4MeExamples.GetRoutes;
begin

end;

procedure TTestRoute4MeExamples.GetUsers;
begin

end;

procedure TTestRoute4MeExamples.LogCustomActivity;
begin

end;

procedure TTestRoute4MeExamples.MoveDestinationToRoute;
var
  ToRouteId: String;
  RouteDestinationId, AfterDestinationId: integer;
begin
  ToRouteId := '5669728DF43FCE78F6CBD3DD5B533197';
  RouteDestinationId := 194447367;
  AfterDestinationId := 194451895;
  FRoute4MeExamples.MoveDestinationToRoute(ToRouteId, RouteDestinationId, AfterDestinationId);

  CheckEqualsBody('MoveDestinationToRoute', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/actions/route/move_route_destination.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
end;

procedure TTestRoute4MeExamples.MultipleDepotMultipleDriver;
var
  DataObject: TDataObject;
begin
  DataObject := FRoute4MeExamples.MultipleDepotMultipleDriver;
  try
    CheckEqualsBody('MultipleDepotMultipleDriver', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  finally
    FreeAndNil(DataObject);
  end;
end;

procedure TTestRoute4MeExamples.MultipleDepotMultipleDriverTimeWindow;
var
  DataObject: TDataObject;
begin
  DataObject := FRoute4MeExamples.MultipleDepotMultipleDriverTimeWindow;
  try
    CheckEqualsBody('MultipleDepotMultipleDriverTimeWindow', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  finally
    FreeAndNil(DataObject);
  end;
end;

procedure TTestRoute4MeExamples.MultipleDepotMultipleDriverWith24StopsTimeWindow;
var
  DataObject: TDataObject;
begin
  DataObject := FRoute4MeExamples.MultipleDepotMultipleDriverWith24StopsTimeWindow;
  try
    CheckEqualsBody('MultipleDepotMultipleDriverWith24StopsTimeWindow', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  finally
    FreeAndNil(DataObject);
  end;
end;

procedure TTestRoute4MeExamples.RemoveAddressBookContacts;
begin

end;

procedure TTestRoute4MeExamples.RemoveDestinationFromOptimization;
var
  OptimizationProblemId: String;
  DestinationId: integer;
  AndReOptimize: boolean;
begin
  OptimizationProblemId := '6084D999940BDCF13A568724DBE8FFE4';
  DestinationId := 194457563;
  AndReOptimize := True;

  FRoute4MeExamples.RemoveDestinationFromOptimization(OptimizationProblemId, DestinationId, AndReOptimize);

  CheckEqualsBody('RemoveDestinationFromOptimization', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/address.php?api_key=11111111111111111111111111111111&optimization_problem_id=6084D999940BDCF13A568724DBE8FFE4&route_destination_id=194457563', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmDELETE = FConnection.Method);
end;

procedure TTestRoute4MeExamples.RemoveOptimization;
begin

end;

procedure TTestRoute4MeExamples.RemoveOrders;
begin

end;

procedure TTestRoute4MeExamples.RemoveRouteDestination;
var
  RouteId: String; DestinationId: integer;
begin
  RouteId := '5BCEACC31C444BCF9D8AB604DA4DFCA7';
  DestinationId := 194450192;
  FRoute4MeExamples.RemoveRouteDestination(RouteId, DestinationId);

  CheckEqualsBody('RemoveRouteDestination', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/address.php?api_key=11111111111111111111111111111111&route_id=5BCEACC31C444BCF9D8AB604DA4DFCA7&route_destination_id=194450192', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmDELETE = FConnection.Method);
end;

procedure TTestRoute4MeExamples.ReOptimization;
var
  OptimizationProblemId: String;
begin
  OptimizationProblemId := '6084D999940BDCF13A568724DBE8FFE4';

  FRoute4MeExamples.ReOptimization(OptimizationProblemId);

  CheckEqualsBody('ReOptimization', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111&optimization_problem_id=6084D999940BDCF13A568724DBE8FFE4&reoptimize=1', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
end;

procedure TTestRoute4MeExamples.ReoptimizeRoute;
begin

end;

procedure TTestRoute4MeExamples.ResequenceRouteDestinations;
var
  DataObjectRoute: TDataObjectRoute;
begin
  DataObjectRoute := TDataObjectRoute.Create;
  DataObjectRoute.RouteId := '5BCEACC31C444BCF9D8AB604DA4DFCA7';
  try
    FRoute4MeExamples.ResequenceRouteDestinations(DataObjectRoute);

    CheckEqualsBody('ResequenceRouteDestinations', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&route_id=5BCEACC31C444BCF9D8AB604DA4DFCA7', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
  finally
    FreeAndNil(DataObjectRoute);
  end;
end;

procedure uteDestinations;
begin
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('Examples\', TTestRoute4MeExamples.Suite);
end.
