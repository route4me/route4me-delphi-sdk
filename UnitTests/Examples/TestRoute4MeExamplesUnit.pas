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
  OutputUnit, DataObjectUnit, NullableBasicTypesUnit, AddressUnit;

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
// https://www.route4me.com/api.v4/address_book.php?api_key=11111111111111111111111111111111
// POST
// AddAddressBookContact.json
end;

procedure TTestRoute4MeExamples.AddAddressNote;
begin
// https://www.route4me.com/actions/addRouteNotes.php?api_key=11111111111111111111111111111111&route_id=585D2628AE1C5A4FBD7B4050CB9D9601&address_id=194622711&dev_lat=33%2c132675170898&dev_lng=-83%2c244743347168&device_type=web&strUpdateType=dropoff
// POST
// как параметры, а не JSON strUpdateType=dropoff&strNoteContents=Test+Note+Contents+27.10.2016+19%3A24%3A04
end;

procedure TTestRoute4MeExamples.AddAvoidanceZone;
begin
// https://www.route4me.com/api.v4/avoidance.php?api_key=11111111111111111111111111111111
// POST
// AddAvoidanceZone.json
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
// https://www.route4me.com/api.v4/order.php?api_key=11111111111111111111111111111111
// POST
// AddOrder.json
end;

procedure TTestRoute4MeExamples.AddRouteDestinations;
var
  RouteId: String;
  DestinationIds: TArray<integer>;
begin
  RouteId := '5BCEACC31C444BCF9D8AB604DA4DFCA7';
  DestinationIds := FRoute4MeExamples.AddRouteDestinations(RouteId);

  CheckEqualsBody('AddRouteDestinations', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&route_id=5BCEACC31C444BCF9D8AB604DA4DFCA7', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
end;

procedure TTestRoute4MeExamples.TrackDeviceLastLocationHistory;
begin

end;

procedure TTestRoute4MeExamples.UpdateAddressBookContact;
begin
// https://www.route4me.com/api.v4/address_book.php?api_key=11111111111111111111111111111111
// PUT
// UpdateAddressBookContact.json
end;

procedure TTestRoute4MeExamples.UpdateAvoidanceZone;
begin
// https://www.route4me.com/api.v4/avoidance.php?api_key=11111111111111111111111111111111 HTTP/1.1
// PUT
// UpdateAvoidanceZone.json
end;

procedure TTestRoute4MeExamples.UpdateOrder;
begin
// https://www.route4me.com/api.v4/order.php?api_key=11111111111111111111111111111111
// PUT
// UpdateOrder.json
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
    SaveString(Actual);
    CheckEquals(EtalonList[0], Actual);
  finally
    FreeAndNil(EtalonList);
  end;
end;

procedure TTestRoute4MeExamples.DeleteAvoidanceZone;
begin
// https://www.route4me.com/api.v4/avoidance.php?api_key=11111111111111111111111111111111&territory_id=503F8B59E9719FE310836C830F7E82A0
// DELETE
// DeleteAvoidanceZone.json
end;

procedure TTestRoute4MeExamples.DeleteRoutes;
begin
// https://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&route_id=68621A20B99EBA14F1A4F2FDAC907B42%2c585D2628AE1C5A4FBD7B4050CB9D9601%2c3535A4E466B05DDD7FB1826D33C7BF4B%2c181AA7EA4C23DFCAD80DB4244B1BC605%2c5E335C48F3A35CC043C2D9F4B865B509%2c1275C40E330F6E54753688FCCD7B4055%2c49924C49F5B845AA429770AD0D115C92 HTTP/1.1
// DELETE
// DeleteRoutes.json
end;

procedure TTestRoute4MeExamples.DuplicateRoute;
begin
// https://www.route4me.com/actions/duplicate_route.php?api_key=11111111111111111111111111111111&to=none&route_id=68621A20B99EBA14F1A4F2FDAC907B42
// GET
// пустое тело
end;

procedure TTestRoute4MeExamples.GenericExample;
begin
// https://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&limit=10&Offset=5
// GET
// пустое тело
end;

procedure TTestRoute4MeExamples.GenericExampleShortcut;
begin
// https://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&limit=10&offset=5
// GET
// пустое тело
end;

procedure TTestRoute4MeExamples.GetActivities;
begin
  FRoute4MeExamples.GetActivities;

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/activity_feed.php?api_key=11111111111111111111111111111111&route_id=68621A20B99EBA14F1A4F2FDAC907B42&limit=10&offset=0', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
end;

procedure TTestRoute4MeExamples.GetAddress;
begin
// https://www.route4me.com/api.v4/address.php?api_key=11111111111111111111111111111111&route_id=585D2628AE1C5A4FBD7B4050CB9D9601&route_destination_id=194622711&notes=1
// GET
// пустое тело
end;

procedure TTestRoute4MeExamples.GetAddressBookContacts;
begin
// https://www.route4me.com/api.v4/address_book.php?api_key=11111111111111111111111111111111&limit=10&offset=0
// GET
// пустое тело
end;

procedure TTestRoute4MeExamples.GetAddressNotes;
begin
// https://www.route4me.com/api.v4/address.php?api_key=11111111111111111111111111111111&route_id=585D2628AE1C5A4FBD7B4050CB9D9601&route_destination_id=194622711&notes=1
// GET
// пустое тело
end;

procedure TTestRoute4MeExamples.GetAvoidanceZone;
begin
// https://www.route4me.com/api.v4/avoidance.php?api_key=11111111111111111111111111111111&territory_id=503F8B59E9719FE310836C830F7E82A0
// GET
// пустое тело
end;

procedure TTestRoute4MeExamples.GetAvoidanceZones;
begin
// https://www.route4me.com/api.v4/avoidance.php?api_key=11111111111111111111111111111111
// GET
// пустое тело
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
// https://www.route4me.com/api.v4/order.php?api_key=11111111111111111111111111111111&limit=10
// GET
// пустое тело
end;

procedure TTestRoute4MeExamples.GetRoute;
var
  RouteId: String;
  GetRouteDirections, GetRoutePathPoints: boolean;
begin
  RouteId := '68621A20B99EBA14F1A4F2FDAC907B42';
  GetRouteDirections := True;
  GetRoutePathPoints := True;

  FRoute4MeExamples.GetRoute(RouteId, GetRouteDirections, GetRoutePathPoints);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&route_id=68621A20B99EBA14F1A4F2FDAC907B42&directions=1&route_path_output=Points', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
end;

procedure TTestRoute4MeExamples.GetRoutes;
begin
  FRoute4MeExamples.GetRoutes;

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&limit=10&offset=5', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
end;

procedure TTestRoute4MeExamples.GetUsers;
begin
  FRoute4MeExamples.GetUsers;

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/member/view_users.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
end;

procedure TTestRoute4MeExamples.LogCustomActivity;
begin
// http://www.route4me.com/api.v4/activity_feed.php?api_key=11111111111111111111111111111111
// POST
// LogCustomActivity.json
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
  CheckEquals('https://www.route4me.com/actions/route/move_route_destination.php?api_key=11111111111111111111111111111111&to_route_id=5669728DF43FCE78F6CBD3DD5B533197&route_destination_id=194447367&after_destination_id=194451895', FConnection.Url);
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
// https://www.route4me.com/api.v4/address_book.php?api_key=11111111111111111111111111111111
// DELETE
// RemoveAddressBookContacts.json
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
// https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111&optimization_problem_id=EECF1B409E2491B80C860C5A7E6565AB
// DELETE
// RemoveOptimization.json
end;

procedure TTestRoute4MeExamples.RemoveOrders;
begin
// https://www.route4me.com/api.v4/order.php?api_key=11111111111111111111111111111111
// DELETE
// RemoveOrders.json
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
(*
todo: проверить.
«апрос на сервер должен быть: {}
ѕо факту: {"addresses":[],"parameters":null}

ѕричина пон€тна. — NullableObject в котором есть nullable-пол€ проблема описана в OptimizationParametersUnit
— пустым массивом - ситуаци€ нова€. ≈сли имеюща€с€ сейчас реализаци€ запроса не приведет у удалению адресов, например,
так и оставить. ¬ противном случае, придетс€ заводить Nullable-массив.



ѕо факту: {"addresses":null,"parameters":null} - на 10.1 Berlin

procedure TTypeMarshaller<TSerial>.MarshalValue(Value: TValue; fieldRTTI: TRttiField);
var
  I, Len: Integer;
  rttiType: TRttiType;
  rttiField: TRttiField;
  convEv: TJSONInterceptor;
  Data: TObject;
begin
  if Value.IsEmpty then
    FConverter.OnNull
  else



*)

  OptimizationProblemId := '6084D999940BDCF13A568724DBE8FFE4';

  FRoute4MeExamples.ReOptimization(OptimizationProblemId);

  CheckEqualsBody('ReOptimization', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111&optimization_problem_id=6084D999940BDCF13A568724DBE8FFE4&reoptimize=1', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
end;

procedure TTestRoute4MeExamples.ReoptimizeRoute;
var
  RouteId: String;
begin
  RouteId := '68621A20B99EBA14F1A4F2FDAC907B42';

  FRoute4MeExamples.ReoptimizeRoute(RouteId);

  CheckEqualsBody('ReoptimizeRoute', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&route_id=68621A20B99EBA14F1A4F2FDAC907B42&reoptimize=1', FConnection.Url);

  CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
end;

procedure TTestRoute4MeExamples.ResequenceRouteDestinations;
  function MakeTestAddress(IsDepot: boolean; DestinationId: integer; SequenceNo: integer): TAddress;
  begin
    Result := TAddress.Create;
    Result.IsDepot := IsDepot;
    Result.RouteDestinationId := DestinationId;
    Result.SequenceNo := SequenceNo;
  end;
var
  DataObjectRoute: TDataObjectRoute;
begin
  DataObjectRoute := TDataObjectRoute.Create;
  DataObjectRoute.RouteId := '5BCEACC31C444BCF9D8AB604DA4DFCA7';
  DataObjectRoute.AddAddress(MakeTestAddress(True, 194447366, 0));
  DataObjectRoute.AddAddress(MakeTestAddress(False, 194447367, 2));
  DataObjectRoute.AddAddress(MakeTestAddress(False, 194447373, 1));
  DataObjectRoute.AddAddress(MakeTestAddress(False, 194447369, 3));
  DataObjectRoute.AddAddress(MakeTestAddress(False, 194447374, 4));
  DataObjectRoute.AddAddress(MakeTestAddress(False, 194447372, 5));
  DataObjectRoute.AddAddress(MakeTestAddress(False, 194447371, 6));
  DataObjectRoute.AddAddress(MakeTestAddress(False, 194447370, 7));
  DataObjectRoute.AddAddress(MakeTestAddress(False, 194447368, 8));
  DataObjectRoute.AddAddress(MakeTestAddress(False, 194447375, 9));

  try
    FRoute4MeExamples.ResequenceRouteDestinations(DataObjectRoute);

    CheckEqualsBody('ResequenceRouteDestinations', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&route_id=5BCEACC31C444BCF9D8AB604DA4DFCA7', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
  finally
    FreeAndNil(DataObjectRoute);
  end;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('Examples\', TTestRoute4MeExamples.Suite);
end.
