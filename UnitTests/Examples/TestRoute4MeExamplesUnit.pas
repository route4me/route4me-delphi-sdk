unit TestRoute4MeExamplesUnit;

interface

uses
  TestFramework, REST.Types, Classes, IdURI, System.NetEncoding,
  SysUtils, TestBaseJsonMarshalUnit, Route4MeExamplesUnit, IRoute4MeManagerUnit,
  ConnectionStubUnit;

type
  TTestRoute4MeExamples = class(TTestCase)
  private
    FExamples: TRoute4MeExamples;
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
    procedure AddRouteDestinationsOptimally;
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
    procedure UpdateRouteCustomFields;
    procedure ReoptimizeRoute;
    procedure MergeRoutes;
    procedure GetRoute;
    procedure GetRoutes;
    procedure ShareRoute;
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
  OutputUnit, DataObjectUnit, NullableBasicTypesUnit, AddressUnit,
  CommonTypesUnit, OrderUnit, AddressBookContactUnit;

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
  // todo: ��� �������� � C#
end;

procedure TTestRoute4MeExamples.SetUp;
begin
  inherited;

  FConnection := TConnectionStub.Create;
  FExamples := TRoute4MeExamples.Create(TOutputDummy.Create, FConnection);
end;

procedure TTestRoute4MeExamples.ShareRoute;
var
  RouteId: String;
  Email: String;
begin
  RouteId := '68621A20B99EBA14F1A4F2FDAC907B42';
  Email := 'test@mail.com';

  FExamples.ShareRoute(RouteId, Email);

  CheckEquals('recipient_email=test@mail.com', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/actions/route/share_route.php?api_key=11111111111111111111111111111111&route_id=68621A20B99EBA14F1A4F2FDAC907B42&response_format=json', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctAPPLICATION_X_WWW_FORM_URLENCODED = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.SingleDepotMultipleDriverNoTimeWindow;
var
  DataObject: TDataObject;
begin
  DataObject := FExamples.SingleDepotMultipleDriverNoTimeWindow;
  try
    CheckEqualsBody('SingleDepotMultipleDriverNoTimeWindow', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
    CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
  finally
    FreeAndNil(DataObject);
  end;
end;

procedure TTestRoute4MeExamples.SingleDriverMultipleTimeWindows;
var
  DataObject: TDataObject;
begin
  DataObject := FExamples.SingleDriverMultipleTimeWindows;
  try
    CheckEqualsBody('SingleDriverMultipleTimeWindows', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
    CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
  finally
    FreeAndNil(DataObject);
  end;
end;

procedure TTestRoute4MeExamples.SingleDriverRoundTrip;
var
  DataObject: TDataObject;
begin
  DataObject := FExamples.SingleDriverRoundTrip;
  try
    CheckEqualsBody('SingleDriverRoundTrip', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
    CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
  finally
    FreeAndNil(DataObject);
  end;
end;

procedure TTestRoute4MeExamples.SingleDriverRoundTripGeneric;
var
  OptimizationProblemId: NullableString;
begin
  OptimizationProblemId := FExamples.SingleDriverRoundTripGeneric;

  CheckEqualsBody('SingleDriverRoundTripGeneric', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.SingleDriverRoute10Stops;
var
  DataObject: TDataObject;
begin
  DataObject := FExamples.SingleDriverRoute10Stops;
  try
    CheckEqualsBody('SingleDriverRoute10Stops', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
    CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
  finally
    FreeAndNil(DataObject);
  end;
end;

procedure TTestRoute4MeExamples.TearDown;
begin
  FreeAndNil(FExamples);

  inherited;
end;

procedure TTestRoute4MeExamples.AddAddressBookContact;
var
  FirstName, Address: String;
begin
  FirstName := 'Test FirstName 37';
  Address := 'Test Address1 28';
  FExamples.AddAddressBookContact(FirstName, Address);

  CheckEqualsBody('AddAddressBookContact', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/address_book.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.AddAddressNote;
var
  RouteId: String;
  AddressId: integer;
  LongitudeStr, LatitudeStr: String;
begin
  RouteId := '585D2628AE1C5A4FBD7B4050CB9D9601';
  AddressId := 194622711;
  FExamples.AddAddressNote(RouteId, AddressId);

  CheckEquals('strUpdateType=dropoff&strNoteContents=Test+Note+Contents+' + TNetEncoding.URL.Encode(DateTimeToStr(Now)),
    FConnection.RequestBody);
  LongitudeStr := FloatToStr(-83.244743347168);
  LatitudeStr := FloatToStr(33.132675170898);
  CheckEquals('https://www.route4me.com/actions/addRouteNotes.php?api_key=11111111111111111111111111111111&' +
    'route_id=585D2628AE1C5A4FBD7B4050CB9D9601&address_id=194622711&' +
    'dev_lat=' + LatitudeStr + '&dev_lng=' + LongitudeStr + '&device_type=web&strUpdateType=dropoff', FConnection.Url);

  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctAPPLICATION_X_WWW_FORM_URLENCODED = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.AddAvoidanceZone;
begin
  FExamples.AddAvoidanceZone;

  CheckEqualsBody('AddAvoidanceZone', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/avoidance.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.AddDestinationToOptimization;
var
  OptimizationProblemId: String;
  DataObject: TDataObject;
  AndReOptimize: boolean;
begin
  OptimizationProblemId := '6084D999940BDCF13A568724DBE8FFE4';
  AndReOptimize := True;

  DataObject := FExamples.AddDestinationToOptimization(OptimizationProblemId, AndReOptimize);
  try
    CheckEqualsBody('AddDestinationToOptimization', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111&optimization_problem_id=6084D999940BDCF13A568724DBE8FFE4&reoptimize=1', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
    CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
  finally
    FreeAndNil(DataObject);
  end;
end;

procedure TTestRoute4MeExamples.AddOrder;
begin
  FExamples.AddOrder;

  CheckEqualsBody('AddOrder', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/order.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.AddRouteDestinations;
var
  RouteId: String;
  DestinationIds: TArray<integer>;
begin
  RouteId := '5BCEACC31C444BCF9D8AB604DA4DFCA7';
  DestinationIds := FExamples.AddRouteDestinations(RouteId);

  CheckEqualsBody('AddRouteDestinations', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&route_id=5BCEACC31C444BCF9D8AB604DA4DFCA7', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
    CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.AddRouteDestinationsOptimally;
var
  RouteId: String;
  DestinationIds: TArray<integer>;
begin
  RouteId := '5BCEACC31C444BCF9D8AB604DA4DFCA7';
  DestinationIds := FExamples.AddRouteDestinationsOptimally(RouteId);

  CheckEqualsBody('AddRouteDestinationsOptimally', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&route_id=5BCEACC31C444BCF9D8AB604DA4DFCA7', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
    CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.TrackDeviceLastLocationHistory;
begin
  // todo: ��� �������� � C#
end;

procedure TTestRoute4MeExamples.UpdateAddressBookContact;
var
  Contact: TAddressBookContact;
begin
  Contact := TAddressBookContact.Create;
  try
    Contact.Address := 'Test Address1 768611171';
    Contact.CountryId := '0';
    Contact.Id := 10494328;
    Contact.Latitude := 38.024654;
    Contact.Longitude := -77.338814;
    Contact.FirstName := 'Test FirstName 768611171';
    Contact.LastName := 'Updated 640291126';
    FExamples.UpdateAddressBookContact(Contact);

    CheckEqualsBody('UpdateAddressBookContact', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/address_book.php?api_key=11111111111111111111111111111111', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
    CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
  finally
    FreeAndNil(Contact);
  end;
end;

procedure TTestRoute4MeExamples.UpdateAvoidanceZone;
var
  TerritoryId: String;
begin
  TerritoryId := '503F8B59E9719FE310836C830F7E82A0';
  FExamples.UpdateAvoidanceZone(TerritoryId);

  CheckEqualsBody('UpdateAvoidanceZone', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/avoidance.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.UpdateOrder;
var
  Order: TOrder;
begin
  Order := TOrder.Create;
  try
    Order.LastName := 'Updated 941062963';
    Order.Address1 := 'Test Address1 1233937909';
    Order.AddressAlias := 'Test AddressAlias 1233937909';
    Order.AddressCountryId := '0';
    Order.CachedLatitude := 37.773972;
    Order.CachedLongitude := -122.431297;
    Order.CurbsideLatitude := 37.773972;
    Order.CurbsideLongitude := -122.431297;
    Order.MemberId := '1';
    Order.OrderId := '1414';
    Order.OrderStatusId := '0';
    FExamples.UpdateOrder(Order);

    CheckEqualsBody('UpdateOrder', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/order.php?api_key=11111111111111111111111111111111', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
    CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
  finally
    FreeAndNil(Order);
  end;
end;

procedure TTestRoute4MeExamples.UpdateRoute;
var
  RouteId: String;
begin
  RouteId := '5BCEACC31C444BCF9D8AB604DA4DFCA7';

  FExamples.UpdateRoute(RouteId);

  CheckEqualsBody('UpdateRoute', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&route_id=5BCEACC31C444BCF9D8AB604DA4DFCA7', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.UpdateRouteCustomFields;
var
  RouteId: String;
  RouteDestinationId: integer;
  CustomFields: TListStringPair;
begin
  RouteId := '5BCEACC31C444BCF9D8AB604DA4DFCA7';
  RouteDestinationId := 194622711;

  CustomFields := TListStringPair.Create;
  try
    CustomFields.Add(TStringPair.Create('animal', 'Lion'));
    CustomFields.Add(TStringPair.Create('type', 'none'));

    FExamples.UpdateRoutesCustomFields(RouteId, RouteDestinationId, CustomFields);

    CheckEqualsBody('UpdateRouteCustomFields', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/address.php?api_key=11111111111111111111111111111111&route_id=5BCEACC31C444BCF9D8AB604DA4DFCA7&route_destination_id=194622711', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
    CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
  finally
    FreeAndNil(CustomFields);
  end;
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
var
  TerritoryId: String;
begin
  TerritoryId := '503F8B59E9719FE310836C830F7E82A0';
  FExamples.DeleteAvoidanceZone(TerritoryId);

  CheckEqualsBody('DeleteAvoidanceZone', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/avoidance.php?api_key=11111111111111111111111111111111&territory_id=503F8B59E9719FE310836C830F7E82A0', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmDELETE = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.DeleteRoutes;
var
  RouteIds: TStringArray;
begin
  SetLength(RouteIds, 7);
  RouteIds[0] := '68621A20B99EBA14F1A4F2FDAC907B42';
  RouteIds[1] := '585D2628AE1C5A4FBD7B4050CB9D9601';
  RouteIds[2] := '3535A4E466B05DDD7FB1826D33C7BF4B';
  RouteIds[3] := '181AA7EA4C23DFCAD80DB4244B1BC605';
  RouteIds[4] := '5E335C48F3A35CC043C2D9F4B865B509';
  RouteIds[5] := '1275C40E330F6E54753688FCCD7B4055';
  RouteIds[6] := '49924C49F5B845AA429770AD0D115C92';

  FExamples.DeleteRoutes(RouteIds);

  CheckEqualsBody('DeleteRoutes', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&route_id=' +
    '68621A20B99EBA14F1A4F2FDAC907B42,585D2628AE1C5A4FBD7B4050CB9D9601,3535A4E466B05DDD7FB1826D33C7BF4B' +
    ',181AA7EA4C23DFCAD80DB4244B1BC605,5E335C48F3A35CC043C2D9F4B865B509,1275C40E330F6E54753688FCCD7B4055' +
    ',49924C49F5B845AA429770AD0D115C92', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmDELETE = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.DuplicateRoute;
var
  RouteId: String;
begin
  RouteId := '68621A20B99EBA14F1A4F2FDAC907B42';
  FExamples.DuplicateRoute(RouteId);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/actions/duplicate_route.php?api_key=11111111111111111111111111111111&to=none&route_id=68621A20B99EBA14F1A4F2FDAC907B42', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.GenericExample;
begin
  FExamples.GenericExample(FConnection);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&limit=10&Offset=5', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.GenericExampleShortcut;
begin
  FExamples.GenericExampleShortcut(FConnection);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&limit=10&offset=5', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.GetActivities;
var
  RouteId: String;
begin
  RouteId := '68621A20B99EBA14F1A4F2FDAC907B42';
  FExamples.GetActivities(RouteId);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/activity_feed.php?api_key=11111111111111111111111111111111&route_id=68621A20B99EBA14F1A4F2FDAC907B42&limit=10&offset=0', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.GetAddress;
var
  RouteId: String;
  RouteDestinationId: integer;
begin
  RouteId := '585D2628AE1C5A4FBD7B4050CB9D9601';
  RouteDestinationId := 194622711;
  FExamples.GetAddress(RouteId, RouteDestinationId);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/address.php?api_key=11111111111111111111111111111111&route_id=585D2628AE1C5A4FBD7B4050CB9D9601&route_destination_id=194622711&notes=1', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.GetAddressBookContacts;
begin
  FExamples.GetAddressBookContacts;

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/address_book.php?api_key=11111111111111111111111111111111&limit=10&offset=0', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.GetAddressNotes;
var
  RouteId: String;
  RouteDestinationId: integer;
begin
  RouteId := '585D2628AE1C5A4FBD7B4050CB9D9601';
  RouteDestinationId := 194622711;
  FExamples.GetAddressNotes(RouteId, RouteDestinationId);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/address.php?api_key=11111111111111111111111111111111&route_id=585D2628AE1C5A4FBD7B4050CB9D9601&route_destination_id=194622711&notes=1', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.GetAvoidanceZone;
var
  TerritoryId: String;
begin
  TerritoryId := '503F8B59E9719FE310836C830F7E82A0';
  FExamples.GetAvoidanceZone(TerritoryId);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/avoidance.php?api_key=11111111111111111111111111111111&territory_id=503F8B59E9719FE310836C830F7E82A0', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.GetAvoidanceZones;
begin
  FExamples.GetAvoidanceZones;

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/avoidance.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.GetOptimization;
var
  OptimizationProblemId: String;
begin
  OptimizationProblemId := '6084D999940BDCF13A568724DBE8FFE4';
  FExamples.GetOptimization(OptimizationProblemId);

  CheckEquals('', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111&optimization_problem_id=6084D999940BDCF13A568724DBE8FFE4', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.GetOptimizations;
begin
  FExamples.GetOptimizations;

  CheckEquals('', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111&limit=10&offset=5', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.GetOrders;
begin
  FExamples.GetOrders;

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/order.php?api_key=11111111111111111111111111111111&limit=10', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.GetRoute;
var
  RouteId: String;
  GetRouteDirections, GetRoutePathPoints: boolean;
begin
  RouteId := '68621A20B99EBA14F1A4F2FDAC907B42';
  GetRouteDirections := True;
  GetRoutePathPoints := True;

  FExamples.GetRoute(RouteId, GetRouteDirections, GetRoutePathPoints);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&route_id=68621A20B99EBA14F1A4F2FDAC907B42&directions=1&route_path_output=Points', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.GetRoutes;
var
  Routes: TDataObjectRouteArray;
  i: integer;
begin
  Routes := FExamples.GetRoutes;
  try
    CheckEquals(EmptyStr, FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&limit=10&offset=5', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
    CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
  finally
    for i := Length(Routes) - 1 downto 0 do
      FreeAndNil(Routes[i]);
  end;
end;

procedure TTestRoute4MeExamples.GetUsers;
begin
  FExamples.GetUsers;

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/member/view_users.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.LogCustomActivity;
var
  Message: String;
  RouteId: String;
begin
  RouteId := '68621A20B99EBA14F1A4F2FDAC907B42';
  Message := 'Test User Activity 27.10.2016 19:21:19';
  FExamples.LogCustomActivity(Message, RouteId);

  CheckEqualsBody('LogCustomActivity', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/activity_feed.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.MergeRoutes;
var
  RouteIds: TListString;
begin
  RouteIds := TListString.Create;
  try
    RouteIds.Add('3A2DD89E6E1A044B2098AD1313E3138C');
    RouteIds.Add('C963990B11B6E3BB0648C0195E683EF0');

    FExamples.MergeRoutes(RouteIds);

    CheckEqualsBody('MergeRoutes', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/actions/merge_routes.php?api_key=11111111111111111111111111111111', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
    CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
  finally
    FreeAndNil(RouteIds);
  end;
end;

procedure TTestRoute4MeExamples.MoveDestinationToRoute;
var
  ToRouteId: String;
  RouteDestinationId, AfterDestinationId: integer;
begin
  ToRouteId := '5669728DF43FCE78F6CBD3DD5B533197';
  RouteDestinationId := 194447367;
  AfterDestinationId := 194451895;
  FExamples.MoveDestinationToRoute(ToRouteId, RouteDestinationId, AfterDestinationId);

  CheckEqualsBody('MoveDestinationToRoute', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/actions/route/move_route_destination.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctAPPLICATION_X_WWW_FORM_URLENCODED = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.MultipleDepotMultipleDriver;
var
  DataObject: TDataObject;
begin
  DataObject := FExamples.MultipleDepotMultipleDriver;
  try
    CheckEqualsBody('MultipleDepotMultipleDriver', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
    CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
  finally
    FreeAndNil(DataObject);
  end;
end;

procedure TTestRoute4MeExamples.MultipleDepotMultipleDriverTimeWindow;
var
  DataObject: TDataObject;
begin
  DataObject := FExamples.MultipleDepotMultipleDriverTimeWindow;
  try
    CheckEqualsBody('MultipleDepotMultipleDriverTimeWindow', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
    CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
  finally
    FreeAndNil(DataObject);
  end;
end;

procedure TTestRoute4MeExamples.MultipleDepotMultipleDriverWith24StopsTimeWindow;
var
  DataObject: TDataObject;
begin
  DataObject := FExamples.MultipleDepotMultipleDriverWith24StopsTimeWindow;
  try
    CheckEqualsBody('MultipleDepotMultipleDriverWith24StopsTimeWindow', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
    CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
  finally
    FreeAndNil(DataObject);
  end;
end;

procedure TTestRoute4MeExamples.RemoveAddressBookContacts;
var
  AddressIds: TArray<integer>;
begin
  SetLength(AddressIds, 2);
  AddressIds[0] := 10494328;
  AddressIds[1] := 10494329;
  FExamples.RemoveAddressBookContacts(AddressIds);

  CheckEqualsBody('RemoveAddressBookContacts', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/address_book.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmDELETE = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
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

  FExamples.RemoveDestinationFromOptimization(OptimizationProblemId, DestinationId, AndReOptimize);

  CheckEqualsBody('RemoveDestinationFromOptimization', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/address.php?api_key=11111111111111111111111111111111&optimization_problem_id=6084D999940BDCF13A568724DBE8FFE4&route_destination_id=194457563', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmDELETE = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.RemoveOptimization;
var
  OptimizationProblemId: String;
begin
  OptimizationProblemId := 'EECF1B409E2491B80C860C5A7E6565AB';
  FExamples.RemoveOptimization(OptimizationProblemId);

  CheckEqualsBody('RemoveOptimization', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111&optimization_problem_id=EECF1B409E2491B80C860C5A7E6565AB', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmDELETE = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.RemoveOrders;
var
  OrderIds: TStringArray;
begin
  SetLength(OrderIds, 2);
  OrderIds[0] := '1414';
  OrderIds[1] := '1415';
  FExamples.RemoveOrders(OrderIds);

  CheckEqualsBody('RemoveOrders', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/order.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmDELETE = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.RemoveRouteDestination;
var
  RouteId: String;
  DestinationId: integer;
begin
  RouteId := '5BCEACC31C444BCF9D8AB604DA4DFCA7';
  DestinationId := 194450192;
  FExamples.RemoveRouteDestination(RouteId, DestinationId);

  CheckEqualsBody('RemoveRouteDestination', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/address.php?api_key=11111111111111111111111111111111&route_id=5BCEACC31C444BCF9D8AB604DA4DFCA7&route_destination_id=194450192', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmDELETE = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.ReOptimization;
var
  OptimizationProblemId: String;
begin
  OptimizationProblemId := '6084D999940BDCF13A568724DBE8FFE4';

  FExamples.ReOptimization(OptimizationProblemId);

  CheckEqualsBody('ReOptimization', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111&optimization_problem_id=6084D999940BDCF13A568724DBE8FFE4&reoptimize=1', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestRoute4MeExamples.ReoptimizeRoute;
var
  RouteId: String;
begin
  RouteId := '68621A20B99EBA14F1A4F2FDAC907B42';

  FExamples.ReoptimizeRoute(RouteId);

  CheckEqualsBody('ReoptimizeRoute', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&route_id=68621A20B99EBA14F1A4F2FDAC907B42&reoptimize=1', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
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
  Route: TDataObjectRoute;
begin
  Route := TDataObjectRoute.Create;
  try
    Route.RouteId := '5BCEACC31C444BCF9D8AB604DA4DFCA7';
    Route.AddAddress(MakeTestAddress(True, 194447366, 0));
    Route.AddAddress(MakeTestAddress(False, 194447367, 2));
    Route.AddAddress(MakeTestAddress(False, 194447373, 1));
    Route.AddAddress(MakeTestAddress(False, 194447369, 3));
    Route.AddAddress(MakeTestAddress(False, 194447374, 4));
    Route.AddAddress(MakeTestAddress(False, 194447372, 5));
    Route.AddAddress(MakeTestAddress(False, 194447371, 6));
    Route.AddAddress(MakeTestAddress(False, 194447370, 7));
    Route.AddAddress(MakeTestAddress(False, 194447368, 8));
    Route.AddAddress(MakeTestAddress(False, 194447375, 9));

    FExamples.ResequenceRouteDestinations(Route);

    CheckEqualsBody('ResequenceRouteDestinations', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&route_id=5BCEACC31C444BCF9D8AB604DA4DFCA7', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
    CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
  finally
    FreeAndNil(Route);
  end;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('Examples\', TTestRoute4MeExamples.Suite);
end.
