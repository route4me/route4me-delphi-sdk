unit TestExamplesRequestsUnit;

interface

uses
  TestFramework, REST.Types, Classes, IdURI, System.NetEncoding,
  SysUtils, TestBaseJsonMarshalUnit, Route4MeExamplesUnit, IRoute4MeManagerUnit,
  ConnectionStubUnit;

type
  TTestExamplesRequests = class(TTestCase)
  private
    FExamples: TRoute4MeExamples;
    FConnection: TConnectionStub;

    procedure SaveString(s: String);

    procedure CheckEqualsBody(TestName: String; Actual: String);

    procedure AddCircleTerritoryWithAddresses;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure SingleDriverRoute10Stops;
    procedure ResequenceRouteDestinations;
    procedure ResequenceAllRouteDestinations;
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
    procedure SearchRoutesForSpecifiedText;
    procedure ShareRoute;
    procedure GetUsers;
    procedure ValidateSession;
    procedure RegisterAccount;
    procedure GetUserDetails;
    procedure AddNewUser;
    procedure Authentication;
    procedure UpdateUser;
    procedure RemoveUser;
    procedure DeviceLicense;
    procedure UserLicense;
    procedure AddConfigValue;
    procedure UpdateConfigValue;
    procedure DeleteConfigValue;
    procedure GetConfigValue;
    procedure GetAllConfigValues;
    procedure RegisterWebinar;
    procedure LogSpecificMessage;
    procedure GetAllActivities;
    procedure GetTeamActivities;
    procedure GetAreaAddedActivities;
    procedure GetAreaUpdatedActivities;
    procedure GetAreaRemovedActivities;
    procedure GetDestinationDeletedActivities;
    procedure GetDestinationOutOfSequenceActivities;
    procedure GetDriverArrivedEarlyActivities;
    procedure GetDriverArrivedLateActivities;
    procedure GetDriverArrivedOnTimeActivities;
    procedure GetGeofenceLeftActivities;
    procedure GetGeofenceEnteredActivities;
    procedure GetDestinationInsertedActivities;
    procedure GetAllDestinationInsertedActivities;
    procedure GetDestinationMarkedAsDepartedActivities;
    procedure GetAllDestinationMarkedAsDepartedActivities;
    procedure GetAllDestinationMarkedAsVisitedActivities;
    procedure GetMemberCreatedActivities;
    procedure GetMemberDeletedActivities;
    procedure GetMemberModifiedActivities;
    procedure GetDestinationMovedActivities;
    procedure GetNoteInsertedActivities;
    procedure GetAllNoteInsertedActivities;
    procedure GetRouteDeletedActivities;
    procedure GetRouteOwnerChangedActivities;
    procedure GetRouteOptimizedActivities;
    procedure GetDestinationUpdatedActivities;
    procedure GetAddress;
    procedure MarkAddressAsDetectedAsVisited;
    procedure MarkAddressAsDetectedAsDeparted;
    procedure MarkAddressAsVisited;
    procedure MarkAddressAsDeparted;
    procedure AddAddressNote;
    procedure GetAddressNotes;
    procedure DuplicateRoute;
    procedure SetGPSPosition;
    procedure TrackDeviceLastLocationHistory;
    procedure GetLocationHistoryFromTimeRange;
    procedure GetAssetTrackingData;
    procedure DeleteRoutes;
    procedure RemoveOptimization;
    procedure CreateLocation;
    procedure RemoveLocations;
    procedure GetLocation;
    procedure GetLocationsByIds;
    procedure GetLocations;
    procedure LocationSearch;
    procedure DisplayRouted;
    procedure UpdateLocation;
    procedure AddCircleAvoidanceZone;
    procedure AddPolygonAvoidanceZone;
    procedure AddRectangularAvoidanceZone;
    procedure GetAvoidanceZones;
    procedure GetAvoidanceZone;
    procedure UpdateAvoidanceZone;
    procedure DeleteAvoidanceZone;
    procedure AddOrder;
    procedure GetOrders;
    procedure GetOrder;
    procedure GetOrdersByDate;
    procedure GetOrdersScheduledFor;
    procedure GetOrdersWithCustomFields;
    procedure GetOrdersWithSpecifiedText;
    procedure UpdateOrder;
    procedure RemoveOrders;
    procedure AddOrderToRoute;
    procedure AddOrderToOptimization;
    procedure GenericExample;
    procedure GenericExampleShortcut;
    procedure AddCircleTerritory;
    procedure AddPolygonTerritory;
    procedure AddRectangularTerritory;
    procedure RemoveTerritory;
    procedure UpdateTerritory;
    procedure GetTerritories;
    procedure GetTerritory;
    procedure BatchForwardGeocodeAddress;
    procedure BulkForwardGeocodeAddresses;
    procedure ReverseGeocodeAddress;
    procedure GetSingleAddress;
    procedure GetAddresses;
    procedure GetLimitedAddresses;
    procedure GetZipCodes;
    procedure GetLimitedZipCodes;
    procedure GetZipCodeAndHouseNumber;
    procedure GetLimitedZipCodeAndHouseNumber;
    procedure GetVehicle;
    procedure GetAllVehicles;
  end;

implementation

uses
  OutputUnit, DataObjectUnit, NullableBasicTypesUnit, AddressUnit,
  CommonTypesUnit, OrderUnit, AddressBookContactUnit, RouteParametersUnit,
  AddOrderToRouteRequestUnit, AddOrderToRouteParameterProviderUnit,
  AddOrderToOptimizationRequestUnit, EnumsUnit, UserParametersUnit,
  UserParameterProviderUnit, TerritoryContourUnit, TerritoryActionsUnit,
  DirectionPathPointUnit, BulkGeocodingRequestUnit, UtilsUnit;

procedure TTestExamplesRequests.SaveString(s: String);
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

procedure TTestExamplesRequests.SearchRoutesForSpecifiedText;
var
  Routes: TDataObjectRouteList;
  Text: String;
begin
  Text := 'Test Text';
  Routes := FExamples.SearchRoutesForSpecifiedText(Text);
  try
    CheckEquals(EmptyStr, FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&query=Test Text', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
    CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
  finally
    FreeAndNil(Routes);
  end;
end;

procedure TTestExamplesRequests.SetGPSPosition;
var
  RouteId: String;
begin
  RouteId := '68621A20B99EBA14F1A4F2FDAC907B42';

  FExamples.SetGPSPosition(RouteId);

  CheckEquals('', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/track/set.php?api_key=11111111111111111111111111111111&' +
    'format=xml&member_id=1&route_id=68621A20B99EBA14F1A4F2FDAC907B42&course=70&' +
    'speed=60&lat=55.6884868&lng=12.5366426&device_type=android_phone&device_guid=HK5454H0K454564WWER445', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.SetUp;
begin
  inherited;

  FConnection := TConnectionStub.Create;
  FExamples := TRoute4MeExamples.Create(TOutputDummy.Create, FConnection);
end;

procedure TTestExamplesRequests.ShareRoute;
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

procedure TTestExamplesRequests.SingleDepotMultipleDriverNoTimeWindow;
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

procedure TTestExamplesRequests.SingleDriverMultipleTimeWindows;
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

procedure TTestExamplesRequests.SingleDriverRoundTrip;
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

procedure TTestExamplesRequests.SingleDriverRoundTripGeneric;
var
  OptimizationProblemId: NullableString;
begin
  OptimizationProblemId := FExamples.SingleDriverRoundTripGeneric;

  CheckEqualsBody('SingleDriverRoundTripGeneric', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.SingleDriverRoute10Stops;
var
  DataObject: TDataObject;
begin
  // todo 5: memory leaks
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

procedure TTestExamplesRequests.TearDown;
begin
  FreeAndNil(FExamples);

  inherited;
end;

procedure TTestExamplesRequests.CreateLocation;
var
  FirstName, Address: String;
begin
  FirstName := 'Test FirstName 37';
  Address := 'Test Address1 28';
  FExamples.CreateLocation(FirstName, Address);

  CheckEqualsBody('AddAddressBookContact', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/address_book.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.AddAddressNote;
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
  LongitudeStr := FloatToStr(-83.244743347168, DottedFormat);
  LatitudeStr := FloatToStr(33.132675170898, DottedFormat);
  CheckEquals('https://www.route4me.com/actions/addRouteNotes.php?api_key=11111111111111111111111111111111&' +
    'route_id=585D2628AE1C5A4FBD7B4050CB9D9601&address_id=194622711&' +
    'dev_lat=' + LatitudeStr + '&dev_lng=' + LongitudeStr + '&device_type=web&strUpdateType=dropoff', FConnection.Url);

  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctAPPLICATION_X_WWW_FORM_URLENCODED = FConnection.ContentType);
end;

procedure TTestExamplesRequests.AddCircleAvoidanceZone;
begin
  FExamples.AddCircleAvoidanceZone;

  CheckEqualsBody('AddCircleAvoidanceZone', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/avoidance.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.AddDestinationToOptimization;
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

procedure TTestExamplesRequests.AddConfigValue;
var
  Key: String;
  Value: String;
begin
  Key := 'destination_icon_width';
  Value := '32';

  FExamples.AddConfigValue(Key, Value);

  CheckEqualsBody('AddConfigValue', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/configuration-settings.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.AddNewUser;
var
  Parameters: TUserParameters;
  Provider: IUserParameterProvider;
  EMail: String;
begin
  Provider := TUserParameterProvider.Create;
  EMail := 'skrynkovskyy+newdispatcher@gmail.com';
  Parameters := Provider.GetParameters(EMail);
  try
    FExamples.AddNewUser(Parameters);

    CheckEqualsBody('AddNewUser', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/user.php?api_key=11111111111111111111111111111111', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
    CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
  finally
    FreeAndNil(Parameters);
  end;
end;

procedure TTestExamplesRequests.AddOrder;
begin
  FExamples.AddOrder;

  CheckEqualsBody('AddOrder', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/order.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.AddOrderToOptimization;
var
  Provider: IAddOrderToRouteParameterProvider;
  OptimizationId: String;
  Parameters: TRouteParameters;
  OrderedAddresses: TOrderedAddressArray;
  i: integer;
begin
  Provider := TAddOrderToRouteParameterProvider.Create;

  OptimizationId := '5BCEACC31C444BCF9D8AB604DA4DFCA7';
  Parameters := Provider.GetParameters;
  OrderedAddresses := Provider.GetAddresses;
  try
    FExamples.AddOrderToOptimization(OptimizationId, Parameters, OrderedAddresses);

    CheckEqualsBody('AddOrderToRoute', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111&' +
      'optimization_problem_id=5BCEACC31C444BCF9D8AB604DA4DFCA7&redirect=0', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
    CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
  finally
    FreeAndNil(Parameters);
    for i := Length(OrderedAddresses) - 1 downto 0 do
      FreeAndNil(OrderedAddresses[i]);
  end;
end;

procedure TTestExamplesRequests.AddOrderToRoute;
var
  RouteId: String;
  Parameters: TRouteParameters;
  OrderedAddresses: TOrderedAddressArray;
  Provider: IAddOrderToRouteParameterProvider;
  i: integer;
begin
  Provider := TAddOrderToRouteParameterProvider.Create;

  RouteId := '5BCEACC31C444BCF9D8AB604DA4DFCA7';
  Parameters := Provider.GetParameters;
  OrderedAddresses := Provider.GetAddresses;
  try
    FExamples.AddOrderToRoute(RouteId, Parameters, OrderedAddresses);

    CheckEqualsBody('AddOrderToRoute', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&' +
      'route_id=5BCEACC31C444BCF9D8AB604DA4DFCA7&redirect=0', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
    CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
  finally
    FreeAndNil(Parameters);
    for i := Length(OrderedAddresses) - 1 downto 0 do
      FreeAndNil(OrderedAddresses[i]);
  end;
end;

procedure TTestExamplesRequests.AddPolygonAvoidanceZone;
begin
  FExamples.AddPolygonAvoidanceZone;

  CheckEqualsBody('AddPolygonAvoidanceZone', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/avoidance.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.AddPolygonTerritory;
begin
  FExamples.AddPolygonTerritory;

  CheckEqualsBody('AddPolygonTerritory', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/territory.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.AddRectangularAvoidanceZone;
begin
  FExamples.AddRectangularAvoidanceZone;

  CheckEqualsBody('AddRectangularAvoidanceZone', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/avoidance.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.AddRectangularTerritory;
begin
  FExamples.AddRectangularTerritory;

  CheckEqualsBody('AddRectangularTerritory', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/territory.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.AddRouteDestinations;
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

procedure TTestExamplesRequests.AddRouteDestinationsOptimally;
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

procedure TTestExamplesRequests.AddCircleTerritory;
begin
  FExamples.AddCircleTerritory;

  CheckEqualsBody('AddCircleTerritory', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/territory.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.AddCircleTerritoryWithAddresses;
var
  ErrorString: String;
  TerritoryName, TerritoryColor: String;
  TerritoryContour: TTerritoryContour;
  TerritoryId: NullableString;
  Actions: TTerritoryActions;
  Addresses: TArray<integer>;
begin
  Actions := TTerritoryActions.Create(FConnection);
  try
    TerritoryName := 'Circle Territory';
    TerritoryColor := 'ff0000';
    TerritoryContour := TTerritoryContour.MakeCircleContour(
      37.5697528227865, -77.4783325195313, 5000);
    SetLength(Addresses, 2);
    Addresses[0] := 123;
    Addresses[1] := 789;

    TerritoryId := Actions.Add(TerritoryName, TerritoryColor, TerritoryContour,
      Addresses, ErrorString);

    CheckEqualsBody('AddCircleTerritoryWithAddresses', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/territory.php?api_key=11111111111111111111111111111111', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
    CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
  finally
    FreeAndNil(Actions);
  end;
end;

procedure TTestExamplesRequests.GetAreaAddedActivities;
begin
  FExamples.GetAreaAddedActivities();

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/get_activities.php?api_key=11111111111111111111111111111111&' +
    'limit=10&offset=0&activity_type=area-added', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetAreaRemovedActivities;
begin
  FExamples.GetAreaRemovedActivities();

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/get_activities.php?api_key=11111111111111111111111111111111&' +
    'limit=10&offset=0&activity_type=area-removed', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetAreaUpdatedActivities;
begin
  FExamples.GetAreaUpdatedActivities();

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/get_activities.php?api_key=11111111111111111111111111111111&' +
    'limit=10&offset=0&activity_type=area-updated', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetAssetTrackingData;
var
  TrackingNumber: String;
begin
  TrackingNumber := 'Q7G9P1L9';

  FExamples.GetAssetTrackingData(TrackingNumber);

  CheckEquals('', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/status.php?api_key=11111111111111111111111111111111&' +
    'tracking=Q7G9P1L9', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.Authentication;
var
  EMail, Password: String;
begin
  EMail := 'user@mail.com';
  Password := '123';

  FExamples.Authentication(EMail, Password);

  CheckEqualsBody('Authentication', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/actions/authenticate.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctAPPLICATION_X_WWW_FORM_URLENCODED = FConnection.ContentType);
end;

procedure TTestExamplesRequests.TrackDeviceLastLocationHistory;
var
  RouteId: String;
begin
  RouteId := '68621A20B99EBA14F1A4F2FDAC907B42';

  FExamples.TrackDeviceLastLocationHistory(RouteId);

  CheckEquals('', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&' +
    'route_id=68621A20B99EBA14F1A4F2FDAC907B42&device_tracking_history=1', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.UpdateLocation;
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
    FExamples.UpdateLocation(Contact);

    CheckEqualsBody('UpdateAddressBookContact', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/address_book.php?api_key=11111111111111111111111111111111', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
    CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
  finally
    FreeAndNil(Contact);
  end;
end;

procedure TTestExamplesRequests.UpdateAvoidanceZone;
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

procedure TTestExamplesRequests.UpdateConfigValue;
var
  Key: String;
  Value: String;
begin
  Key := 'destination_icon_uri';
  Value := '222';

  FExamples.UpdateConfigValue(Key, Value);

  CheckEqualsBody('UpdateConfigValue', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/configuration-settings.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.UpdateOrder;
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
    Order.Id := 1414;
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

procedure TTestExamplesRequests.UpdateRoute;
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

procedure TTestExamplesRequests.UpdateRouteCustomFields;
var
  RouteId: String;
  RouteDestinationId: integer;
begin
  // todo 5: memory leaks
  RouteId := '5BCEACC31C444BCF9D8AB604DA4DFCA7';
  RouteDestinationId := 194622711;

  FExamples.UpdateRoutesCustomFields(RouteId, RouteDestinationId);

  CheckEqualsBody('UpdateRouteCustomFields', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/address.php?api_key=11111111111111111111111111111111&route_id=5BCEACC31C444BCF9D8AB604DA4DFCA7&route_destination_id=194622711', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.UpdateTerritory;
var
  TerritoryId: String;
begin
  TerritoryId := '503F8B59E9719FE310836C830F7E82A0';
  FExamples.UpdateTerritory(TerritoryId);

  CheckEqualsBody('UpdateTerritory', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/territory.php?api_key=11111111111111111111111111111111&territory_id=503F8B59E9719FE310836C830F7E82A0', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.UpdateUser;
var
  Parameters: TUserParameters;
  Provider: IUserParameterProvider;
  Email: String;
begin
  Provider := TUserParameterProvider.Create;
  EMail := 'skrynkovskyy+newdispatcher@gmail.com';
  Parameters := Provider.GetParameters(Email);
  try
    FExamples.UpdateUser(Parameters);

    CheckEqualsBody('UpdateUser', FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/user.php?api_key=11111111111111111111111111111111', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
    CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
  finally
    FreeAndNil(Parameters);
  end;
end;

procedure TTestExamplesRequests.UserLicense;
var
  MemberId: integer;
  SessionId: integer;
  DeviceId: String;
  DeviceType: TDeviceType;
  Subscription: String;
  Token: String;
  Payload: String;
begin
  MemberId := 777777;
  SessionId := 454563;
  Subscription := 'IPAD_MONTHLY';
  Token := '4/P7q7W91a-oMsCeLvIaQm6bTrgtp7';
  Payload := 'APA91bHun4MxP5egoKMwt2KZFBaFUH-1RYqx';
  DeviceId := '54564';
  DeviceType := TDeviceType.IPad;

  FExamples.UserLicense(MemberId, SessionId, DeviceId, DeviceType,
    Subscription, Token, Payload);

  CheckEqualsBody('UserLicense', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/member/user_license.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.ValidateSession;
var
  SessionGuid: String;
  MemberId: integer;
begin
  SessionGuid := 'ea250d5b4d48735419d300a95add8208';
  MemberId := 194622711;

  FExamples.ValidateSession(SessionGuid, MemberId);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/datafeed/session/validate_session.php?api_key=11111111111111111111111111111111&' +
    'session_guid=ea250d5b4d48735419d300a95add8208&member_id=194622711&format=json', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.CheckEqualsBody(TestName, Actual: String);
var
  EtalonFilename: String;
  EtalonList: TStringList;
begin
  EtalonFilename := '..\..\Etalons\Route4MeExamples\' + TestName + '.json';

  SaveString(Actual);

  EtalonList := TStringList.Create;
  try
    EtalonList.LoadFromFile(EtalonFilename);
//    SaveString(Actual);
    CheckEquals(EtalonList[0], Actual);
  finally
    FreeAndNil(EtalonList);
  end;
end;

procedure TTestExamplesRequests.DeleteAvoidanceZone;
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

procedure TTestExamplesRequests.DeleteRoutes;
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

procedure TTestExamplesRequests.GetDestinationDeletedActivities;
begin
  FExamples.GetDestinationDeletedActivities();

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/get_activities.php?api_key=11111111111111111111111111111111&' +
    'limit=10&offset=0&activity_type=delete-destination', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetDestinationInsertedActivities;
var
  RouteId: String;
begin
  RouteId := '68621A20B99EBA14F1A4F2FDAC907B42';
  FExamples.GetDestinationInsertedActivities(RouteId);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/get_activities.php?api_key=11111111111111111111111111111111&' +
    'route_id=68621A20B99EBA14F1A4F2FDAC907B42&limit=10&offset=0&activity_type=insert-destination', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetAllDestinationInsertedActivities;
begin
  FExamples.GetAllDestinationInsertedActivities();

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/get_activities.php?api_key=11111111111111111111111111111111&' +
    'limit=10&offset=0&activity_type=insert-destination', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetDestinationMarkedAsDepartedActivities;
var
  RouteId: String;
begin
  RouteId := '68621A20B99EBA14F1A4F2FDAC907B42';
  FExamples.GetDestinationMarkedAsDepartedActivities(RouteId);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/get_activities.php?api_key=11111111111111111111111111111111&' +
    'route_id=68621A20B99EBA14F1A4F2FDAC907B42&limit=10&offset=0&activity_type=mark-destination-departed', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetDestinationMovedActivities;
begin
  FExamples.GetDestinationMovedActivities();

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/get_activities.php?api_key=11111111111111111111111111111111&' +
    'limit=10&offset=0&activity_type=move-destination', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetAllDestinationMarkedAsDepartedActivities;
begin
  FExamples.GetAllDestinationMarkedAsDepartedActivities();

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/get_activities.php?api_key=11111111111111111111111111111111&' +
    'limit=10&offset=0&activity_type=mark-destination-departed', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetAllDestinationMarkedAsVisitedActivities;
begin
  FExamples.GetAllDestinationMarkedAsVisitedActivities();

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/get_activities.php?api_key=11111111111111111111111111111111&' +
    'limit=10&offset=0&activity_type=mark-destination-visited', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetAllNoteInsertedActivities;
begin
  FExamples.GetAllNoteInsertedActivities();

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/get_activities.php?api_key=11111111111111111111111111111111&' +
    'limit=10&offset=0&activity_type=note-insert', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetAllVehicles;
begin
  FExamples.GetAllVehicles;

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/vehicles/view_vehicles.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetDestinationOutOfSequenceActivities;
begin
  FExamples.GetDestinationOutOfSequenceActivities();

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/get_activities.php?api_key=11111111111111111111111111111111&' +
    'limit=10&offset=0&activity_type=destination-out-sequence', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetDestinationUpdatedActivities;
begin
  FExamples.GetDestinationUpdatedActivities();

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/get_activities.php?api_key=11111111111111111111111111111111&' +
    'limit=10&offset=0&activity_type=update-destinations', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.DeviceLicense;
var
  DeviceId: String;
  DeviceType: TDeviceType;
begin
  DeviceId := '546546516';
  DeviceType := TDeviceType.IPad;
  FExamples.DeviceLicense(DeviceId, DeviceType);

  CheckEqualsBody('DeviceLicense', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/device/verify_device_license.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.DisplayRouted;
begin
  FExamples.DisplayRouted;

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/address_book.php?api_key=11111111111111111111111111111111&' +
    'limit=10&offset=0&display=routed', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetDriverArrivedEarlyActivities;
begin
  FExamples.GetDriverArrivedEarlyActivities();

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/get_activities.php?api_key=11111111111111111111111111111111&' +
    'limit=10&offset=0&activity_type=driver-arrived-early', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetDriverArrivedLateActivities;
begin
  FExamples.GetDriverArrivedLateActivities();

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/get_activities.php?api_key=11111111111111111111111111111111&' +
    'limit=10&offset=0&activity_type=driver-arrived-late', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetDriverArrivedOnTimeActivities;
begin
  FExamples.GetDriverArrivedOnTimeActivities();

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/get_activities.php?api_key=11111111111111111111111111111111&' +
    'limit=10&offset=0&activity_type=driver-arrived-on-time', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.DuplicateRoute;
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

procedure TTestExamplesRequests.BatchForwardGeocodeAddress;
var
  Address: String;
begin
  Address := 'Los Angeles International Airport, CA';
  FExamples.BatchForwardGeocodeAddress(Address);

  CheckEquals('{}', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/geocoder.php?api_key=11111111111111111111111111111111&' +
    'addresses=Los Angeles International Airport, CA&format=xml', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.BulkForwardGeocodeAddresses;
var
  Addresses: TAddressInfoArray;
begin
  SetLength(Addresses, 3);
  Addresses[0] := TAddressInfo.Create('6817 Harrison Rd, Fredericksburg, VA 22407',
    'MirandaJCohen@dayrep.com', 'Reste1982', 'arridea.com', '404-317-9869', 'Miranda', 'Cohen');
  Addresses[1] := TAddressInfo.Create('7404 Drew Ln, Fredericksburg, VA 22407',
    'WilliamCBennett@rhyta.com', 'Enton1954', '', '912-852-2180', 'William', 'Bennett');
  Addresses[2] := TAddressInfo.Create('12316 Willow Woods Dr, Fredericksburg, VA 22407',
    'GeorgeENicholson@armyspy.com', 'Smis1967', '', '912-852-2180', 'George', 'Nicholson');
  FExamples.BulkForwardGeocodeAddresses(Addresses);

  CheckEqualsBody('BulkForwardGeocodeAddresses', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/actions/upload/json-geocode.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GenericExample;
begin
  FExamples.GenericExample(FConnection);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&limit=10&Offset=5', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GenericExampleShortcut;
begin
  FExamples.GenericExampleShortcut(FConnection);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&limit=10&offset=5', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetGeofenceEnteredActivities;
begin
  FExamples.GetGeofenceEnteredActivities();

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/get_activities.php?api_key=11111111111111111111111111111111&' +
    'limit=10&offset=0&activity_type=geofence-entered', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetGeofenceLeftActivities;
begin
  FExamples.GetGeofenceLeftActivities();

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/get_activities.php?api_key=11111111111111111111111111111111&' +
    'limit=10&offset=0&activity_type=geofence-left', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetLocationsByIds;
var
  Id1, Ids2: integer;
begin
  Id1 := 123;
  Ids2 := 678;
  FExamples.GetLocationsByIds([Id1, Ids2]);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/address_book.php?api_key=11111111111111111111111111111111&' +
    'address_id=123,678', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetMemberCreatedActivities;
begin
  FExamples.GetMemberCreatedActivities();

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/get_activities.php?api_key=11111111111111111111111111111111&' +
    'limit=10&offset=0&activity_type=member-created', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetMemberDeletedActivities;
begin
  FExamples.GetMemberDeletedActivities();

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/get_activities.php?api_key=11111111111111111111111111111111&' +
    'limit=10&offset=0&activity_type=member-deleted', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetMemberModifiedActivities;
begin
  FExamples.GetMemberModifiedActivities();

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/get_activities.php?api_key=11111111111111111111111111111111&' +
    'limit=10&offset=0&activity_type=member-modified', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetNoteInsertedActivities;
var
  RouteId: String;
begin
  RouteId := '68621A20B99EBA14F1A4F2FDAC907B42';
  FExamples.GetNoteInsertedActivities(RouteId);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/get_activities.php?api_key=11111111111111111111111111111111&' +
    'route_id=68621A20B99EBA14F1A4F2FDAC907B42&limit=10&offset=0&activity_type=note-insert', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetTeamActivities;
var
  RouteId: String;
  Limit, Offset: integer;
begin
  RouteId := '68621A20B99EBA14F1A4F2FDAC907B42';
  Limit := 10;
  Offset := 0;
  FExamples.GetTeamActivities(RouteId, Limit, Offset);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/activity_feed.php?api_key=11111111111111111111111111111111&' +
    'route_id=68621A20B99EBA14F1A4F2FDAC907B42&team=true&limit=10&offset=0', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetTerritories;
begin
  FExamples.GetTerritories;

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/territory.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetTerritory;
var
  TerritoryId: String;
begin
  TerritoryId := '503F8B59E9719FE310836C830F7E82A0';
  FExamples.GetTerritory(TerritoryId);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/territory.php?api_key=11111111111111111111111111111111&territory_id=503F8B59E9719FE310836C830F7E82A0', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetAddress;
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

procedure TTestExamplesRequests.GetAddresses;
begin
  FExamples.GetGeocodingAddresses();

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://rapid.route4me.com/street_data/?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetLimitedAddresses;
var
  Limit, Offset: integer;
begin
  Limit := 5;
  Offset := 2;
  FExamples.GetLimitedGeocodingAddresses(Limit, Offset);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://rapid.route4me.com/street_data/2/5/?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetLimitedZipCodeAndHouseNumber;
var
  Limit, Offset: integer;
  ZipCode: String;
  HouseNumber: String;
begin
  ZipCode := '00601';
  HouseNumber := '17';
  Limit := 5;
  Offset := 2;
  FExamples.GetLimitedZipCodeAndHouseNumber(ZipCode, HouseNumber, Limit, Offset);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://rapid.route4me.com/street_data/service/00601/17/2/5/?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetLimitedZipCodes;
var
  Limit, Offset: integer;
  ZipCode: String;
begin
  ZipCode := '00601';
  Limit := 5;
  Offset := 2;
  FExamples.GetLimitedZipCodes(ZipCode, Limit, Offset);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://rapid.route4me.com/street_data/zipcode/00601/2/5/?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetLocation;
var
  Query: String;
begin
  Query := 'technology';
  FExamples.GetLocation(Query);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/address_book.php?api_key=11111111111111111111111111111111&' +
    'limit=10&offset=0&query=technology', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetLocationHistoryFromTimeRange;
var
  RouteId: String;
  StartDate, EndDate: TDateTime;
begin
  RouteId := '68621A20B99EBA14F1A4F2FDAC907B42';
  StartDate := 42702.8408228241;
  EndDate := 42702.9408228241;

  FExamples.GetLocationHistoryFromTimeRange(RouteId, StartDate, EndDate);

  CheckEquals('', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/track/get_device_location.php?api_key=11111111111111111111111111111111&' +
    'route_id=68621A20B99EBA14F1A4F2FDAC907B42&format=json&last_position=0&time_period=custom&' +
    'start_date=1480335047&end_date=1480343687', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetLocations;
begin
  FExamples.GetLocations;

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/address_book.php?api_key=11111111111111111111111111111111&limit=10&offset=0', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetAddressNotes;
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

procedure TTestExamplesRequests.GetAllActivities;
var
  Limit, Offset: integer;
begin
  Limit := 10;
  Offset := 0;
  FExamples.GetAllActivities(Limit, Offset);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/activity_feed.php?api_key=11111111111111111111111111111111&limit=10&offset=0', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetAllConfigValues;
begin
  FExamples.GetAllConfigValues;

  CheckEquals('', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/configuration-settings.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetAvoidanceZone;
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

procedure TTestExamplesRequests.GetAvoidanceZones;
begin
  FExamples.GetAvoidanceZones;

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/avoidance.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetConfigValue;
var
  Key: String;
begin
  Key := 'destination_icon_width';

  FExamples.GetConfigValue(Key);

  CheckEquals('', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/configuration-settings.php?api_key=11111111111111111111111111111111' +
    '&config_key=destination_icon_width', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetOptimization;
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

procedure TTestExamplesRequests.GetOptimizations;
begin
  FExamples.GetOptimizations;

  CheckEquals('', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/optimization_problem.php?api_key=11111111111111111111111111111111&limit=10&offset=5', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetOrder;
var
  OrderId: integer;
begin
  OrderId := 68621;

  FExamples.GetOrder(OrderId);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/order.php?api_key=11111111111111111111111111111111&' +
    'order_id=68621', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetOrdersByDate;
var
  Date: TDate;
begin
  Date := 56454;

  FExamples.GetOrders(Date);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  // The date "2054-07-25" is tomorrow's date relative to "Date"
  CheckEquals('https://www.route4me.com/api.v4/order.php?api_key=11111111111111111111111111111111&' +
    'day_added_YYMMDD=2054-07-25', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetOrdersScheduledFor;
var
  Date: TDate;
begin
  Date := 56454;

  FExamples.GetOrdersScheduledFor(Date);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/order.php?api_key=11111111111111111111111111111111&' +
    'scheduled_for_YYMMDD=2054-07-24', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetOrdersWithCustomFields;
var
  Fields: TArray<String>;
begin
  Fields := ['order_id', 'member_id'];
  FExamples.GetOrdersWithCustomFields(Fields);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/order.php?api_key=11111111111111111111111111111111&' +
    'fields=order_id,member_id&offset=0&limit=10', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetOrdersWithSpecifiedText;
var
  Text: String;
begin
  Text := 'Test Fields';
  FExamples.GetOrdersWithSpecifiedText(Text);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/order.php?api_key=11111111111111111111111111111111&' +
    'query=Test+Fields&offset=0&limit=10', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetOrders;
begin
  FExamples.GetOrders;

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/order.php?api_key=11111111111111111111111111111111&limit=10', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetRoute;
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

procedure TTestExamplesRequests.GetRouteDeletedActivities;
begin
  FExamples.GetRouteDeletedActivities();

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/get_activities.php?api_key=11111111111111111111111111111111&' +
    'limit=10&offset=0&activity_type=route-delete', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetRouteOptimizedActivities;
begin
  FExamples.GetRouteOptimizedActivities();

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/get_activities.php?api_key=11111111111111111111111111111111&' +
    'limit=10&offset=0&activity_type=route-optimized', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetRouteOwnerChangedActivities;
begin
  FExamples.GetRouteOwnerChangedActivities();

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/get_activities.php?api_key=11111111111111111111111111111111&' +
    'limit=10&offset=0&activity_type=route-owner-changed', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetRoutes;
var
  Routes: TDataObjectRouteList;
  Limit, Offset: integer;
begin
  Limit := 10;
  Offset := 5;
  Routes := FExamples.GetRoutes(Limit, Offset);
  try
    CheckEquals(EmptyStr, FConnection.RequestBody);
    CheckEquals('https://www.route4me.com/api.v4/route.php?api_key=11111111111111111111111111111111&limit=10&offset=5', FConnection.Url);
    CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
    CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
  finally
    FreeAndNil(Routes);
  end;
end;

procedure TTestExamplesRequests.GetSingleAddress;
var
  Pk: integer;
begin
  Pk := 4;
  FExamples.GetSingleGeocodingAddress(Pk);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://rapid.route4me.com/street_data/4/?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetUserDetails;
var
  MemberId: integer;
begin
  MemberId := 194622711;

  FExamples.GetUserDetails(MemberId);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/user.php?api_key=11111111111111111111111111111111&' +
    'member_id=194622711', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetUsers;
begin
  FExamples.GetUsers;

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/member/view_users.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetVehicle;
var
  VehicleId: string;
begin
  VehicleId := '0A18C14AB42F6B6D7E830CE4082493E3';
  FExamples.GetVehicle(VehicleId);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/vehicles/view_vehicles.php?api_key=11111111111111111111111111111111' +
    '&vehicle_id=0A18C14AB42F6B6D7E830CE4082493E3', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetZipCodeAndHouseNumber;
var
  ZipCode: String;
  HouseNumber: String;
begin
  ZipCode := '00601';
  HouseNumber := '17';
  FExamples.GetZipCodeAndHouseNumber(ZipCode, HouseNumber);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://rapid.route4me.com/street_data/service/00601/17/?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.GetZipCodes;
var
  ZipCode: String;
begin
  ZipCode := '00601';
  FExamples.GetZipCodes(ZipCode);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://rapid.route4me.com/street_data/zipcode/00601/?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.LocationSearch;
var
  Query: String;
  Fields: TArray<String>;
begin
  Query := 'peter';
  Fields := ['first_name', 'address_email'];
  FExamples.LocationSearch(Query, Fields);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/address_book.php?api_key=11111111111111111111111111111111&' +
    'limit=10&offset=0&query=peter&fields=first_name,address_email', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.LogSpecificMessage;
var
  Message: String;
  RouteId: String;
begin
  RouteId := '68621A20B99EBA14F1A4F2FDAC907B42';
  Message := 'Test User Activity 27.10.2016 19:21:19';
  FExamples.LogSpecificMessage(Message, RouteId);

  CheckEqualsBody('LogCustomActivity', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/activity_feed.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.MarkAddressAsDeparted;
var
  RouteId: String;
  AddressId: integer;
  MemberId: integer;
  IsDeparted: boolean;
begin
  RouteId := '585D2628AE1C5A4FBD7B4050CB9D9601';
  AddressId := 194622711;
  MemberId := 2;
  IsDeparted := True;
  FExamples.MarkAddressAsDeparted(RouteId, AddressId, MemberId, IsDeparted);

  CheckEquals('', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/route/mark_address_departed.php?api_key=11111111111111111111111111111111&' +
    'route_id=585D2628AE1C5A4FBD7B4050CB9D9601&address_id=194622711&member_id=2&is_departed=1', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.MarkAddressAsDetectedAsDeparted;
var
  RouteId: String;
  RouteDestinationId: integer;
  IsDeparted: boolean;
begin
  RouteId := '585D2628AE1C5A4FBD7B4050CB9D9601';
  RouteDestinationId := 194622711;
  IsDeparted := True;
  FExamples.MarkAddressAsDetectedAsDeparted(RouteId, RouteDestinationId, IsDeparted);

  CheckEqualsBody('MarkAddressAsDetectedAsDeparted', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/address.php?api_key=11111111111111111111111111111111&' +
    'route_id=585D2628AE1C5A4FBD7B4050CB9D9601&route_destination_id=194622711', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.MarkAddressAsDetectedAsVisited;
var
  RouteId: String;
  RouteDestinationId: integer;
  IsVisited: boolean;
begin
  RouteId := '585D2628AE1C5A4FBD7B4050CB9D9601';
  RouteDestinationId := 194622711;
  IsVisited := True;
  FExamples.MarkAddressAsDetectedAsVisited(RouteId, RouteDestinationId, IsVisited);

  CheckEqualsBody('MarkAddressAsDetectedAsVisited', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/address.php?api_key=11111111111111111111111111111111&' +
    'route_id=585D2628AE1C5A4FBD7B4050CB9D9601&route_destination_id=194622711', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPUT = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.MarkAddressAsVisited;
var
  RouteId: String;
  AddressId: integer;
  MemberId: integer;
  IsVisited: boolean;
begin
  RouteId := '585D2628AE1C5A4FBD7B4050CB9D9601';
  AddressId := 194622711;
  MemberId := 2;
  IsVisited := True;
  FExamples.MarkAddressAsVisited(RouteId, AddressId, MemberId, IsVisited);

  CheckEquals('', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/actions/address/update_address_visited.php?api_key=11111111111111111111111111111111&' +
    'route_id=585D2628AE1C5A4FBD7B4050CB9D9601&address_id=194622711&member_id=2&is_visited=1', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.MergeRoutes;
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

procedure TTestExamplesRequests.MoveDestinationToRoute;
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

procedure TTestExamplesRequests.MultipleDepotMultipleDriver;
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

procedure TTestExamplesRequests.MultipleDepotMultipleDriverTimeWindow;
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

procedure TTestExamplesRequests.MultipleDepotMultipleDriverWith24StopsTimeWindow;
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

procedure TTestExamplesRequests.RegisterAccount;
var
  Plan, Industry, FirstName, LastName, Email: String;
  Terms: boolean;
  DeviceType: TDeviceType;
  Password, PasswordConfirmation: String;
begin
  Plan := 'enterprise_plan';
  Industry := 'Gifting';
  FirstName := 'Olman';
  LastName := 'Oland';
  Email := 'ololol@outlook.com';
  Terms := True;
  DeviceType := TDeviceType.Web;
  Password := '123';
  PasswordConfirmation := '123';

  FExamples.RegisterAccount(Plan, Industry, FirstName, LastName, Email, Terms,
    DeviceType, Password, PasswordConfirmation);

  CheckEqualsBody('RegisterAccount', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/actions/register_action.php?api_key=11111111111111111111111111111111&' +
    'plan=enterprise_plan', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctAPPLICATION_X_WWW_FORM_URLENCODED = FConnection.ContentType);
end;

procedure TTestExamplesRequests.RegisterWebinar;
var
  EMail, FirstName, LastName, Phone, Company: String;
  MemberId: integer;
  Date: TDateTime;
begin
  MemberId := 123456;
  EMail := 'oooooo@yahoo.com';
  FirstName := 'Mmmmm';
  LastName := 'Ccccc';
  Phone := '454-454544';
  Company := 'c_name';
  Date := 42702.8408228241;

  FExamples.RegisterWebinar(EMail, FirstName, LastName, Phone, Company, MemberId, Date);

  CheckEqualsBody('RegisterWebinar', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/actions/webinar_register.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.RemoveLocations;
var
  AddressIds: TArray<integer>;
begin
  SetLength(AddressIds, 2);
  AddressIds[0] := 10494328;
  AddressIds[1] := 10494329;
  FExamples.RemoveLocations(AddressIds);

  CheckEqualsBody('RemoveAddressBookContacts', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/address_book.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmDELETE = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.DeleteConfigValue;
var
  Key: String;
begin
  Key := 'My height';

  FExamples.DeleteConfigValue(Key);

  CheckEqualsBody('DeleteConfigValue', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/configuration-settings.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmDELETE = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.RemoveDestinationFromOptimization;
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

procedure TTestExamplesRequests.RemoveOptimization;
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

procedure TTestExamplesRequests.RemoveOrders;
var
  OrderIds: TIntegerArray;
begin
  SetLength(OrderIds, 2);
  OrderIds[0] := 1414;
  OrderIds[1] := 1415;
  FExamples.RemoveOrders(OrderIds);

  CheckEqualsBody('RemoveOrders', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/order.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmDELETE = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.RemoveRouteDestination;
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

procedure TTestExamplesRequests.RemoveTerritory;
var
  TerritoryId: String;
begin
  TerritoryId := '503F8B59E9719FE310836C830F7E82A0';
  FExamples.RemoveTerritory(TerritoryId);

  CheckEquals('{}', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/territory.php?api_key=11111111111111111111111111111111&territory_id=503F8B59E9719FE310836C830F7E82A0', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmDELETE = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.RemoveUser;
var
  MemberId: integer;
begin
  MemberId := 194450192;
  FExamples.RemoveUser(MemberId);

  CheckEqualsBody('RemoveUser', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v4/user.php?api_key=11111111111111111111111111111111', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmDELETE = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.ReOptimization;
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

procedure TTestExamplesRequests.ReoptimizeRoute;
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

procedure TTestExamplesRequests.ResequenceAllRouteDestinations;
var
  RouteId: String;
begin
  RouteId := '5BCEACC31C444BCF9D8AB604DA4DFCA7';

  FExamples.ResequenceAllRouteDestinations(RouteId);

  CheckEquals(EmptyStr, FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api.v3/route/reoptimize_2.php?api_key=11111111111111111111111111111111&' +
    'route_id=5BCEACC31C444BCF9D8AB604DA4DFCA7&disable_optimization=0&optimize=Distance', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmGET = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

procedure TTestExamplesRequests.ResequenceRouteDestinations;
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

procedure TTestExamplesRequests.ReverseGeocodeAddress;
var
  Location: TDirectionPathPoint;
begin
  Location := TDirectionPathPoint.Create(42.35863, -71.0567);
  FExamples.ReverseGeocodeAddress(Location);

  CheckEquals('{}', FConnection.RequestBody);
  CheckEquals('https://www.route4me.com/api/geocoder.php?api_key=11111111111111111111111111111111&' +
    'addresses=42.35863,-71.0567&format=xml', FConnection.Url);
  CheckTrue(TRESTRequestMethod.rmPOST = FConnection.Method);
  CheckTrue(TRESTContentType.ctTEXT_PLAIN = FConnection.ContentType);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('Examples\Requests\', TTestExamplesRequests.Suite);
end.
