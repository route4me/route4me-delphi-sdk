unit Route4MeExamplesUnit;

interface

uses
  SysUtils, System.Generics.Collections,
  Route4MeManagerUnit, OutputUnit, NullableBasicTypesUnit,
  CommonTypesUnit, IConnectionUnit, BulkGeocodingRequestUnit,
  DataObjectUnit, AddressBookContactUnit, OrderUnit, RouteParametersUnit,
  AddOrderToRouteRequestUnit, AddressUnit, EnumsUnit, UserParametersUnit,
  TerritoryUnit, DirectionPathPointUnit;

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
    function GetRoutes(Limit, Offset: integer): TDataObjectRouteList;
    function SearchRoutesForSpecifiedText(Text: String): TDataObjectRouteList;
    procedure BatchForwardGeocodeAddress(Address: String);
    procedure BulkForwardGeocodeAddresses(Addresses: TAddressInfoArray);
    procedure ReverseGeocodeAddress(Location: TDirectionPathPoint);
    procedure GetSingleGeocodingAddress(Pk: integer);
    procedure GetGeocodingAddresses;
    procedure GetLimitedGeocodingAddresses(Limit, Offset: integer);
    procedure GetZipCodes(ZipCode: String);
    procedure GetLimitedZipCodes(ZipCode: String; Limit, Offset: integer);
    procedure GetZipCodeAndHouseNumber(ZipCode, HouseNumber: String);
    procedure GetLimitedZipCodeAndHouseNumber(ZipCode, HouseNumber: String; Limit, Offset: integer);
    procedure GetUsers();
    procedure ValidateSession(SessionGuid: String; MemberId: integer);
    procedure RegisterAccount(Plan, Industry, FirstName, LastName, Email: String;
      Terms: boolean; DeviceType: TDeviceType;
      Password, PasswordConfirmation: String);
    procedure GetUserDetails(MemberId: integer);
    function AddNewUser(Parameters: TUserParameters): NullableInteger;
    procedure UpdateUser(Parameters: TUserParameters);
    procedure RemoveUser(MemberId: integer);
    function Authentication(EMail, Password: String): NullableString;
    procedure DeviceLicense(DeviceId: String; DeviceType: TDeviceType);
    procedure UserLicense(MemberId, SessionId: integer; DeviceId: String;
      DeviceType: TDeviceType; Subscription, Token, Payload: String);
    procedure RegisterWebinar(EMail, FirstName, LastName, Phone, Company: String;
      MemberId: integer; Date: TDateTime);
    function AddConfigValue(Key, Value: String): boolean;
    function UpdateConfigValue(Key, Value: String): boolean;
    function DeleteConfigValue(Key: String): boolean;
    procedure GetConfigValue(Key: String);
    procedure GetAllConfigValues;
    function LogSpecificMessage(Message: String; RouteId: String): boolean;
    procedure GetAllActivities(Limit, Offset: integer);
    procedure GetTeamActivities(RouteId: String; Limit, Offset: integer);
    procedure GetAreaAddedActivities;
    procedure GetAreaUpdatedActivities;
    procedure GetAreaRemovedActivities;
    procedure GetDestinationDeletedActivities;
    procedure GetDestinationOutOfSequenceActivities;
    procedure GetDriverArrivedEarlyActivities;
    procedure GetDriverArrivedLateActivities;
    procedure GetDriverArrivedOnTimeActivities;
    procedure GetGeofenceEnteredActivities;
    procedure GetGeofenceLeftActivities;
    procedure GetDestinationInsertedActivities(RouteId: String);
    procedure GetAllDestinationInsertedActivities;
    procedure GetDestinationMarkedAsDepartedActivities(RouteId: String);
    procedure GetAllDestinationMarkedAsDepartedActivities;
    procedure GetAllDestinationMarkedAsVisitedActivities;
    procedure GetMemberCreatedActivities;
    procedure GetMemberDeletedActivities;
    procedure GetMemberModifiedActivities;
    procedure GetDestinationMovedActivities;
    procedure GetNoteInsertedActivities(RouteId: String);
    procedure GetAllNoteInsertedActivities;
    procedure GetRouteDeletedActivities;
    procedure GetRouteOptimizedActivities;
    procedure GetRouteOwnerChangedActivities;
    procedure GetDestinationUpdatedActivities;
    procedure GetAddress(RouteId: String; RouteDestinationId: integer);
    procedure MarkAddressAsDetectedAsVisited(RouteId: String; RouteDestinationId: integer;
      IsVisited: boolean);
    procedure MarkAddressAsDetectedAsDeparted(RouteId: String; RouteDestinationId: integer;
      IsDeparted: boolean);
    procedure MarkAddressAsVisited(RouteId: String; AddressId, MemberId: integer;
      IsVisited: boolean);
    procedure MarkAddressAsDeparted(RouteId: String; AddressId, MemberId: integer;
      IsDeparted: boolean);
    procedure AddAddressNote(RouteId: String; AddressId: integer);
    procedure GetAddressNotes(RouteId: String; RouteDestinationId: integer);
    function DuplicateRoute(RouteId: String): NullableString;
    procedure ShareRoute(RouteId: String; RecipientEmail: String);
    procedure DeleteRoutes(RouteIds: TStringArray);
    function CreateLocation(FirstName, Address: String): TAddressBookContact;
    procedure GetLocation(Query: String);
    procedure DisplayRouted;
    procedure LocationSearch(Query: String; Fields: TArray<String>);
    procedure GetLocationsByIds(AddressesIds: TArray<integer>);
    procedure GetLocations;
    procedure UpdateLocation(Contact: TAddressBookContact);
    procedure RemoveLocations(AddressIds: TArray<integer>);
    function AddCircleAvoidanceZone: NullableString;
    function AddPolygonAvoidanceZone: NullableString;
    function AddRectangularAvoidanceZone: NullableString;
    procedure GetAvoidanceZones;
    procedure GetAvoidanceZone(TerritoryId: String);
    procedure UpdateAvoidanceZone(TerritoryId: String);
    procedure DeleteAvoidanceZone(TerritoryId: String);
    function AddOrder(): TOrder;
    procedure GetOrders; overload;
    procedure GetOrder(OrderId: integer);
    procedure GetOrders(Date: TDate); overload;
    procedure GetOrdersScheduledFor(Date: TDate);
    procedure GetOrdersWithCustomFields(Fields: TArray<String>);
    procedure GetOrdersWithSpecifiedText(SpecifiedText: String);
    procedure UpdateOrder(Order: TOrder);
    procedure RemoveOrders(OrderIds: TIntegerArray);
    procedure AddOrderToRoute(RouteId: String; Parameters: TRouteParameters;
      OrderedAddresses: TOrderedAddressArray);
    procedure AddOrderToOptimization(OptimizationId: String;
      Parameters: TRouteParameters; OrderedAddresses: TOrderedAddressArray);
    procedure SetGPSPosition(RouteId: String);
    procedure TrackDeviceLastLocationHistory(RouteId: String);
    procedure GetLocationHistoryFromTimeRange(RouteId: String; StartDate, EndDate: TDateTime);
    procedure GetAssetTrackingData(TrackingNumber: String);
    procedure GenericExample(Connection: IConnection);
    procedure GenericExampleShortcut(Connection: IConnection);
    function AddCircleTerritory: NullableString;
    function AddPolygonTerritory: NullableString;
    function AddRectangularTerritory: NullableString;
    procedure RemoveTerritory(TerritoryId: String);
    procedure GetTerritories;
    procedure GetTerritory(TerritoryId: String);
    procedure UpdateTerritory(TerritoryId: String);
    procedure GetVehicle(VehicleId: String);
    procedure GetAllVehicles;
    procedure PreviewFile(FileId: string);
    procedure UploadFileGeocoding(FileId: string);
    procedure GetAllVendors;
    procedure GetVendor(VendorId: integer);
    procedure SearchVendors(Size: TVendorSizeType; IsIntegrated: boolean;
      Feature, Country, Search: String; Page, PerPage: integer);
  end;

implementation

uses
  BaseExampleUnit,
  SingleDriverRoute10StopsUnit, MultipleDepotMultipleDriverUnit,
  MultipleDepotMultipleDriverTimeWindowUnit,
  SingleDepotMultipleDriverNoTimeWindowUnit,
  SingleDriverMultipleTimeWindowsUnit, SingleDriverRoundTripUnit,
  SingleDriverRoundTripGenericUnit,
  MultipleDepotMultipleDriverWith24StopsTimeWindowUnit, RemoveLocationsUnit,
  ResequenceRouteDestinationsUnit, AddRouteDestinationsUnit,
  AddRouteDestinationsOptimallyUnit, RemoveRouteDestinationUnit,
  MoveDestinationToRouteUnit, GetOptimizationsUnit, GetOptimizationUnit,
  AddDestinationToOptimizationUnit, RemoveDestinationFromOptimizationUnit,
  ReoptimizeRouteUnit, ReOptimizationUnit, UpdateRouteUnit, GetRoutesUnit,
  GetRouteUnit, GetUsersUnit, LogSpecificMessageUnit, GetAllActivitiesUnit,
  GetAddressUnit, GetAddressNotesUnit, AddAddressNoteUnit, DuplicateRouteUnit,
  AddCircleAvoidanceZoneUnit, GetAvoidanceZoneUnit, GetAvoidanceZonesUnit,
  UpdateAvoidanceZoneUnit, DeleteAvoidanceZoneUnit, AddOrderUnit, GetOrdersUnit,
  UpdateOrderUnit, RemoveOrdersUnit, SetGPSPositionUnit, CreateLocationUnit,
  TrackDeviceLastLocationHistoryUnit, GenericExampleShortcutUnit, ShareRouteUnit,
  GenericExampleUnit, MergeRoutesUnit, UpdateRoutesCustomFieldsUnit,
  DeleteRoutesUnit, RemoveOptimizationUnit, ResequenceAllRouteDestinationsUnit,
  AddOrderToRouteUnit, AddOrderToOptimizationUnit, GetOrderUnit,
  GetOrdersByDateUnit, GetOrdersScheduledForUnit, GetOrdersWithCustomFieldsUnit,
  GetOrdersWithSpecifiedTextUnit, MarkAddressAsDepartedUnit,
  MarkAddressAsVisitedUnit, MarkAddressAsDetectedAsVisitedUnit,
  MarkAddressAsDetectedAsDepartedUnit, BatchForwardGeocodeAddressUnit,
  ValidateSessionUnit, RegisterAccountUnit, GetUserDetailsUnit, AddNewUserUnit,
  UpdateUserUnit, RemoveAddressBookContactsRequestUnit, RemoveUserUnit,
  AuthenticationUnit, DeviceLicenseUnit, UserLicenseUnit, RegisterWebinarUnit,
  GetTeamActivitiesUnit, GetAllDestinationMarkedAsDepartedActivitiesUnit,
  GetAreaRemovedActivitiesUnit, GetAreaUpdatedActivitiesUnit,
  GetDestinationDeletedActivitiesUnit, GetDestinationOutOfSequenceActivitiesUnit,
  GetDriverArrivedEarlyActivitiesUnit, GetDriverArrivedLateActivitiesUnit,
  GetDriverArrivedOnTimeActivitiesUnit, GetDestinationMarkedAsDepartedActivitiesUnit,
  GetGeofenceEnteredActivitiesUnit, GetAllDestinationInsertedActivitiesUnit,
  GetDestinationInsertedActivitiesUnit, GetGeofenceLeftActivitiesUnit,
  GetAllDestinationMarkedAsVisitedActivitiesUnit, GetAreaAddedActivitiesUnit,
  GetMemberCreatedActivitiesUnit, GetMemberDeletedActivitiesUnit,
  GetMemberModifiedActivitiesUnit, GetDestinationMovedActivitiesUnit,
  GetNoteInsertedActivitiesUnit, GetAllNoteInsertedActivitiesUnit,
  GetRouteDeletedActivitiesUnit, GetRouteOwnerChangedActivitiesUnit,
  GetDestinationUpdatedActivitiesUnit, GetRouteOptimizedActivitiesUnit,
  GetLocationsByIdsUnit, GetLocationsUnit, UpdateLocationUnit, GetLocationUnit,
  DisplayRoutedUnit, LocationSearchUnit, AddPolygonAvoidanceZoneUnit,
  AddRectangularAvoidanceZoneUnit, AddCircleTerritoryUnit, RemoveTerritoryUnit,
  AddPolygonTerritoryUnit, AddRectangularTerritoryUnit, GetTerritoriesUnit,
  GetTerritoryUnit, UpdateTerritoryUnit, ReverseGeocodeAddressUnit,
  GetSingleGeocodingAddressUnit, GetLimitedGeocodingAddressesUnit,
  GetGeocodingAddressesUnit, GetZipCodesUnit, GetLimitedZipCodesUnit,
  GetZipCodeAndHouseNumberUnit, GetLimitedZipCodeAndHouseNumberUnit,
  BulkForwardGeocodeAddressesUnit, SearchRoutesForSpecifiedTextUnit,
  GetLocationHistoryFromTimeRangeUnit, GetAssetTrackingDataUnit,
  AddConfigValueUnit, DeleteConfigValueUnit, UpdateConfigValueUnit,
  GetConfigValueUnit, GetAllConfigValuesUnit, GetVehiclesUnit, GetVehicleUnit, PreviewFileUnit, UploadFileGeocodingUnit, GetAllVendorsUnit, GetVendorUnit, SearchVendorsUnit;

procedure TRoute4MeExamples.GetAreaAddedActivities;
var
  Example: TGetAreaAddedActivities;
begin
  Example := MakeExample(TGetAreaAddedActivities) as TGetAreaAddedActivities;
  try
    Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetAreaRemovedActivities;
var
  Example: TGetAreaRemovedActivities;
begin
  Example := MakeExample(TGetAreaRemovedActivities) as TGetAreaRemovedActivities;
  try
    Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetAreaUpdatedActivities;
var
  Example: TGetAreaUpdatedActivities;
begin
  Example := MakeExample(TGetAreaUpdatedActivities) as TGetAreaUpdatedActivities;
  try
    Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetAssetTrackingData(TrackingNumber: String);
var
  Example: TGetAssetTrackingData;
begin
  Example := MakeExample(TGetAssetTrackingData) as TGetAssetTrackingData;
  try
    Example.Execute(TrackingNumber);
  finally
    FreeAndNil(Example);
  end;
end;

function TRoute4MeExamples.CreateLocation(FirstName, Address: String): TAddressBookContact;
var
  Example: TCreateLocation;
begin
  Example := MakeExample(TCreateLocation) as TCreateLocation;
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

function TRoute4MeExamples.AddCircleAvoidanceZone: NullableString;
var
  Example: TAddCircleAvoidanceZone;
begin
  Example := MakeExample(TAddCircleAvoidanceZone) as TAddCircleAvoidanceZone;
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

function TRoute4MeExamples.AddConfigValue(Key, Value: String): boolean;
var
  Example: TAddConfigValue;
begin
  Example := MakeExample(TAddConfigValue) as TAddConfigValue;
  try
    Result := Example.Execute(Key, Value);
  finally
    FreeAndNil(Example);
  end;
end;

function TRoute4MeExamples.AddNewUser(Parameters: TUserParameters): NullableInteger;
var
  Example: TAddNewUser;
begin
  Example := MakeExample(TAddNewUser) as TAddNewUser;
  try
    Result := Example.Execute(Parameters);
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

function TRoute4MeExamples.AddPolygonAvoidanceZone: NullableString;
var
  Example: TAddPolygonAvoidanceZone;
begin
  Example := MakeExample(TAddPolygonAvoidanceZone) as TAddPolygonAvoidanceZone;
  try
    Result := Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

function TRoute4MeExamples.AddPolygonTerritory: NullableString;
var
  Example: TAddPolygonTerritory;
begin
  Example := MakeExample(TAddPolygonTerritory) as TAddPolygonTerritory;
  try
    Result := Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

function TRoute4MeExamples.AddRectangularAvoidanceZone: NullableString;
var
  Example: TAddRectangularAvoidanceZone;
begin
  Example := MakeExample(TAddRectangularAvoidanceZone) as TAddRectangularAvoidanceZone;
  try
    Result := Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

function TRoute4MeExamples.AddRectangularTerritory: NullableString;
var
  Example: TAddRectangularTerritory;
begin
  Example := MakeExample(TAddRectangularTerritory) as TAddRectangularTerritory;
  try
    Result := Example.Execute;
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

function TRoute4MeExamples.AddCircleTerritory: NullableString;
var
  Example: TAddCircleTerritory;
begin
  Example := MakeExample(TAddCircleTerritory) as TAddCircleTerritory;
  try
    Result := Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

function TRoute4MeExamples.Authentication(EMail, Password: String): NullableString;
var
  Example: TAuthentication;
begin
  Example := MakeExample(TAuthentication) as TAuthentication;
  try
    Result := Example.Execute(EMail, Password);
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

function TRoute4MeExamples.DeleteConfigValue(Key: String): boolean;
var
  Example: TDeleteConfigValue;
begin
  Example := MakeExample(TDeleteConfigValue) as TDeleteConfigValue;
  try
    Result := Example.Execute(Key);
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

procedure TRoute4MeExamples.GetDestinationDeletedActivities;
var
  Example: TGetDestinationDeletedActivities;
begin
  Example := MakeExample(TGetDestinationDeletedActivities) as TGetDestinationDeletedActivities;
  try
    Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetDestinationInsertedActivities(RouteId: String);
var
  Example: TGetDestinationInsertedActivities;
begin
  Example := MakeExample(TGetDestinationInsertedActivities) as TGetDestinationInsertedActivities;
  try
    Example.Execute(RouteId);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetAllDestinationInsertedActivities;
var
  Example: TGetAllDestinationInsertedActivities;
begin
  Example := MakeExample(TGetAllDestinationInsertedActivities) as TGetAllDestinationInsertedActivities;
  try
    Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetDestinationMarkedAsDepartedActivities(RouteId: String);
var
  Example: TGetDestinationMarkedAsDepartedActivities;
begin
  Example := MakeExample(TGetDestinationMarkedAsDepartedActivities) as TGetDestinationMarkedAsDepartedActivities;
  try
    Example.Execute(RouteId);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetAllDestinationMarkedAsDepartedActivities;
var
  Example: TGetAllDestinationMarkedAsDepartedActivities;
begin
  Example := MakeExample(TGetAllDestinationMarkedAsDepartedActivities) as TGetAllDestinationMarkedAsDepartedActivities;
  try
    Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetAllDestinationMarkedAsVisitedActivities;
var
  Example: TGetAllDestinationMarkedAsVisitedActivities;
begin
  Example := MakeExample(TGetAllDestinationMarkedAsVisitedActivities) as TGetAllDestinationMarkedAsVisitedActivities;
  try
    Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetAllNoteInsertedActivities;
var
  Example: TGetAllNoteInsertedActivities;
begin
  Example := MakeExample(TGetAllNoteInsertedActivities) as TGetAllNoteInsertedActivities;
  try
    Example.Execute();
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetAllVehicles;
var
  Example: TGetVehicles;
begin
  Example := MakeExample(TGetVehicles) as TGetVehicles;
  try
    Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetAllVendors;
var
  Example: TGetAllVendors;
begin
  Example := MakeExample(TGetAllVendors) as TGetAllVendors;
  try
    Example.Execute();
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetDestinationMovedActivities;
var
  Example: TGetDestinationMovedActivities;
begin
  Example := MakeExample(TGetDestinationMovedActivities) as TGetDestinationMovedActivities;
  try
    Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetDestinationOutOfSequenceActivities;
var
  Example: TGetDestinationOutOfSequenceActivities;
begin
  Example := MakeExample(TGetDestinationOutOfSequenceActivities) as TGetDestinationOutOfSequenceActivities;
  try
    Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetDestinationUpdatedActivities;
var
  Example: TGetDestinationUpdatedActivities;
begin
  Example := MakeExample(TGetDestinationUpdatedActivities) as TGetDestinationUpdatedActivities;
  try
    Example.Execute();
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

procedure TRoute4MeExamples.DeviceLicense(DeviceId: String; DeviceType: TDeviceType);
var
  Example: TDeviceLicense;
begin
  Example := MakeExample(TDeviceLicense) as TDeviceLicense;
  try
    Example.Execute(DeviceId, DeviceType);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.DisplayRouted;
var
  Example: TDisplayRouted;
begin
  Example := MakeExample(TDisplayRouted) as TDisplayRouted;
  try
    Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetDriverArrivedEarlyActivities;
var
  Example: TGetDriverArrivedEarlyActivities;
begin
  Example := MakeExample(TGetDriverArrivedEarlyActivities) as TGetDriverArrivedEarlyActivities;
  try
    Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetDriverArrivedLateActivities;
var
  Example: TGetDriverArrivedLateActivities;
begin
  Example := MakeExample(TGetDriverArrivedLateActivities) as TGetDriverArrivedLateActivities;
  try
    Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetDriverArrivedOnTimeActivities;
var
  Example: TGetDriverArrivedOnTimeActivities;
begin
  Example := MakeExample(TGetDriverArrivedOnTimeActivities) as TGetDriverArrivedOnTimeActivities;
  try
    Example.Execute;
  finally
    FreeAndNil(Example);
  end;
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

procedure TRoute4MeExamples.BatchForwardGeocodeAddress(Address: String);
var
  Example: TBatchForwardGeocodeAddress;
begin
  Example := MakeExample(TBatchForwardGeocodeAddress) as TBatchForwardGeocodeAddress;
  try
    Example.Execute(Address);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.BulkForwardGeocodeAddresses(Addresses: TAddressInfoArray);
var
  Example: TBulkForwardGeocodeAddresses;
begin
  Example := MakeExample(TBulkForwardGeocodeAddresses) as TBulkForwardGeocodeAddresses;
  try
    Example.Execute(Addresses);
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

procedure TRoute4MeExamples.GetGeocodingAddresses;
var
  Example: TGetGeocodingAddresses;
begin
  Example := MakeExample(TGetGeocodingAddresses) as TGetGeocodingAddresses;
  try
    Example.Execute();
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetGeofenceEnteredActivities;
var
  Example: TGetGeofenceEnteredActivities;
begin
  Example := MakeExample(TGetGeofenceEnteredActivities) as TGetGeofenceEnteredActivities;
  try
    Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetGeofenceLeftActivities;
var
  Example: TGetGeofenceLeftActivities;
begin
  Example := MakeExample(TGetGeofenceLeftActivities) as TGetGeofenceLeftActivities;
  try
    Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetLocationsByIds(AddressesIds: TArray<integer>);
var
  Example: TGetLocationsByIds;
begin
  Example := MakeExample(TGetLocationsByIds) as TGetLocationsByIds;
  try
    Example.Execute(AddressesIds);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetMemberCreatedActivities;
var
  Example: TGetMemberCreatedActivities;
begin
  Example := MakeExample(TGetMemberCreatedActivities) as TGetMemberCreatedActivities;
  try
    Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetMemberDeletedActivities;
var
  Example: TGetMemberDeletedActivities;
begin
  Example := MakeExample(TGetMemberDeletedActivities) as TGetMemberDeletedActivities;
  try
    Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetMemberModifiedActivities;
var
  Example: TGetMemberModifiedActivities;
begin
  Example := MakeExample(TGetMemberModifiedActivities) as TGetMemberModifiedActivities;
  try
    Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetNoteInsertedActivities(RouteId: String);
var
  Example: TGetNoteInsertedActivities;
begin
  Example := MakeExample(TGetNoteInsertedActivities) as TGetNoteInsertedActivities;
  try
    Example.Execute(RouteId);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetTeamActivities(RouteId: String; Limit, Offset: integer);
var
  Example: TGetTeamActivities;
begin
  Example := MakeExample(TGetTeamActivities) as TGetTeamActivities;
  try
    Example.Execute(RouteId, Limit, Offset);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetTerritories;
var
  Example: TGetTerritories;
begin
  Example := MakeExample(TGetTerritories) as TGetTerritories;
  try
    Example.Execute;
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetTerritory(TerritoryId: String);
var
  Example: TGetTerritory;
begin
  Example := MakeExample(TGetTerritory) as TGetTerritory;
  try
    Example.Execute(TerritoryId);
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

procedure TRoute4MeExamples.GetLimitedGeocodingAddresses(Limit, Offset: integer);
var
  Example: TGetLimitedGeocodingAddresses;
begin
  Example := MakeExample(TGetLimitedGeocodingAddresses) as TGetLimitedGeocodingAddresses;
  try
    Example.Execute(Limit, Offset);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetLimitedZipCodeAndHouseNumber(ZipCode,
  HouseNumber: String; Limit, Offset: integer);
var
  Example: TGetLimitedZipCodeAndHouseNumber;
begin
  Example := MakeExample(TGetLimitedZipCodeAndHouseNumber) as TGetLimitedZipCodeAndHouseNumber;
  try
    Example.Execute(ZipCode, HouseNumber, Limit, Offset);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetLimitedZipCodes(ZipCode: String; Limit, Offset: integer);
var
  Example: TGetLimitedZipCodes;
begin
  Example := MakeExample(TGetLimitedZipCodes) as TGetLimitedZipCodes;
  try
    Example.Execute(ZipCode, Limit, Offset);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetLocation(Query: String);
var
  Example: TGetLocation;
begin
  Example := MakeExample(TGetLocation) as TGetLocation;
  try
    Example.Execute(Query);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetLocationHistoryFromTimeRange(RouteId: String;
  StartDate, EndDate: TDateTime);
var
  Example: TGetLocationHistoryFromTimeRange;
begin
  Example := MakeExample(TGetLocationHistoryFromTimeRange) as TGetLocationHistoryFromTimeRange;
  try
    Example.Execute(RouteId, StartDate, EndDate);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetLocations;
var
  Example: TGetLocations;
begin
  Example := MakeExample(TGetLocations) as TGetLocations;
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

procedure TRoute4MeExamples.GetAllActivities(Limit, Offset: integer);
var
  Example: TGetAllActivities;
begin
  Example := MakeExample(TGetAllActivities) as TGetAllActivities;
  try
    Example.Execute(Limit, Offset);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetAllConfigValues;
var
  Example: TGetAllConfigValues;
begin
  Example := MakeExample(TGetAllConfigValues) as TGetAllConfigValues;
  try
    Example.Execute();
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

procedure TRoute4MeExamples.GetConfigValue(Key: String);
var
  Example: TGetConfigValue;
begin
  Example := MakeExample(TGetConfigValue) as TGetConfigValue;
  try
    Example.Execute(Key);
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

procedure TRoute4MeExamples.GetOrder(OrderId: integer);
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

procedure TRoute4MeExamples.GetOrdersWithCustomFields(Fields: TArray<String>);
var
  Example: TGetOrdersWithCustomFields;
begin
  Example := MakeExample(TGetOrdersWithCustomFields) as TGetOrdersWithCustomFields;
  try
    Example.Execute(Fields);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetOrdersWithSpecifiedText(SpecifiedText: String);
var
  Example: TGetOrdersWithSpecifiedText;
begin
  Example := MakeExample(TGetOrdersWithSpecifiedText) as TGetOrdersWithSpecifiedText;
  try
    Example.Execute(SpecifiedText);
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

procedure TRoute4MeExamples.GetRouteDeletedActivities;
var
  Example: TGetRouteDeletedActivities;
begin
  Example := MakeExample(TGetRouteDeletedActivities) as TGetRouteDeletedActivities;
  try
    Example.Execute();
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetRouteOptimizedActivities;
var
  Example: TGetRouteOptimizedActivities;
begin
  Example := MakeExample(TGetRouteOptimizedActivities) as TGetRouteOptimizedActivities;
  try
    Example.Execute();
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetRouteOwnerChangedActivities;
var
  Example: TGetRouteOwnerChangedActivities;
begin
  Example := MakeExample(TGetRouteOwnerChangedActivities) as TGetRouteOwnerChangedActivities;
  try
    Example.Execute();
  finally
    FreeAndNil(Example);
  end;
end;

function TRoute4MeExamples.GetRoutes(Limit, Offset: integer): TDataObjectRouteList;
var
  Example: TGetRoutes;
begin
  Example := MakeExample(TGetRoutes) as TGetRoutes;
  try
    Result := Example.Execute(Limit, Offset);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetSingleGeocodingAddress(Pk: integer);
var
  Example: TGetSingleGeocodingAddress;
begin
  Example := MakeExample(TGetSingleGeocodingAddress) as TGetSingleGeocodingAddress;
  try
    Example.Execute(Pk);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetUserDetails(MemberId: integer);
var
  Example: TGetUserDetails;
begin
  Example := MakeExample(TGetUserDetails) as TGetUserDetails;
  try
    Example.Execute(MemberId);
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

procedure TRoute4MeExamples.GetVehicle(VehicleId: string);
var
  Example: TGetVehicle;
begin
  Example := MakeExample(TGetVehicle) as TGetVehicle;
  try
    Example.Execute(VehicleId);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetVendor(VendorId: integer);
var
  Example: TGetVendor;
begin
  Example := MakeExample(TGetVendor) as TGetVendor;
  try
    Example.Execute(VendorId);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetZipCodeAndHouseNumber(ZipCode, HouseNumber: String);
var
  Example: TGetZipCodeAndHouseNumber;
begin
  Example := MakeExample(TGetZipCodeAndHouseNumber) as TGetZipCodeAndHouseNumber;
  try
    Example.Execute(ZipCode, HouseNumber);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.GetZipCodes(ZipCode: String);
var
  Example: TGetZipCodes;
begin
  Example := MakeExample(TGetZipCodes) as TGetZipCodes;
  try
    Example.Execute(ZipCode);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.LocationSearch(Query: String; Fields: TArray<String>);
var
  Example: TLocationSearch;
begin
  Example := MakeExample(TLocationSearch) as TLocationSearch;
  try
    Example.Execute(Query, Fields);
  finally
    FreeAndNil(Example);
  end;
end;

function TRoute4MeExamples.LogSpecificMessage(Message, RouteId: String): boolean;
var
  Example: TLogSpecificMessage;
begin
  Example := MakeExample(TLogSpecificMessage) as TLogSpecificMessage;
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

procedure TRoute4MeExamples.MarkAddressAsDeparted(RouteId: String;
  AddressId, MemberId: integer; IsDeparted: boolean);
var
  Example: TMarkAddressAsDeparted;
begin
  Example := MakeExample(TMarkAddressAsDeparted) as TMarkAddressAsDeparted;
  try
    Example.Execute(RouteId, AddressId, MemberId, IsDeparted);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.MarkAddressAsDetectedAsDeparted(RouteId: String;
  RouteDestinationId: integer; IsDeparted: boolean);
var
  Example: TMarkAddressAsDetectedAsDeparted;
begin
  Example := MakeExample(TMarkAddressAsDetectedAsDeparted) as TMarkAddressAsDetectedAsDeparted;
  try
    Example.Execute(RouteId, RouteDestinationId, IsDeparted);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.MarkAddressAsDetectedAsVisited(RouteId: String;
  RouteDestinationId: integer; IsVisited: boolean);
var
  Example: TMarkAddressAsDetectedAsVisited;
begin
  Example := MakeExample(TMarkAddressAsDetectedAsVisited) as TMarkAddressAsDetectedAsVisited;
  try
    Example.Execute(RouteId, RouteDestinationId, IsVisited);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.MarkAddressAsVisited(RouteId: String;
  AddressId, MemberId: integer; IsVisited: boolean);
var
  Example: TMarkAddressAsVisited;
begin
  Example := MakeExample(TMarkAddressAsVisited) as TMarkAddressAsVisited;
  try
    Example.Execute(RouteId, AddressId, MemberId, IsVisited);
  finally
    FreeAndNil(Example);
  end;
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

procedure TRoute4MeExamples.PreviewFile(FileId: string);
var
  Example: TPreviewFile;
begin
  Example := MakeExample(TPreviewFile) as TPreviewFile;
  try
    Example.Execute(FileId);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.RegisterAccount(Plan, Industry, FirstName, LastName,
  Email: String; Terms: boolean; DeviceType: TDeviceType; Password,
  PasswordConfirmation: String);
var
  Example: TRegisterAccount;
begin
  Example := MakeExample(TRegisterAccount) as TRegisterAccount;
  try
    Example.Execute(Plan, Industry, FirstName, LastName, Email, Terms,
      DeviceType, Password, PasswordConfirmation);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.RegisterWebinar(EMail, FirstName, LastName, Phone,
  Company: String; MemberId: integer; Date: TDateTime);
var
  Example: TRegisterWebinar;
begin
  Example := MakeExample(TRegisterWebinar) as TRegisterWebinar;
  try
    Example.Execute(EMail, FirstName, LastName, Phone, Company, MemberId, Date);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.RemoveLocations(
  AddressIds: TArray<integer>);
var
  Example: TRemoveLocations;
begin
  Example := MakeExample(TRemoveLocations) as TRemoveLocations;
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

procedure TRoute4MeExamples.RemoveOrders(OrderIds: TIntegerArray);
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

procedure TRoute4MeExamples.RemoveTerritory(TerritoryId: String);
var
  Example: TRemoveTerritory;
begin
  Example := MakeExample(TRemoveTerritory) as TRemoveTerritory;
  try
    Example.Execute(TerritoryId);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.RemoveUser(MemberId: integer);
var
  Example: TRemoveUser;
begin
  Example := MakeExample(TRemoveUser) as TRemoveUser;
  try
    Example.Execute(MemberId);
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

procedure TRoute4MeExamples.ReverseGeocodeAddress(
  Location: TDirectionPathPoint);
var
  Example: TReverseGeocodeAddress;
begin
  Example := MakeExample(TReverseGeocodeAddress) as TReverseGeocodeAddress;
  try
    Example.Execute(Location);
  finally
    FreeAndNil(Example);
  end;
end;

function TRoute4MeExamples.SearchRoutesForSpecifiedText(
  Text: String): TDataObjectRouteList;
var
  Example: TSearchRoutesForSpecifiedText;
begin
  Example := MakeExample(TSearchRoutesForSpecifiedText) as TSearchRoutesForSpecifiedText;
  try
    Result := Example.Execute(Text);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.SearchVendors(Size: TVendorSizeType;
  IsIntegrated: boolean; Feature, Country, Search: String; Page,
  PerPage: integer);
var
  Example: TSearchVendors;
begin
  Example := MakeExample(TSearchVendors) as TSearchVendors;
  try
    Example.Execute(Size, IsIntegrated, Feature, Country, Search, Page, PerPage);
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

procedure TRoute4MeExamples.UpdateLocation(
  Contact: TAddressBookContact);
var
  Example: TUpdateLocation;
begin
  Example := MakeExample(TUpdateLocation) as TUpdateLocation;
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

function TRoute4MeExamples.UpdateConfigValue(Key, Value: String): boolean;
var
  Example: TUpdateConfigValue;
begin
  Example := MakeExample(TUpdateConfigValue) as TUpdateConfigValue;
  try
    Result := Example.Execute(Key, Value);
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

procedure TRoute4MeExamples.UpdateTerritory(TerritoryId: String);
var
  Example: TUpdateTerritory;
begin
  Example := MakeExample(TUpdateTerritory) as TUpdateTerritory;
  try
    Example.Execute(TerritoryId);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.UpdateUser(Parameters: TUserParameters);
var
  Example: TUpdateUser;
begin
  Example := MakeExample(TUpdateUser) as TUpdateUser;
  try
    Example.Execute(Parameters);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.UploadFileGeocoding(FileId: string);
var
  Example: TUploadFileGeocoding;
begin
  Example := MakeExample(TUploadFileGeocoding) as TUploadFileGeocoding;
  try
    Example.Execute(FileId);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.UserLicense(MemberId, SessionId: integer;
  DeviceId: String; DeviceType: TDeviceType;
  Subscription, Token, Payload: String);
var
  Example: TUserLicense;
begin
  Example := MakeExample(TUserLicense) as TUserLicense;
  try
    Example.Execute(MemberId, SessionId, DeviceId, DeviceType,
      Subscription, Token, Payload);
  finally
    FreeAndNil(Example);
  end;
end;

procedure TRoute4MeExamples.ValidateSession(SessionGuid: String; MemberId: integer);
var
  Example: TValidateSession;
begin
  Example := MakeExample(TValidateSession) as TValidateSession;
  try
    Example.Execute(SessionGuid, MemberId);
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
