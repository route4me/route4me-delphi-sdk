unit AddressUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections, System.Rtti, Classes, SysUtils,
  Generics.Defaults,
  JSONNullableAttributeUnit,
  NullableBasicTypesUnit, AddressNoteUnit, EnumsUnit,
  DirectionUnit, ManifestUnit, DirectionPathPointUnit, GeocodingUnit,
  JSONDictionaryIntermediateObjectUnit;

type
  /// <summary>
  /// Json schema for an Address class, which is used for Optimization and Route mantenance procedures
  /// </summary>
  /// <remarks>
  ///  https://github.com/route4me/json-schemas/blob/master/Address.dtd
  /// </remarks>
  TAddress = class
  private
    [JSONName('route_destination_id')]
    [Nullable]
    FRouteDestinationId: NullableInteger;

    [JSONName('alias')]
    FAlias: String;

    [JSONName('member_id')]
    [Nullable]
    FMemberId: NullableString;

    [JSONName('address')]
    FAddressString: String;

    [JSONName('is_depot')]
    [Nullable]
    FIsDepot: NullableBoolean;

    [JSONName('lat')]
    FLatitude: double;

    [JSONName('lng')]
    FLongitude: double;

    [JSONName('route_id')]
    [Nullable]
    FRouteId: NullableString;

    [JSONName('time')]
    [Nullable]
    FTime: NullableInteger;

    [JSONName('custom_fields')]
    [NullableObject(TDictionaryStringIntermediateObject)]
    FCustomFields: NullableObject;

    [JSONName('curbside_lat')]
    [Nullable(True)]
    FCurbsideLatitude: NullableDouble;

    [JSONName('curbside_lng')]
    [Nullable(True)]
    FCurbsideLongitude: NullableDouble;

    [JSONName('time_window_start')]
    [Nullable]
    FTimeWindowStart: NullableInteger;

    [JSONName('time_window_end')]
    [Nullable]
    FTimeWindowEnd: NullableInteger;

    [JSONName('time_window_start_2')]
    [Nullable]
    FTimeWindowStart2: NullableInteger;

    [JSONName('time_window_end_2')]
    [Nullable]
    FTimeWindowEnd2: NullableInteger;

    [JSONName('sequence_no')]
    [Nullable]
    FSequenceNo: NullableInteger;

    [JSONName('original_route_id')]
    [Nullable]
    FOriginalRouteId: NullableString;

    [JSONName('optimization_problem_id')]
    [Nullable]
    FOptimizationProblemId: NullableString;

    [JSONName('timeframe_violation_state')]
    [Nullable]
    FTimeframeViolationState: NullableInteger;

    [JSONName('timeframe_violation_time')]
    [Nullable]
    FTimeframeViolationTime: NullableInteger;

    [JSONName('timeframe_violation_rate')]
    [Nullable]
    FTimeframeViolationRate: NullableDouble;

    [JSONName('priority')]
    [Nullable]
    FPriority: NullableInteger;

    [JSONName('address_stop_type')]
    [Nullable]
    FAddressStopType: NullableString;

    [JSONName('geofence_detected_visited_timestamp')]
    [Nullable]
    FGeofenceDetectedVisitedTimestamp: NullableInteger;

    [JSONName('geofence_detected_departed_timestamp')]
    [Nullable]
    FGeofenceDetectedDepartedTimestamp: NullableInteger;

    [JSONName('geofence_detected_service_time')]
    [Nullable]
    FGeofenceDetectedServiceTime: NullableInteger;

    [JSONName('geofence_detected_visited_lat')]
    [Nullable]
    FGeofenceDetectedVisitedLat: NullableDouble;

    [JSONName('geofence_detected_visited_lng')]
    [Nullable]
    FGeofenceDetectedVisitedLng: NullableDouble;

    [JSONName('geofence_detected_departed_lat')]
    [Nullable]
    FGeofenceDetectedDepartedLat: NullableDouble;

    [JSONName('geofence_detected_departed_lng')]
    [Nullable]
    FGeofenceDetectedDepartedLng: NullableDouble;

    [JSONName('route_name')]
    [Nullable]
    FRouteName: NullableString;

    [JSONName('geocoded')]
    [Nullable]
    FGeocoded: NullableBoolean;

    [JSONName('preferred_geocoding')]
    [Nullable]
    FPreferredGeocoding: NullableInteger;

    [JSONName('failed_geocoding')]
    [Nullable]
    FFailedGeocoding: NullableBoolean;

    [JSONName('geocodings')]
    [NullableArray(TGeocoding)]
    FGeocodings: TGeocodingArray;

    [JSONName('contact_id')]
    [Nullable]
    FContactId: NullableInteger;

    [JSONName('is_visited')]
    [Nullable]
    FIsVisited: NullableBoolean;

    [JSONName('is_departed')]
    [Nullable]
    FIsDeparted: NullableBoolean;

    [JSONName('visited_lat')]
    [Nullable]
    FVisitedLat: NullableDouble;

    [JSONName('visited_lng')]
    [Nullable]
    FVisitedLng: NullableDouble;

    [JSONName('departed_lat')]
    [Nullable]
    FDepartedLat: NullableDouble;

    [JSONName('departed_lng')]
    [Nullable]
    FDepartedLng: NullableDouble;

    [JSONName('timestamp_last_visited')]
    [Nullable]
    FTimestampLastVisited: NullableInteger;

    [JSONName('timestamp_last_departed')]
    [Nullable]
    FTimestampLastDeparted: NullableInteger;

    [JSONName('customer_po')]
    [Nullable]
    FCustomerPo: NullableString;

    [JSONName('invoice_no')]
    [Nullable]
    FInvoiceNo: NullableString;

    [JSONName('reference_no')]
    [Nullable]
    FReferenceNo: NullableString;

    [JSONName('account_no')]
    [Nullable]
    FAccountNo: NullableString;

    [JSONName('order_no')]
    [Nullable]
    FOrderNo: NullableString;

    [JSONName('tracking_number')]
    [Nullable]
    FTrackingNumber: NullableString;

    [JSONName('weight')]
    [Nullable]
    FWeight: NullableDouble;

    [JSONName('cost')]
    [Nullable]
    FCost: NullableDouble;

    [JSONName('revenue')]
    [Nullable]
    FRevenue: NullableDouble;

    [JSONName('cube')]
    [Nullable]
    FCube: NullableDouble;

    [JSONName('pieces')]
    [Nullable]
    FPieces: NullableInteger;

    [JSONName('email')]
    [Nullable]
    FEmail: NullableString;

    [JSONName('phone')]
    [Nullable]
    FPhone: NullableString;

    [JSONName('destination_note_count')]
    [Nullable]
    FDestinationNoteCount: NullableInteger;

    [JSONName('drive_time_to_next_destination')]
    [Nullable]
    FDriveTimeToNextDestination: NullableInteger;

    [JSONName('abnormal_traffic_time_to_next_destination')]
    [Nullable]
    FAbnormalTrafficTimeToNextDestination: NullableInteger;

    [JSONName('uncongested_time_to_next_destination')]
    [Nullable]
    FUncongestedTimeToNextDestination: NullableInteger;

    [JSONName('distance_to_next_destination')]
    [Nullable]
    FDistanceToNextDestination: NullableDouble;

    [JSONName('generated_time_window_start')]
    [Nullable]
    FGeneratedTimeWindowStart: NullableInteger;

    [JSONName('generated_time_window_end')]
    [Nullable]
    FGeneratedTimeWindowEnd: NullableInteger;

    [JSONName('path_to_next')]
    [NullableArray(TDirectionPathPoint)]
    FPathToNext: TDirectionPathPointArray;

    [JSONName('directions')]
    [NullableArray(TDirection)]
    FDirections: TDirectionArray;

    [JSONName('manifest')]
    [NullableObject(TManifest)]
    FManifest: NullableObject;

    [JSONName('notes')]
    [NullableArray(TAddressNote)]
    FNotes: TAddressNoteArray;

    function GetCustomFields: TDictionaryStringIntermediateObject;
    procedure SetCustomFields(const Value: TDictionaryStringIntermediateObject);
    function GetAddressStopType: TAddressStopType;
    procedure SetAddressStopType(const Value: TAddressStopType);
    function GetManifest: TManifest;
    procedure SetManifest(const Value: TManifest);
  public
    /// <remarks>
    ///  Constructor with 0-arguments must be and be public.
    ///  For JSON-deserialization.
    /// </remarks>
    constructor Create; overload; virtual;
    constructor Create(AddressString: String; Latitude, Longitude: double; Time: NullableInteger); overload;
    constructor Create(AddressString: String; Alias: String; Latitude, Longitude: double; Time: NullableInteger); overload;
    constructor Create(AddressString: String; Latitude, Longitude: double; Time: NullableInteger; TimeWindowStart, TimeWindowEnd: integer); overload;
    constructor Create(AddressString: String; Latitude, Longitude: double; Time: NullableInteger;
      TimeWindowStart, TimeWindowEnd, TimeWindowStart2, TimeWindowEnd2: integer); overload;
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; override;

    /// <summary>
    /// The route Address Line 1
    /// </summary>
    property AddressString: String read FAddressString write FAddressString;

    /// <summary>
    /// Address Alias
    /// </summary>
    property Alias: String read FAlias write FAlias;

    /// <summary>
    /// Internal unique address identifier
    /// </summary>
    property RouteDestinationId: NullableInteger read FRouteDestinationId write FRouteDestinationId;

    /// <summary>
    /// Member ID
    /// </summary>
    property MemberId: NullableString read FMemberId write FMemberId;

    /// <summary>
    /// This address is a depot
    /// </summary>
    property IsDepot: NullableBoolean read FIsDepot write FIsDepot;

    /// <summary>
    /// The latitude of this address
    /// </summary>

    property Latitude: double read FLatitude write FLatitude;

    /// <summary>
    /// The longitude of this address
    /// </summary>
    property Longitude: double read FLongitude write FLongitude;

    /// <summary>
    /// The id of the route being viewed, modified, erased
    /// </summary>
    property RouteId: NullableString read FRouteId write FRouteId;

    /// <summary>
    /// Service time (seconds)
    /// </summary>
    property Time: NullableInteger read FTime write FTime;

    property CustomFields: TDictionaryStringIntermediateObject read GetCustomFields write SetCustomFields;
    procedure AddCustomField(Key: String; Value: String);

    /// <summary>
    ///  Generate optimal routes and driving directions to this curbside latitude
    /// </summary>
    property CurbsideLatitude: NullableDouble read FCurbsideLatitude write FCurbsideLatitude;

    /// <summary>
    ///  Generate optimal routes and driving directions to this curbside longitude
    /// </summary>
    property CurbsideLongitude: NullableDouble read FCurbsideLongitude write FCurbsideLongitude;

    /// <summary>
    ///  Time Window Start in seconds, relative to the route start date (midnight), UTC time zone. It is relative to start datebecause start time changes would shift time windows
    /// </summary>
    property TimeWindowStart: NullableInteger read FTimeWindowStart write FTimeWindowStart;

    /// <summary>
    ///  Time Window End in seconds, relative to the route start date (midnight), UTC time zone. It is relative to start datebecause start time changes would shift time windows
    /// </summary>
    property TimeWindowEnd: NullableInteger read FTimeWindowEnd write FTimeWindowEnd;

    /// <summary>
    ///  See time_window_start
    /// </summary>
    property TimeWindowStart2: NullableInteger read FTimeWindowStart2 write FTimeWindowStart2;

    /// <summary>
    ///  See time_window_end
    /// </summary>
    property TimeWindowEnd2: NullableInteger read FTimeWindowEnd2 write FTimeWindowEnd2;

    /// <summary>
    ///  The sequence number for the address
    /// </summary>
    property SequenceNo: NullableInteger read FSequenceNo write FSequenceNo;

    /// <summary>
    ///  The original customer-specific route id assigned at route creation time
    /// </summary>
    property OriginalRouteId: NullableString read FOriginalRouteId write FOriginalRouteId;

    /// <summary>
    ///  the id of the optimization request that was used to initially instantiate this route
    /// </summary>
    property OptimizationProblemId: NullableString read FOptimizationProblemId write FOptimizationProblemId;

    /// <summary>
    ///  State of the timeframe violation. In a response only
    /// </summary>
    property TimeframeViolationState: NullableInteger read FTimeframeViolationState write FTimeframeViolationState;

    /// <summary>
    ///  Moment of the timeframe violation. In a response only
    /// </summary>
    property TimeframeViolationTime: NullableInteger read FTimeframeViolationTime write FTimeframeViolationTime;

    /// <summary>
    ///  Rate of the timeframe violation. In a response only
    /// </summary>
    property TimeframeViolationRate: NullableDouble read FTimeframeViolationRate write FTimeframeViolationRate;

    /// <summary>
    ///  0 is the highest priority; n has higher priority than n + 1
    /// </summary>
    property Priority: NullableInteger read FPriority write FPriority;


    /// <summary>
    ///  The type of stop that this is (PICKUP, DELIVERY, BREAK, MEETUP)
    /// </summary>
    property AddressStopType: TAddressStopType read GetAddressStopType write SetAddressStopType;

    /// <summary>
    ///  Timestamp of a geofence detected visited
    /// </summary>
    property GeofenceDetectedVisitedTimestamp: NullableInteger read FGeofenceDetectedVisitedTimestamp write FGeofenceDetectedVisitedTimestamp;

    /// <summary>
    ///  Timestamp of a geofence detected departed
    /// </summary>
    property GeofenceDetectedDepartedTimestamp: NullableInteger read FGeofenceDetectedDepartedTimestamp write FGeofenceDetectedDepartedTimestamp;

    /// <summary>
    ///  The service time of a detected geofence
    /// </summary>
    property GeofenceDetectedServiceTime: NullableInteger read FGeofenceDetectedServiceTime write FGeofenceDetectedServiceTime;

    /// <summary>
    ///  Latitude of a visited detected geofence
    /// </summary>
    property GeofenceDetectedVisitedLat: NullableDouble read FGeofenceDetectedVisitedLat write FGeofenceDetectedVisitedLat;

    /// <summary>
    ///  Longitude of a visited detected geofence
    /// </summary>
    property GeofenceDetectedVisitedLng: NullableDouble read FGeofenceDetectedVisitedLng write FGeofenceDetectedVisitedLng;

    /// <summary>
    ///  Latitude of a departed detected geofence
    /// </summary>
    property GeofenceDetectedDepartedLat: NullableDouble read FGeofenceDetectedDepartedLat write FGeofenceDetectedDepartedLat;

    /// <summary>
    ///  Longitude of a departed detected geofence
    /// </summary>
    property GeofenceDetectedDepartedLng: NullableDouble read FGeofenceDetectedDepartedLng write FGeofenceDetectedDepartedLng;

    /// <summary>
    ///  Route Name
    /// </summary>
    property RouteName: NullableString read FRouteName write FRouteName;

    /// <summary>
    ///  True means the 'address' field was successfully geocoded
    /// </summary>
    property Geocoded: NullableBoolean read FGeocoded write FGeocoded;

    /// <summary>
    ///  Index of 'geocodings' array that the user has chosen
    /// </summary>
    property PreferredGeocoding: NullableInteger read FPreferredGeocoding write FPreferredGeocoding;

    /// <summary>
    ///  True means there was a geocoding attempt which failed. False means success or no geocoding
    /// </summary>
    property FailedGeocoding: NullableBoolean read FFailedGeocoding write FFailedGeocoding;

    /// <summary>
    ///  Geocodings ID
    /// </summary>
    property Geocodings: TGeocodingArray read FGeocodings;
    procedure AddGeocoding(Geocoding: TGeocoding);

    /// <summary>
    ///  Address book contact id (0 means not connected to the address book)
    /// </summary>
    property ContactId: NullableInteger read FContactId write FContactId;

    /// <summary>
    ///  The driver pressed the 'Visited' button
    /// </summary>
    property IsVisited: NullableBoolean read FIsVisited write FIsVisited;

    /// <summary>
    ///  The driver marked the 'Departed' button
    /// </summary>
    property IsDeparted: NullableBoolean read FIsDeparted write FIsDeparted;

    /// <summary>
    ///  Last known check in latitude
    /// </summary>
    property VisitedLat: NullableDouble read FVisitedLat write FVisitedLat;

    /// <summary>
    ///  Last known check in longitude
    /// </summary>
    property VisitedLng: NullableDouble read FVisitedLng write FVisitedLng;

    /// <summary>
    ///  Last known departed latitude
    /// </summary>
    property DepartedLat: NullableDouble read FDepartedLat write FDepartedLat;

    /// <summary>
    ///  Last known departed longitude
    /// </summary>
    property DepartedLng: NullableDouble read FDepartedLng write FDepartedLng;

    /// <summary>
    ///  Timestamp when the driver presses 'Visited'
    /// </summary>
    property TimestampLastVisited: NullableInteger read FTimestampLastVisited write FTimestampLastVisited;

    /// <summary>
    ///  Timestamp when the driver marks the stop as 'Departed'
    /// </summary>
    property TimestampLastDeparted: NullableInteger read FTimestampLastDeparted write FTimestampLastDeparted;

    /// <summary>
    ///  The customer purchase order for the address
    /// </summary>
    property CustomerPo: NullableString read FCustomerPo write FCustomerPo;

    /// <summary>
    ///  The invoice number for the address
    /// </summary>
    property InvoiceNo: NullableString read FInvoiceNo write FInvoiceNo;

    /// <summary>
    ///  The reference number for the address
    /// </summary>
    property ReferenceNo: NullableString read FReferenceNo write FReferenceNo;

    /// <summary>
    ///  The account number for the address
    /// </summary>
    property AccountNo: NullableString read FAccountNo write FAccountNo;

    /// <summary>
    ///  The order number for the address
    /// </summary>
    property OrderNo: NullableString read FOrderNo write FOrderNo;

    /// <summary>
    ///  Systemwide unique code, which permits end-users (recipients) to track the status of their order
    /// </summary>
    property TrackingNumber: NullableString read FTrackingNumber write FTrackingNumber;

    /// <summary>
    ///  Weight
    /// </summary>
    property Weight: NullableDouble read FWeight write FWeight;

    /// <summary>
    ///  The cost of the order for the address
    /// </summary>
    property Cost: NullableDouble read FCost write FCost;

    /// <summary>
    ///  The total revenue for the address
    /// </summary>
    property Revenue: NullableDouble read FRevenue write FRevenue;

    /// <summary>
    ///  The cubic volume of the cargo being delivered or picked up at the address
    /// </summary>
    property Cube: NullableDouble read FCube write FCube;

    /// <summary>
    ///  Pieces
    /// </summary>
    property Pieces: NullableInteger read FPieces write FPieces;

    /// <summary>
    ///  A valid e-mail address assigned to this stop
    /// </summary>
    property Email: NullableString read FEmail write FEmail;

    /// <summary>
    ///  Customer Phone
    /// </summary>
    property Phone: NullableString read FPhone write FPhone;

    /// <summary>
    ///  How many notes have been added to this destination
    /// </summary>
    property DestinationNoteCount: NullableInteger read FDestinationNoteCount write FDestinationNoteCount;

    /// <summary>
    ///  Time to next destination in seconds
    /// </summary>
    property DriveTimeToNextDestination: NullableInteger read FDriveTimeToNextDestination write FDriveTimeToNextDestination;

    /// <summary>
    ///  Abnormal traffic time to next destination
    /// </summary>
    property AbnormalTrafficTimeToNextDestination: NullableInteger read FAbnormalTrafficTimeToNextDestination write FAbnormalTrafficTimeToNextDestination;

    /// <summary>
    ///  Supposing that there was no traffic at all, this gives how many seconds it takes to get to the next stop
    /// </summary>
    property UncongestedTimeToNextDestination: NullableInteger read FUncongestedTimeToNextDestination write FUncongestedTimeToNextDestination;

    /// <summary>
    ///  Distance to next destination in route unit (the default unit is in miles)
    /// </summary>
    property DistanceToNextDestination: NullableDouble read FDistanceToNextDestination write FDistanceToNextDestination;

    /// <summary>
    ///  Generated Time Window Start in seconds
    /// </summary>
    property GeneratedTimeWindowStart: NullableInteger read FGeneratedTimeWindowStart write FGeneratedTimeWindowStart;

    /// <summary>
    ///  Generated Time Window End in seconds
    /// </summary>
    property GeneratedTimeWindowEnd: NullableInteger read FGeneratedTimeWindowEnd write FGeneratedTimeWindowEnd;

    /// <summary>
    /// </summary>
    property PathToNext: TDirectionPathPointArray read FPathToNext;
    procedure AddPathToNext(DirectionPathPoint: TDirectionPathPoint);

    /// <summary>
    /// </summary>
    property Directions: TDirectionArray read FDirections;
    procedure AddDirection(Direction: TDirection);

    /// <summary>
    ///  The manifest contains values derived from other values
    /// </summary>
    property Manifest: TManifest read GetManifest write SetManifest;

    /// <summary>
    ///  Notes
    /// </summary>
    property Notes: TAddressNoteArray read FNotes;
    procedure AddNote(Note: TAddressNote);
  end;

  TAddressesArray = TArray<TAddress>;
  TAddressesList = TList<TAddress>;

  function SortAddresses(Addresses: TAddressesArray): TAddressesArray;

implementation

uses
  Math;

{ TAddress }

constructor TAddress.Create(AddressString: String; Latitude, Longitude: double;
  Time: NullableInteger);
begin
  Create;

  FAddressString := AddressString;
  FLatitude := Latitude;
  FLongitude := Longitude;
  FTime := Time;
end;

procedure TAddress.AddCustomField(Key: String; Value: String);
var
  Dic: TDictionaryStringIntermediateObject;
begin
  if (FCustomFields.IsNull) then
    FCustomFields := TDictionaryStringIntermediateObject.Create();
  Dic := FCustomFields.Value as TDictionaryStringIntermediateObject;
  Dic.Add(Key, Value);
end;

constructor TAddress.Create(AddressString, Alias: String; Latitude,
  Longitude: double; Time: NullableInteger);
begin
  Create(AddressString, Latitude, Longitude, Time);
  FAlias := Alias;
end;

constructor TAddress.Create;
begin
  FAlias := EmptyStr;

  FCurbsideLatitude := NullableDouble.Null;
  FCurbsideLongitude := NullableDouble.Null;
  FMemberId := NullableString.Null;
  FRouteDestinationId := NullableInteger.Null;
  FRouteId := NullableString.Null;
  FIsDepot := NullableBoolean.Null;
  FTime := NullableInteger.Null;
  FTimeWindowStart := NullableInteger.Null;
  FTimeWindowEnd := NullableInteger.Null;
  FTimeWindowStart2 := NullableInteger.Null;
  FTimeWindowEnd2 := NullableInteger.Null;
  FSequenceNo := NullableInteger.Null;
  FOriginalRouteId := NullableString.Null;
  FOptimizationProblemId := NullableString.Null;
  FTimeframeViolationState := NullableInteger.Null;
  FTimeframeViolationTime := NullableInteger.Null;
  FTimeframeViolationRate := NullableDouble.Null;
  FPriority := NullableInteger.Null;
  FAddressStopType := NullableString.Null;
  FGeofenceDetectedVisitedTimestamp := NullableInteger.Null;
  FGeofenceDetectedDepartedTimestamp := NullableInteger.Null;
  FGeofenceDetectedServiceTime := NullableInteger.Null;
  FGeofenceDetectedVisitedLat := NullableDouble.Null;
  FGeofenceDetectedVisitedLng := NullableDouble.Null;
  FGeofenceDetectedDepartedLat := NullableDouble.Null;
  FGeofenceDetectedDepartedLng := NullableDouble.Null;
  FRouteName := NullableString.Null;
  FGeocoded := NullableBoolean.Null;
  FPreferredGeocoding := NullableInteger.Null;
  FFailedGeocoding := NullableBoolean.Null;
  FContactId := NullableInteger.Null;
  FIsVisited := NullableBoolean.Null;
  FIsDeparted := NullableBoolean.Null;
  FVisitedLat := NullableDouble.Null;
  FVisitedLng := NullableDouble.Null;
  FDepartedLat := NullableDouble.Null;
  FDepartedLng := NullableDouble.Null;
  FTimestampLastVisited := NullableInteger.Null;
  FTimestampLastDeparted := NullableInteger.Null;
  FCustomerPo := NullableString.Null;
  FInvoiceNo := NullableString.Null;
  FReferenceNo := NullableString.Null;
  FAccountNo := NullableString.Null;
  FOrderNo := NullableString.Null;
  FTrackingNumber := NullableString.Null;
  FWeight := NullableDouble.Null;
  FCost := NullableDouble.Null;
  FRevenue := NullableDouble.Null;
  FCube := NullableDouble.Null;
  FPieces := NullableInteger.Null;
  FEmail := NullableString.Null;
  FPhone := NullableString.Null;
  FDestinationNoteCount := NullableInteger.Null;
  FDriveTimeToNextDestination := NullableInteger.Null;
  FAbnormalTrafficTimeToNextDestination := NullableInteger.Null;
  FUncongestedTimeToNextDestination := NullableInteger.Null;
  FDistanceToNextDestination := NullableDouble.Null;
  FGeneratedTimeWindowStart := NullableInteger.Null;
  FGeneratedTimeWindowEnd := NullableInteger.Null;
  FManifest := NullableObject.Null;
  FCustomFields := NullableObject.Null;

  SetLength(FGeocodings, 0);
  SetLength(FNotes, 0);
  SetLength(FPathToNext, 0);
  SetLength(FDirections, 0);
end;

constructor TAddress.Create(AddressString: String; Latitude, Longitude: double;
  Time: NullableInteger; TimeWindowStart, TimeWindowEnd: integer);
begin
  Create(AddressString, Latitude, Longitude, Time);
  FTimeWindowStart := TimeWindowStart;
  FTimeWindowEnd := TimeWindowEnd;
end;

procedure TAddress.AddNote(Note: TAddressNote);
begin
  SetLength(FNotes, Length(FNotes) + 1);
  FNotes[High(FNotes)] := Note;
end;

procedure TAddress.AddPathToNext(DirectionPathPoint: TDirectionPathPoint);
begin
  SetLength(FPathToNext, Length(FPathToNext) + 1);
  FPathToNext[High(FPathToNext)] := DirectionPathPoint;
end;

constructor TAddress.Create(AddressString: String; Latitude, Longitude: double;
  Time: NullableInteger; TimeWindowStart, TimeWindowEnd, TimeWindowStart2,
  TimeWindowEnd2: integer);
begin
  Create(AddressString, Latitude, Longitude, Time, TimeWindowStart, TimeWindowEnd);
  FTimeWindowStart2 := TimeWindowStart2;
  FTimeWindowEnd2 := TimeWindowEnd2;
end;

destructor TAddress.Destroy;
var
  i: integer;
begin
  for i := Length(FPathToNext) - 1 downto 0 do
    FreeAndNil(FPathToNext[i]);

  for i := Length(FDirections) - 1 downto 0 do
    FreeAndNil(FDirections[i]);

  for i := Length(FGeocodings) - 1 downto 0 do
    FreeAndNil(FGeocodings[i]);

  for i := Length(FNotes) - 1 downto 0 do
    FreeAndNil(FNotes[i]);

  FManifest.Free;
  FCustomFields.Free;

  inherited;
end;

function TAddress.Equals(Obj: TObject): Boolean;
var
  Other: TAddress;
  i: integer;
  SortedPathToNext1, SortedPathToNext2: TDirectionPathPointArray;
  SortedDirections1, SortedDirections2: TDirectionArray;
  SortedNotes1, SortedNotes2: TAddressNoteArray;
  SortedGeocodings1, SortedGeocodings2: TGeocodingArray;
begin
  Result := False;

  if not (Obj is TAddress) then
    Exit;

  Other := TAddress(Obj);

  Result := (FAddressString = Other.FAddressString) and
    (FAlias = Other.FAlias) and
    (FRouteDestinationId = Other.FRouteDestinationId) and
    (FMemberId = Other.FMemberId) and
    (FIsDepot = Other.FIsDepot) and
    (FLatitude = Other.FLatitude) and
    (FLongitude = Other.FLongitude) and
    (FRouteId = Other.FRouteId) and
    (FTime = Other.FTime) and
    (FCurbsideLatitude = Other.FCurbsideLatitude) and
    (FCurbsideLongitude = Other.FCurbsideLongitude) and
    (FTimeWindowStart = Other.FTimeWindowStart) and
    (FTimeWindowEnd = Other.FTimeWindowEnd) and
    (FTimeWindowStart2 = Other.FTimeWindowStart2) and
    (FTimeWindowEnd2 = Other.FTimeWindowEnd2) and
    (FSequenceNo = Other.FSequenceNo) and
    (FOriginalRouteId = Other.FOriginalRouteId) and
    (FOptimizationProblemId = Other.FOptimizationProblemId) and
    (FTimeframeViolationState = Other.FTimeframeViolationState) and
    (FTimeframeViolationTime = Other.FTimeframeViolationTime) and
    (FTimeframeViolationRate = Other.FTimeframeViolationRate) and
    (FPriority = Other.FPriority) and
    (FAddressStopType = Other.FAddressStopType) and
    (FGeofenceDetectedVisitedTimestamp = Other.FGeofenceDetectedVisitedTimestamp) and
    (FGeofenceDetectedDepartedTimestamp = Other.FGeofenceDetectedDepartedTimestamp) and
    (FGeofenceDetectedServiceTime = Other.FGeofenceDetectedServiceTime) and
    (FTimeframeViolationRate = Other.FTimeframeViolationRate) and
    (FGeofenceDetectedVisitedLat = Other.FGeofenceDetectedVisitedLat) and
    (FGeofenceDetectedVisitedLng = Other.FGeofenceDetectedVisitedLng) and
    (FGeofenceDetectedDepartedLat = Other.FGeofenceDetectedDepartedLat) and
    (FGeofenceDetectedDepartedLng = Other.FGeofenceDetectedDepartedLng) and
    (FRouteName = Other.FRouteName) and
    (FGeocoded = Other.FGeocoded) and
    (FPreferredGeocoding = Other.FPreferredGeocoding) and
    (FFailedGeocoding = Other.FFailedGeocoding) and
    (FContactId = Other.FContactId) and
    (FIsVisited = Other.FIsVisited) and
    (FIsDeparted = Other.FIsDeparted) and
    (FVisitedLat = Other.FVisitedLat) and
    (FVisitedLng = Other.FVisitedLng) and
    (FDepartedLat = Other.FDepartedLat) and
    (FDepartedLng = Other.FDepartedLng) and
    (FTimestampLastVisited = Other.FTimestampLastVisited) and
    (FTimestampLastDeparted = Other.FTimestampLastDeparted) and
    (FCustomerPo = Other.FCustomerPo) and
    (FInvoiceNo = Other.FInvoiceNo) and
    (FReferenceNo = Other.FReferenceNo) and
    (FAccountNo = Other.FAccountNo) and
    (FOrderNo = Other.FOrderNo) and
    (FTrackingNumber = Other.FTrackingNumber) and
    (FWeight = Other.FWeight) and
    (FCost = Other.FCost) and
    (FRevenue = Other.FRevenue) and
    (FCube = Other.FCube) and
    (FPieces = Other.FPieces) and
    (FEmail = Other.FEmail) and
    (FPhone = Other.FPhone) and
    (FDestinationNoteCount = Other.FDestinationNoteCount) and
    (FDriveTimeToNextDestination = Other.FDriveTimeToNextDestination) and
    (FAbnormalTrafficTimeToNextDestination = Other.FAbnormalTrafficTimeToNextDestination) and
    (FUncongestedTimeToNextDestination = Other.FUncongestedTimeToNextDestination) and
    (FDistanceToNextDestination = Other.FDistanceToNextDestination) and
    (FGeneratedTimeWindowStart = Other.FGeneratedTimeWindowStart) and
    (FGeneratedTimeWindowEnd = Other.FGeneratedTimeWindowEnd) and
    (FManifest = Other.FManifest) and
    (FCustomFields = Other.FCustomFields);

  if not Result then
    Exit;

  Result := False;

  if (Length(FPathToNext) <> Length(Other.FPathToNext)) or
    (Length(FDirections) <> Length(Other.FDirections)) or
    (Length(FGeocodings) <> Length(Other.FGeocodings)) or
    (Length(FNotes) <> Length(Other.FNotes)) then
    Exit;

  SortedDirections1 := DirectionUnit.SortDirections(Directions);
  SortedDirections2 := DirectionUnit.SortDirections(Other.Directions);
  for i := 0 to Length(SortedDirections1) - 1 do
    if (not SortedDirections1[i].Equals(SortedDirections2[i])) then
      Exit;

  SortedGeocodings1 := GeocodingUnit.SortGeocodings(Geocodings);
  SortedGeocodings2 := GeocodingUnit.SortGeocodings(Other.Geocodings);
  for i := 0 to Length(SortedGeocodings1) - 1 do
    if (not SortedGeocodings1[i].Equals(SortedGeocodings2[i])) then
      Exit;

  SortedNotes1 := AddressNoteUnit.SortAddressNotes(Notes);
  SortedNotes2 := AddressNoteUnit.SortAddressNotes(Other.Notes);
  for i := 0 to Length(SortedNotes1) - 1 do
    if (not SortedNotes1[i].Equals(SortedNotes2[i])) then
      Exit;

  SortedPathToNext1 := DirectionPathPointUnit.SortDirectionPathPoints(PathToNext);
  SortedPathToNext2 := DirectionPathPointUnit.SortDirectionPathPoints(Other.PathToNext);
  for i := 0 to Length(SortedPathToNext1) - 1 do
    if (not SortedPathToNext1[i].Equals(SortedPathToNext2[i])) then
      Exit;

  Result := True;
end;

function TAddress.GetAddressStopType: TAddressStopType;
var
  AddressStopType: TAddressStopType;
begin
  Result := TAddressStopType.astUnknown;
  if FAddressStopType.IsNotNull then
    for AddressStopType := Low(TAddressStopType) to High(TAddressStopType) do
      if (FAddressStopType = TAddressStopTypeDescription[AddressStopType]) then
        Exit(AddressStopType);
end;

function TAddress.GetCustomFields: TDictionaryStringIntermediateObject;
begin
  if FCustomFields.IsNull then
    Result := nil
  else
    Result := FCustomFields.Value as TDictionaryStringIntermediateObject;
end;

function TAddress.GetManifest: TManifest;
begin
  if (FManifest.IsNull) then
    Result := nil
  else
    Result := FManifest.Value as TManifest;
end;

procedure TAddress.SetAddressStopType(const Value: TAddressStopType);
begin
  FAddressStopType := TAddressStopTypeDescription[Value];
end;

procedure TAddress.SetCustomFields(
  const Value: TDictionaryStringIntermediateObject);
begin
  FCustomFields := Value;
end;

procedure TAddress.SetManifest(const Value: TManifest);
begin
  FManifest := Value;
end;

procedure TAddress.AddDirection(Direction: TDirection);
begin
  SetLength(FDirections, Length(FDirections) + 1);
  FDirections[High(FDirections)] := Direction;
end;

procedure TAddress.AddGeocoding(Geocoding: TGeocoding);
begin
  SetLength(FGeocodings, Length(FGeocodings) + 1);
  FGeocodings[High(FGeocodings)] := Geocoding;
end;

function SortAddresses(Addresses: TAddressesArray): TAddressesArray;
begin
  SetLength(Result, Length(Addresses));
  if Length(Addresses) = 0 then
    Exit;

  TArray.Copy<TAddress>(Addresses, Result, Length(Addresses));
  TArray.Sort<TAddress>(Result, TComparer<TAddress>.Construct(
    function (const Address1, Address2: TAddress): Integer
    begin
      Result := IfThen(Address1.SequenceNo.IsNotNull, Address1.SequenceNo.Value, -1) -
        IfThen(Address2.SequenceNo.IsNotNull, Address2.SequenceNo.Value, -1);
      if (result = 0) then
        Result := IfThen(Address1.IsDepot.IsNotNull and Address1.IsDepot.Value, 0, 1) -
          IfThen(Address2.IsDepot.IsNotNull and Address2.IsDepot.Value, 0, 1)
    end));
end;

end.
