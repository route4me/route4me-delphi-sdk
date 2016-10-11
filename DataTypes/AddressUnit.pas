unit AddressUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections, System.Rtti, Classes, SysUtils,
  JSONNullableAttributeUnit,
  NullableBasicTypesUnit;

type
  TAddress = class
  private
    [JSONName('route_destination_id')]
    [JSONNullable]
    FRouteDestinationId: NullableInteger;

    [JSONName('alias')]
    [JSONNullable]
    FAlias: NullableString;

    [JSONName('member_id')]
    [JSONNullable]
    FMemberId: NullableString;

    [JSONName('address')]
    FAddressString: String;

    [JSONName('is_depot')]
    [JSONNullable]
    FIsDepot: NullableBoolean;

    [JSONName('lat')]
    FLatitude: double;

    [JSONName('lng')]
    FLongitude: double;

    [JSONName('route_id')]
    [JSONNullable]
    FRouteId: NullableString;

    [JSONName('time')]
    [JSONNullable]
    FTime: NullableInteger;

    [JSONName('custom_fields')]
    [JSONNullable]
    FCustomFields: NullableObject;

    [JSONName('curbside_lat')]
    [JSONNullable(True)]
    FCurbsideLatitude: NullableDouble;

    [JSONName('curbside_lng')]
    [JSONNullable(True)]
    FCurbsideLongitude: NullableDouble;

    [JSONName('time_window_start')]
    [JSONNullable]
    FTimeWindowStart: NullableInteger;

    [JSONName('time_window_end')]
    [JSONNullable]
    FTimeWindowEnd: NullableInteger;

    [JSONName('time_window_start_2')]
    [JSONNullable]
    FTimeWindowStart2: NullableInteger;

    [JSONName('time_window_end_2')]
    [JSONNullable]
    FTimeWindowEnd2: NullableInteger;

    [JSONName('sequence_no')]
    [JSONNullable]
    FSequenceNo: NullableInteger;
  public
    constructor Create; overload;
    constructor Create(AddressString: String; Latitude, Longitude: double; Time: NullableInteger); overload;
    constructor Create(AddressString: String; Alias: String; Latitude, Longitude: double; Time: NullableInteger); overload;
    constructor Create(AddressString: String; Latitude, Longitude: double; Time: NullableInteger; TimeWindowStart, TimeWindowEnd: integer); overload;
    constructor Create(AddressString: String; Latitude, Longitude: double; Time: NullableInteger;
      TimeWindowStart, TimeWindowEnd, TimeWindowStart2, TimeWindowEnd2: integer); overload;

    procedure AddCustomField(Key: String; Value: String);

    property AddressString: String read FAddressString write FAddressString;
    property Alias: NullableString read FAlias write FAlias;

    property RouteDestinationId: NullableInteger read FRouteDestinationId write FRouteDestinationId;

    /// <summary>
    /// The id of the member inside the route4me system
    /// </summary>
    property MemberId: NullableString read FMemberId write FMemberId;

    /// <summary>
    /// Designate this stop as a depot.
    /// A route may have multiple depots/points of origin
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
    /// The expected amount of time that will be spent at this address by the driver/user
    /// </summary>
    property Time: NullableInteger read FTime write FTime;

    property CustomFields: NullableObject read FCustomFields write FCustomFields;

    /// <summary>
    ///  Generate optimal routes and driving directions to this curbside latitude
    /// </summary>
    property CurbsideLatitude: NullableDouble read FCurbsideLatitude write FCurbsideLatitude;

    /// <summary>
    ///  Generate optimal routes and driving directions to this curbside longitude
    /// </summary>
    property CurbsideLongitude: NullableDouble read FCurbsideLongitude write FCurbsideLongitude;

    property TimeWindowStart: NullableInteger read FTimeWindowStart write FTimeWindowStart;

    property TimeWindowEnd: NullableInteger read FTimeWindowEnd write FTimeWindowEnd;

    property TimeWindowStart2: NullableInteger read FTimeWindowStart2 write FTimeWindowStart2;

    property TimeWindowEnd2: NullableInteger read FTimeWindowEnd2 write FTimeWindowEnd2;

    property SequenceNo: NullableInteger read FSequenceNo write FSequenceNo;

(*
    //if this route was duplicated from an existing route, this value would have the original route's id
    [DataMember(Name = 'original_route_id', EmitDefaultValue = false)]
    property string OriginalRouteId { get; set; }

    //the id of the optimization request that was used to initially instantiate this route
    [DataMember(Name = 'optimization_problem_id', EmitDefaultValue = false)]
    property string OptimizationProblemId { get; set; }

    [DataMember(Name = 'geocoded', EmitDefaultValue = false)]
    property bool? Geocoded { get; set; }

    [DataMember(Name = 'preferred_geocoding', EmitDefaultValue = false)]
    property int? PreferredGeocoding { get; set; }

    [DataMember(Name = 'FailedGeocoding', EmitDefaultValue = false)]
    property bool? FailedGeocoding { get; set; }

    //when planning a route from the address book or using existing address book ids
    //pass the address book id (contact_id) for an address so that route4me can run
    //analytics on the address book addresses that were used to plan routes, and to find previous visits to
    //favorite addresses
    [DataMember(Name = 'contact_id', EmitDefaultValue = false)]
    property int? ContactId { get; set; }


    //status flag to mark an address as visited (aka check in)
    [DataMember(Name = 'is_visited', EmitDefaultValue = false)]
    property bool? IsVisited { get; set; }

    //status flag to mark an address as departed (aka check out)
    [DataMember(Name = 'is_departed', EmitDefaultValue = false)]
    property bool? IsDeparted { get; set; }

    //the last known visited timestamp of this address
    [DataMember(Name = 'timestamp_last_visited', EmitDefaultValue = false)]
    property uint? TimestampLastVisited { get; set; }

    //the last known departed timestamp of this address
    [DataMember(Name = 'timestamp_last_departed', EmitDefaultValue = false)]
    property uint? TimestampLastDeparted { get; set; }

    //pass-through data about this route destination
    //the data will be visible on the manifest, website, and mobile apps
    [DataMember(Name = 'customer_po', EmitDefaultValue = false)]
    property object CustomerPo { get; set; }

    //pass-through data about this route destination
    //the data will be visible on the manifest, website, and mobile apps
    [DataMember(Name = 'invoice_no', EmitDefaultValue = false)]
    property object InvoiceNo { get; set; }

    //pass-through data about this route destination
    //the data will be visible on the manifest, website, and mobile apps
    [DataMember(Name = 'reference_no', EmitDefaultValue = false)]
    property object ReferenceNo { get; set; }

    //pass-through data about this route destination
    //the data will be visible on the manifest, website, and mobile apps
    [DataMember(Name = 'order_no', EmitDefaultValue = false)]
    property object OrderNo { get; set; }

    [DataMember(Name = 'weight', EmitDefaultValue = false)]
    property object Weight { get; set; }

    [DataMember(Name = 'cost', EmitDefaultValue = false)]
    property object Cost { get; set; }

    [DataMember(Name = 'revenue', EmitDefaultValue = false)]
    property object Revenue { get; set; }

    //the cubic volume that this destination/order/line-item consumes/contains
    //this is how much space it will take up on a vehicle
    [DataMember(Name = 'cube', EmitDefaultValue = false)]
    property object Cube { get; set; }

    //the number of pieces/palllets that this destination/order/line-item consumes/contains on a vehicle
    [DataMember(Name = 'pieces', EmitDefaultValue = false)]
    property object Pieces { get; set; }

    //pass-through data about this route destination
    //the data will be visible on the manifest, website, and mobile apps
    //also used to email clients when vehicles are approaching (future capability)
    [DataMember(Name = 'email', EmitDefaultValue = false)]
    property object Email { get; set; }

    //pass-through data about this route destination
    //the data will be visible on the manifest, website, and mobile apps
    //also used to sms message clients when vehicles are approaching (future capability)
    [DataMember(Name = 'phone', EmitDefaultValue = false)]
    property object Phone { get; set; }

    //the number of notes that are already associated with this address on the route
    [DataMember(Name = 'destination_note_count', EmitDefaultValue = false)]
    property int? DestinationNoteCount { get; set; }

    //server-side generated amount of km/miles that it will take to get to the next location on the route
    [DataMember(Name = 'drive_time_to_next_destination', EmitDefaultValue = false)]
    property int? DriveTimeToNextDestination { get; set; }

    //server-side generated amount of seconds that it will take to get to the next location
    [DataMember(Name = 'distance_to_next_destination', EmitDefaultValue = false)]
    property double? DistanceToNextDestination { get; set; }


    //estimated time window start based on the optimization engine, after all the sequencing has been completed
    [DataMember(Name = 'generated_time_window_start', EmitDefaultValue = false)]
    property int? GeneratedTimeEindowStart { get; set; }

    //estimated time window end based on the optimization engine, after all the sequencing has been completed
    [DataMember(Name = 'generated_time_window_end', EmitDefaultValue = false)]
    property int? GeneratedTimeWindowEnd { get; set; }

    //the unique socket channel name which should be used to get real time alerts
    [DataMember(Name = 'channel_name', EmitDefaultValue = false)]
    property string channel_name { get; set; }

    [DataMember(Name = 'notes', EmitDefaultValue = false)]
    property AddressNote[] Notes { get; set; }

    //if present, the priority will sequence addresses in all the optimal routes so that
    //higher priority addresses are general at the beginning of the route sequence
    //1 is the highest priority, 100000 is the lowest
    [DataMember(Name = 'priority', EmitDefaultValue = false)]
    property int? Priority { get; set; }

*)
  end;
implementation

{ TAddress }

uses JSONDictionaryInterceptorObjectUnit;

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
  FAlias := NullableString.Null;
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

  FCustomFields := NullableObject.Null;
end;

constructor TAddress.Create(AddressString: String; Latitude, Longitude: double;
  Time: NullableInteger; TimeWindowStart, TimeWindowEnd: integer);
begin
  Create(AddressString, Latitude, Longitude, Time);
  FTimeWindowStart := TimeWindowStart;
  FTimeWindowEnd := TimeWindowEnd;
end;

constructor TAddress.Create(AddressString: String; Latitude, Longitude: double;
  Time: NullableInteger; TimeWindowStart, TimeWindowEnd, TimeWindowStart2,
  TimeWindowEnd2: integer);
begin
  Create(AddressString, Latitude, Longitude, Time, TimeWindowStart, TimeWindowEnd);
  FTimeWindowStart2 := TimeWindowStart2;
  FTimeWindowEnd2 := TimeWindowEnd2;
end;

end.
