unit RouteParametersUnit;

interface

uses
  REST.Json.Types,
  JSONNullableAttributeUnit,
  EnumsUnit, NullableBasicTypesUnit;

type
  TRouteParameters = class
  private
    [JSONName('is_upload')]
    [JSONNullable]
    FIsUpload: NullableString;

    [JSONName('rt')]
    [JSONNullable]
    FRT: NullableBoolean;

    [JSONName('disable_optimization')]
    [JSONNullable]
    FDisableOptimization: NullableBoolean;

    [JSONName('route_name')]
    [JSONNullable]
    FRouteName: NullableString;

    [JSONName('algorithm_type')]
    [JSONNullable]
    FAlgorithmType: NullableInteger;

    [JSONName('store_route')]
    [JSONNullable]
    FStoreRoute: NullableBoolean;

    [JSONName('route_date')]
    [JSONNullable]
    FRouteDate: NullableInt64;

    [JSONName('route_time')]
    [JSONNullable]
    FRouteTime: NullableInteger;

    [JSONName('optimize')]
    [JSONNullable]
    FOptimize: NullableString;

    [JSONName('distance_unit')]
    [JSONNullable]
    FDistanceUnit: NullableString;

    [JSONName('device_type')]
    [JSONNullable]
    FDeviceType: NullableString;

    [JSONName('route_max_duration')]
    [JSONNullable]
    FRouteMaxDuration: NullableInteger;

    [JSONName('vehicle_capacity')]
    [JSONNullable]
    FVehicleCapacity: NullableString;

    [JSONName('vehicle_max_distance_mi')]
    [JSONNullable]
    FVehicleMaxDistanceMI: NullableString;

    [JSONName('travel_mode')]
    [JSONNullable]
    FTravelMode: NullableString;

    //1 = ROUTE4ME_METRIC_EUCLIDEAN (use euclidean distance when computing point to point distance)
    //2 = ROUTE4ME_METRIC_MANHATTAN (use manhattan distance (taxicab geometry) when computing point to point distance)
    //3 = ROUTE4ME_METRIC_GEODESIC (use geodesic distance when computing point to point distance)
    //#4 is the default and suggested metric
    //4 = ROUTE4ME_METRIC_MATRIX (use road network driving distance when computing point to point distance)
    //5 = ROUTE4ME_METRIC_EXACT_2D (use exact rectilinear distance)
    [JSONName('metric')]
    [JSONNullable]
    FMetric: NullableInteger;

    [JSONName('parts')]
    [JSONNullable]
    FParts: NullableInteger;
  public
    constructor Create;

    //let the R4M api know if this sdk request is coming from a file upload within your environment (for analytics)
//    [DataMember(Name = 'is_upload', EmitDefaultValue = false)]
    property IsUpload: NullableString read FIsUpload write FIsUpload;

    //the tour type of this route. rt is short for round trip, the optimization engine changes its behavior for round trip routes
//    [DataMember(Name = 'rt', EmitDefaultValue = false)]
    property RT: NullableBoolean read FRT write FRT; // nullable

    //by disabling optimization, the route optimization engine will not resequence the stops in your
//    [DataMember(Name = 'disable_optimization', EmitDefaultValue = false)]
    property DisableOptimization: NullableBoolean read FDisableOptimization write FDisableOptimization; // nullable

    //the name of this route. this route name will be accessible in the search API, and also will be displayed on the mobile device of a user
//    [DataMember(Name = 'route_name', EmitDefaultValue = false)]
    property RouteName: NullableString read FRouteName write FRouteName;

    //the type of algorithm to use when optimizing the route
//    [DataMember(Name = 'algorithm_type', EmitDefaultValue = false)]
    property AlgorithmType: NullableInteger read FAlgorithmType write FAlgorithmType;

    // deprecated
    //all routes are stored by default at this time
//    [DataMember(Name = 'store_route', EmitDefaultValue = false)]
    property StoreRoute: NullableBoolean read FStoreRoute write FStoreRoute;  // nullable

    //the route start date in UTC, unix timestamp seconds.
    //used to show users when the route will begin, also used for reporting and analytics
//    [DataMember(Name = 'route_date', EmitDefaultValue = false)]
    property RouteDate: NullableInt64 read FRouteDate write FRouteDate;  // nullable

    //offset in seconds relative to the route start date (i.e. 9AM would be 60 * 60 * 9)
//    [DataMember(Name = 'route_time', EmitDefaultValue = false)]
    property RouteTime: NullableInteger read FRouteTime write FRouteTime;

//    [DataMember(Name = 'optimize', EmitDefaultValue = false)]
    property Optimize: NullableString read FOptimize write FOptimize;

    //km or mi, the route4me api will convert all measurements into these units
//    [DataMember(Name = 'distance_unit', EmitDefaultValue = false)]
    property DistanceUnit: NullableString read FDistanceUnit write FDistanceUnit;

    //the type of device making this request
    //ENUM('web', 'iphone', 'ipad', 'android_phone', 'android_tablet')
//    [DataMember(Name = 'device_type', EmitDefaultValue = false)]
    property DeviceType: NullableString read FDeviceType write FDeviceType;

    //when using a multiple driver algorithm, this is the maximum permissible duration of a generated route
    //the optimization system will automatically create more routes when the route_max_duration is exceeded for a route
    //however it will create an 'unrouted' list of addresses if the maximum number of drivers is exceeded
//    [DataMember(Name = 'route_max_duration', EmitDefaultValue = false)]
    property RouteMaxDuration: NullableInteger read FRouteMaxDuration write FRouteMaxDuration; // nullable

//    [DataMember(Name = 'vehicle_capacity', EmitDefaultValue = false)]
    property VehicleCapacity: NullableString read FVehicleCapacity write FVehicleCapacity;

//    [DataMember(Name = 'vehicle_max_distance_mi', EmitDefaultValue = false)]
    property VehicleMaxDistanceMI: NullableString read FVehicleMaxDistanceMI write FVehicleMaxDistanceMI;

//    [DataMember(Name = 'travel_mode', EmitDefaultValue = false)]
    property TravelMode: NullableString read FTravelMode write FTravelMode;

    property Metric: NullableInteger read FMetric write FMetric;

    property Parts: NullableInteger read FParts write FParts;

(*    //deprecated
    //specify if the route can be viewed by unauthenticated users
    [DataMember(Name = 'shared_propertyly', EmitDefaultValue = false)]
    property string Sharedpropertyly { get; set; }

    //when the tour type is not round trip (rt = false), enable lock last so that the final destination is fixed
    //example: driver leaves a depot, but must always arrive at home ( or a specific gas station) at the end of the route
    [DataMember(Name = 'lock_last', EmitDefaultValue = false)]
    property bool? LockLast { get; set; }

    [DataMember(Name = 'avoid', EmitDefaultValue = false)]
    property string Avoid { get; set; }

    [DataMember(Name = 'vehicle_id', EmitDefaultValue = false)]
    property string VehicleId { get; set; }

    //deprecated, all new routes should be assigned to a member_id
    [DataMember(Name = 'driver_id', EmitDefaultValue = false)]
    property string DriverId { get; set; }

    //the latitude of the device making this sdk request
    [DataMember(Name = 'dev_lat', EmitDefaultValue = false)]
    property double? DevLatitude { get; set; }

    //the longitude of the device making this sdk request
    [DataMember(Name = 'dev_lng', EmitDefaultValue = false)]
    property double? DevLongitude { get; set; }

    //the email address to notify upon completion of an optimization request
    [DataMember(Name = 'route_email', EmitDefaultValue = false)]
    property string RouteEmail { get; set; }

    //type of route being created: ENUM(api,null)
    [DataMember(Name = 'route_type', EmitDefaultValue = false)]
    property string RouteType { get; set; }

    //in order for users in your organization to have routes assigned to them,
    //you must provide their member id within the route4me system
    //a list of member ids can be retrieved with view_users api method
    [DataMember(Name = 'member_id', EmitDefaultValue = false)]
    property string MemberId { get; set; }


    //specify the ip address of the remote user making this optimization request
    [DataMember(Name = 'ip', EmitDefaultValue = false)]
    property string Ip { get; set; }


    //the method to use when compute the distance between the points in a route
    //1 = DEFAULT (R4M PROPRIETARY ROUTING)
    //2 = DEPRECRATED
    //3 = R4M TRAFFIC ENGINE
    //4 = DEPRECATED
    //5 = DEPRECATED
    //6 = TRUCKING
    [DataMember(Name = 'dm', EmitDefaultValue = false)]
    property int? DM { get; set; }

    //directions method
    //1 = DEFAULT (R4M PROPRIETARY INTERNAL NAVIGATION SYSTEM)
    //2 = DEPRECATED
    //3 = TRUCKING
    //4 = DEPRECATED
    [DataMember(Name = 'dirm', EmitDefaultValue = false)]
    property int? Dirm { get; set; }

    //deprecated
    [DataMember(Name = 'device_id', EmitDefaultValue = false)]
    property object DeviceID { get; set; }

    //for routes that have trucking directions enabled, directions generated
    //will ensure compliance so that road directions generated do not take the vehicle
    //where trailers are prohibited
    [DataMember(Name = 'has_trailer', EmitDefaultValue = false)]
    property bool? HasTrailer { get; set; }

    //for routes that have trucking directions enabled, directions generated
    //will ensure compliance so that road directions generated do not take the vehicle
    //on roads where the weight of the vehicle in tons exceeds this value
    [DataMember(Name = 'trailer_weight_t', EmitDefaultValue = false)]
    property double? TrailerWeightT { get; set; }


    [DataMember(Name = 'limited_weight_t', EmitDefaultValue = false)]
    property double? LimitedWeightT { get; set; }

    //for routes that have trucking directions enabled, directions generated
    //will ensure compliance so that road directions generated do not take the vehicle
    //where the weight per axle in tons exceeds this value
    [DataMember(Name = 'weight_per_axle_t', EmitDefaultValue = false)]
    property double? WeightPerAxleT { get; set; }

    //for routes that have trucking directions enabled, directions generated
    //will ensure compliance of this maximum height of truck when generating road network driving directions
    [DataMember(Name = 'truck_height_meters', EmitDefaultValue = false)]
    property int? TruckHeightMeters { get; set; }

    //for routes that have trucking directions enabled, directions generated
    //will ensure compliance of this width of the truck when generating road network driving directions
    [DataMember(Name = 'truck_width_meters', EmitDefaultValue = false)]
    property int? TruckWidthMeters { get; set; }

    //for routes that have trucking directions enabled, directions generated
    //will ensure compliance of this length of the truck when generating road network driving directions
    [DataMember(Name = 'truck_length_meters', EmitDefaultValue = false)]
    property int? TruckLengthMeters { get; set; }


    //the minimum number of stops permitted per created subroute
    [DataMember(Name = 'min_tour_size', EmitDefaultValue = false)]
    property int? MinTourSize { get; set; }

    //the maximum number of stops permitted per created subroute
    [DataMember(Name = 'max_tour_size', EmitDefaultValue = false)]
    property int? MaxTourSize { get; set; }

    //there are 3 types of optimization qualities that are optimizations goals
    //1 - Generate Optimized Routes As Quickly as Possible
    //2 - Generate Routes That Look Better On A Map
    //3 - Generate The Shortest And Quickest Possible Routes

    [DataMember(Name = 'optimization_quality', EmitDefaultValue = false)]
    property int? OptimizationQuality { get; set; }*)
  end;
implementation

{ TRouteParameters }

constructor TRouteParameters.Create;
begin
    FIsUpload := NullableString.Null;
    FRT := NullableBoolean.Null;
    FDisableOptimization := NullableBoolean.Null;
    FRouteName := NullableString.Null;
    FAlgorithmType := NullableInteger.Null;
    FStoreRoute := NullableBoolean.Null;
    FRouteDate := NullableInt64.Null;
    FRouteTime := NullableInteger.Null;
    FOptimize := NullableString.Null;
    FDistanceUnit := NullableString.Null;
    FDeviceType := NullableString.Null;
    FRouteMaxDuration := NullableInteger.Null;
    FVehicleCapacity := NullableString.Null;
    FVehicleMaxDistanceMI := NullableString.Null;
    FTravelMode := NullableString.Null;
    FMetric := NullableInteger.Null;
    FParts := NullableInteger.Null;
end;

end.
