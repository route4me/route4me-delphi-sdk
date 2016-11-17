unit RouteParametersUnit;

interface

uses
  REST.Json.Types,
  JSONNullableAttributeUnit,
  EnumsUnit, NullableBasicTypesUnit;

type
  /// <summary>
  ///  Route Parameters
  /// </summary>
  /// <remarks>
  ///  https://github.com/route4me/json-schemas/blob/master/RouteParameters.dtd
  /// </remarks>
  TRouteParameters = class
  private
    [JSONName('is_upload')]
    [Nullable]
    FIsUpload: NullableBoolean;

    [JSONName('rt')]
    [Nullable]
    FRT: NullableBoolean;

    [JSONName('disable_optimization')]
    [Nullable]
    FDisableOptimization: NullableBoolean;

    [JSONName('route_name')]
    [Nullable]
    FRouteName: NullableString;

    [JSONName('algorithm_type')]
    [Nullable]
    FAlgorithmType: NullableInteger;

    [JSONName('store_route')]
    [Nullable]
    FStoreRoute: NullableBoolean;

    [JSONName('route_date')]
    [Nullable]
    FRouteDate: NullableInteger;

    [JSONName('route_time')]
    [Nullable]
    FRouteTime: NullableInteger;

    [JSONName('optimize')]
    [Nullable]
    FOptimize: NullableString;

    [JSONName('distance_unit')]
    [Nullable]
    FDistanceUnit: NullableString;

    [JSONName('device_type')]
    [Nullable]
    FDeviceType: NullableString;

    [JSONName('route_max_duration')]
    [Nullable]
    FRouteMaxDuration: NullableInteger;

    [JSONName('vehicle_capacity')]
    [Nullable]
    FVehicleCapacity: NullableString;

    [JSONName('vehicle_max_distance_mi')]
    [Nullable]
    FVehicleMaxDistanceMI: NullableString;

    [JSONName('travel_mode')]
    [Nullable]
    FTravelMode: NullableString;

    [JSONName('metric')]
    [Nullable]
    FMetric: NullableInteger;

    [JSONName('parts')]
    [Nullable]
    FParts: NullableInteger;

    [JSONName('dev_lng')]
    [Nullable]
    FDevLongitude: NullableDouble;

    [JSONName('route_email')]
    [Nullable]
    FRouteEmail: NullableString;

    [JSONName('dirm')]
    [Nullable]
    FDirm: NullableInteger;

    [JSONName('dm')]
    [Nullable]
    FDM: NullableInteger;

    [JSONName('member_id')]
    [Nullable]
    FMemberId: NullableString;

    [JSONName('driver_id')]
    [Nullable]
    FDriverId: NullableString;

    [JSONName('vehicle_id')]
    [Nullable]
    FVehicleId: NullableString;

    [JSONName('routeType')]
    [Nullable]
    FRouteType: NullableString;

    [JSONName('dev_lat')]
    [Nullable]
    FDevLatitude: NullableDouble;

    [JSONName('ip')]
    [Nullable]
    FIp: NullableString;

    [JSONName('device_id')]
    [Nullable]
    FDeviceId: NullableString;

    [JSONName('lock_last')]
    [Nullable]
    FLockLast: NullableBoolean;

    [JSONName('avoid')]
    [Nullable]
    FAvoid: NullableString;

    [JSONName('truck_height_meters')]
    [Nullable]
    FTruckHeightMeters: NullableInteger;

    [JSONName('truck_length_meters')]
    [Nullable]
    FTruckLengthMeters: NullableInteger;

    [JSONName('has_trailer')]
    [Nullable]
    FHasTrailer: NullableBoolean;

    [JSONName('max_tour_size')]
    [Nullable]
    FMaxTourSize: NullableInteger;

    [JSONName('truck_width_meters')]
    [Nullable]
    FTruckWidthMeters: NullableInteger;

    [JSONName('min_tour_size')]
    [Nullable]
    FMinTourSize: NullableInteger;

    [JSONName('limited_weight_t')]
    [Nullable]
    FLimitedWeightT: NullableDouble;

    [JSONName('optimization_quality')]
    [Nullable]
    FOptimizationQuality: NullableInteger;

    [JSONName('trailer_weight_t')]
    [Nullable]
    FTrailerWeightT: NullableDouble;

    [JSONName('weight_per_axle_t')]
    [Nullable]
    FWeightPerAxleT: NullableDouble;

    function GetAlgorithmType: TAlgorithmType;
    procedure SetAlgorithmType(const Value: TAlgorithmType);
    function GetOptimize: TOptimize;
    procedure SetOptimize(const Value: TOptimize);
    function GetDistanceUnit: TDistanceUnit;
    procedure SetDistanceUnit(const Value: TDistanceUnit);
    function GetDeviceType: TDeviceType;
    procedure SetDeviceType(const Value: TDeviceType);
    function GetTravelMode: TTravelMode;
    procedure SetTravelMode(const Value: TTravelMode);
    function GetMetric: TMetric;
    procedure SetMetric(const Value: TMetric);
    function GetAvoid: TAvoid;
    procedure SetAvoid(const Value: TAvoid);

  public
    /// <remarks>
    ///  Constructor with 0-arguments must be and be public.
    ///  For JSON-deserialization.
    /// </remarks>
    constructor Create;

    function Equals(Obj: TObject): Boolean; override;

    /// <summary>
    ///  Let the R4M api know if this sdk request is coming from a file upload within your environment (for analytics)
    /// </summary>
    property IsUpload: NullableBoolean read FIsUpload write FIsUpload;

    /// <summary>
    ///  The tour type of this route. rt is short for round trip, the optimization engine changes its behavior for round trip routes
    /// </summary>
    property RT: NullableBoolean read FRT write FRT;

    /// <summary>
    ///  By disabling optimization, the route optimization engine will not resequence the stops in your
    /// </summary>
    property DisableOptimization: NullableBoolean read FDisableOptimization write FDisableOptimization;

    /// <summary>
    ///  The name of this route. this route name will be accessible in the search API, and also will be displayed on the mobile device of a user
    /// </summary>
    property RouteName: NullableString read FRouteName write FRouteName;

    /// <summary>
    ///  The algorithm to be used
    /// </summary>
    property AlgorithmType: TAlgorithmType read GetAlgorithmType write SetAlgorithmType;

    /// <summary>
    ///  Always true
    /// </summary>
    property StoreRoute: NullableBoolean read FStoreRoute write FStoreRoute;

    /// <summary>
    ///  The route start date in UTC, unix timestamp seconds. Used to show users when the route will begin, also used for reporting and analytics
    /// </summary>
    property RouteDate: NullableInteger read FRouteDate write FRouteDate;

    /// <summary>
    ///  Time when the route starts (relative to route_date) (Seconds). UTC timezone as well
    /// </summary>
    property RouteTime: NullableInteger read FRouteTime write FRouteTime;

    /// <summary>
    ///  The driving directions will be generated biased for this selection. This has no impact on route sequencing
    /// </summary>
    property Optimize: TOptimize read GetOptimize write SetOptimize;

    /// <summary>
    ///  The distance measurement unit for the route
    /// </summary>
    property DistanceUnit: TDistanceUnit read GetDistanceUnit write SetDistanceUnit;

    /// <summary>
    ///  The type of the device that is creating this route
    /// </summary>
    property DeviceType: TDeviceType read GetDeviceType write SetDeviceType;

    /// <summary>
    ///  How many seconds a route can last at most. Default is 24 hours = 86400 seconds
    /// </summary>
    property RouteMaxDuration: NullableInteger read FRouteMaxDuration write FRouteMaxDuration; // nullable

    /// <summary>
    ///  How much cargo can the vehicle carry (units, e.g. cubic meters)
    /// </summary>
    property VehicleCapacity: NullableString read FVehicleCapacity write FVehicleCapacity;

    /// <summary>
    ///  Max distance for a single vehicle in this route (always in miles)
    /// </summary>
    property VehicleMaxDistanceMI: NullableString read FVehicleMaxDistanceMI write FVehicleMaxDistanceMI;

    /// <summary>
    ///  The mode of travel that the directions should be optimized for
    /// </summary>
    property TravelMode: TTravelMode read GetTravelMode write SetTravelMode;

    /// <summary>
    ///  Integer [1, 2, 3, 4, 5]
    /// 1 = ROUTE4ME_METRIC_EUCLIDEAN (use euclidean distance when computing point to point distance)
    /// 2 = ROUTE4ME_METRIC_MANHATTAN (use manhattan distance (taxicab geometry) when computing point to point distance)
    /// 3 = ROUTE4ME_METRIC_GEODESIC (use geodesic distance when computing point to point distance)
    /// #4 is the default and suggested metric
    /// 4 = ROUTE4ME_METRIC_MATRIX (use road network driving distance when computing point to point distance)
    /// 5 = ROUTE4ME_METRIC_EXACT_2D (use exact rectilinear distance)
    /// </summary>
    property Metric: TMetric read GetMetric write SetMetric;

    /// <summary>
    ///  Legacy feature which permits a user to request an example number of optimized routes
    /// </summary>
    property Parts: NullableInteger read FParts write FParts;

    /// <summary>
    ///  A flag to indicate if the last stop in the route should be fixed
    /// </summary>
    property LockLast: NullableBoolean read FLockLast write FLockLast;

    /// <summary>
    ///  Options which let the user choose which road obstacles to avoid. This has no impact on route sequencing
    /// </summary>
    property Avoid: TAvoid read GetAvoid write SetAvoid;

    /// <summary>
    ///  The unique internal id of a vehicle
    /// </summary>
    property VehicleId: NullableString read FVehicleId write FVehicleId;

    /// <summary>
    ///  The unique internal id of a driver
    /// </summary>
    property DriverId: NullableString read FDriverId write FDriverId;

    /// <summary>
    ///  The latitude location of where a mobile device was located when it made a request to create the route
    /// </summary>
    property DevLatitude: NullableDouble read FDevLatitude write FDevLatitude;

    /// <summary>
    ///  The longitude location of where a mobile device was located when it made a request to create the route
    /// </summary>
    property DevLongitude: NullableDouble read FDevLongitude write FDevLongitude;

    /// <summary>
    ///  Addresses where this route was shared after completion
    /// </summary>
    property RouteEmail: NullableString read FRouteEmail write FRouteEmail;

    /// <summary>
    ///  Type of route being created: ENUM(api,null)
    /// </summary>
    property RouteType: NullableString read FRouteType write FRouteType;

    /// <summary>
    ///  User ID who is assigned to the route
    /// </summary>
    property MemberId: NullableString read FMemberId write FMemberId;

    /// <summary>
    ///  IP Address in decimal form of user who created the route
    /// </summary>
    property Ip: NullableString read FIp write FIp;

    /// <summary>
    ///  Undocumented/not publicly shown
    /// </summary>
    //the method to use when compute the distance between the points in a route
    //1 = DEFAULT (R4M PROPRIETARY ROUTING)
    //2 = DEPRECRATED
    //3 = R4M TRAFFIC ENGINE
    //4 = DEPRECATED
    //5 = DEPRECATED
    //6 = TRUCKING
    property DM: NullableInteger read FDM write FDM;

    /// <summary>
    ///  Undocumented/not publicly shown
    /// </summary>
    //directions method
    //1 = DEFAULT (R4M PROPRIETARY INTERNAL NAVIGATION SYSTEM)
    //2 = DEPRECATED
    //3 = TRUCKING
    //4 = DEPRECATED
    property Dirm: NullableInteger read FDirm write FDirm;

    /// <summary>
    ///  32 Character MD5 String ID of the device that was used to plan this route
    /// </summary>
    property DeviceId: NullableString read FDeviceId write FDeviceId;

    /// <summary>
    ///  if True vehicle has trailer
    /// </summary>
    property HasTrailer: NullableBoolean read FHasTrailer write FHasTrailer;

    /// <summary>
    ///  If has_trailer = true, specifies the weight of the trailer (required)
    /// </summary>
    property TrailerWeightT: NullableDouble read FTrailerWeightT write FTrailerWeightT;

    /// <summary>
    ///  If travel_mode = 'Trucking', specifies the truck weight (required)
    /// </summary>
    property LimitedWeightT: NullableDouble read FLimitedWeightT write FLimitedWeightT;

    /// <summary>
    ///  If travel_mode = 'Trucking', specifies the weight per axle (required)
    /// </summary>
    property WeightPerAxleT: NullableDouble read FWeightPerAxleT write FWeightPerAxleT;

    /// <summary>
    ///  If travel_mode = 'Trucking', specifies the truck height (required)
    /// </summary>
    property TruckHeightMeters: NullableInteger read FTruckHeightMeters write FTruckHeightMeters;

    /// <summary>
    ///  If travel_mode = 'Trucking', specifies the truck width (required)
    /// </summary>
    property TruckWidthMeters: NullableInteger read FTruckWidthMeters write FTruckWidthMeters;

    /// <summary>
    ///  If travel_mode = 'Trucking', specifies the truck length (required)
    /// </summary>
    property TruckLengthMeters: NullableInteger read FTruckLengthMeters write FTruckLengthMeters;

    /// <summary>
    ///  Must be > 0; the minimum number of stops allowed in a subtour. null means there is no minimum
    /// </summary>
    //the minimum number of stops permitted per created subroute
    property MinTourSize: NullableInteger read FMinTourSize write FMinTourSize;

    /// <summary>
    ///  Must be > 0; the maximum number of stops allowed in a subtour. null means there is no maximum
    /// </summary>
    property MaxTourSize: NullableInteger read FMaxTourSize write FMaxTourSize;

    /// <summary>
    ///  there are 3 types of optimization qualities that are optimizations goals
    ///  1 - Generate Optimized Routes As Quickly as Possible
    ///  2 - Generate Routes That Look Better On A Map
    ///  3 - Generate The Shortest And Quickest Possible Routes
    /// </summary>
    property OptimizationQuality: NullableInteger read FOptimizationQuality write FOptimizationQuality;
  end;

implementation

{ TRouteParameters }

constructor TRouteParameters.Create;
begin
    FIsUpload := NullableBoolean.Null;
    FRT := NullableBoolean.Null;
    FDisableOptimization := NullableBoolean.Null;
    FRouteName := NullableString.Null;
    FAlgorithmType := NullableInteger.Null;
    FStoreRoute := NullableBoolean.Null;
    FRouteDate := NullableInteger.Null;
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
    FDevLongitude := NullableDouble.Null;
    FRouteEmail := NullableString.Null;
    FDirm := NullableInteger.Null;
    FDM := NullableInteger.Null;
    FMemberId := NullableString.Null;
    FDriverId := NullableString.Null;
    FVehicleId := NullableString.Null;
    FRouteType := NullableString.Null;
    FDevLatitude := NullableDouble.Null;
    FIp := NullableString.Null;
    FDeviceId := NullableString.Null;
    FLockLast := NullableBoolean.Null;
    FAvoid := NullableString.Null;
    FTruckHeightMeters := NullableInteger.Null;
    FTruckLengthMeters := NullableInteger.Null;
    FHasTrailer := NullableBoolean.Null;
    FMaxTourSize := NullableInteger.Null;
    FTruckWidthMeters := NullableInteger.Null;
    FMinTourSize := NullableInteger.Null;
    FLimitedWeightT := NullableDouble.Null;
    FOptimizationQuality := NullableInteger.Null;
    FTrailerWeightT := NullableDouble.Null;
    FWeightPerAxleT := NullableDouble.Null;
end;

function TRouteParameters.Equals(Obj: TObject): Boolean;
var
  Other: TRouteParameters;
begin
  Result := False;

  if not (Obj is TRouteParameters) then
    Exit;

  Other := TRouteParameters(Obj);

  Result :=
    (FIsUpload = Other.FIsUpload) and
    (FRT = Other.FRT) and
    (FDisableOptimization = Other.FDisableOptimization) and
    (FRouteName = Other.FRouteName) and
    (FAlgorithmType = Other.FAlgorithmType) and
    (FStoreRoute = Other.FStoreRoute) and
    (FRouteDate = Other.FRouteDate) and
    (FRouteTime = Other.FRouteTime) and
    (FOptimize = Other.FOptimize) and
    (FDistanceUnit = Other.FDistanceUnit) and
    (FDeviceType = Other.FDeviceType) and
    (FRouteMaxDuration = Other.FRouteMaxDuration) and
    (FVehicleCapacity = Other.FVehicleCapacity) and
    (FVehicleMaxDistanceMI = Other.FVehicleMaxDistanceMI) and
    (FTravelMode = Other.FTravelMode) and
    (FMetric = Other.FMetric) and
    (FParts = Other.FParts) and
    (FDevLatitude = Other.FDevLatitude) and
    (FDevLongitude = Other.FDevLongitude) and
    (FRouteEmail = Other.FRouteEmail) and
    (FDirm = Other.FDirm) and
    (FDM = Other.FDM) and
    (FMemberId = Other.FMemberId) and
    (FDriverId = Other.FDriverId) and
    (FVehicleId = Other.FVehicleId) and
    (FRouteType = Other.FRouteType) and
    (FIp = Other.FIp) and
    (FDeviceId = Other.FDeviceId) and
    (FLockLast = Other.FLockLast) and
    (FAvoid = Other.FAvoid) and
    (FTruckHeightMeters = Other.FTruckHeightMeters) and
    (FTruckLengthMeters = Other.FTruckLengthMeters) and
    (FHasTrailer = Other.FHasTrailer) and
    (FMaxTourSize = Other.FMaxTourSize) and
    (FTruckWidthMeters = Other.FTruckWidthMeters) and
    (FMinTourSize = Other.FMinTourSize) and
    (FLimitedWeightT = Other.FLimitedWeightT) and
    (FOptimizationQuality = Other.FOptimizationQuality) and
    (FTrailerWeightT = Other.FTrailerWeightT) and
    (FWeightPerAxleT = Other.FWeightPerAxleT);
end;

function TRouteParameters.GetAlgorithmType: TAlgorithmType;
begin
  if FAlgorithmType.IsNull then
    Result := TAlgorithmType.NoneAlgorithmType
  else
    Result := TAlgorithmType(FAlgorithmType.Value);
end;

function TRouteParameters.GetAvoid: TAvoid;
var
  Avoid: TAvoid;
begin
  Result := TAvoid.Empty;
  if FAvoid.IsNotNull then
    for Avoid := Low(TAvoid) to High(TAvoid) do
      if (FAvoid = TAvoidDescription[Avoid]) then
        Exit(Avoid);
end;

function TRouteParameters.GetDeviceType: TDeviceType;
var
  DeviceType: TDeviceType;
begin
  Result := TDeviceType.UnknownDevice;
  if FDeviceType.IsNotNull then
    for DeviceType := Low(TDeviceType) to High(TDeviceType) do
      if (FDeviceType = TDeviceTypeDescription[DeviceType]) then
        Exit(DeviceType);
end;

function TRouteParameters.GetDistanceUnit: TDistanceUnit;
var
  DistanceUnit: TDistanceUnit;
begin
  Result := TDistanceUnit.Undefinded;
  if FDistanceUnit.IsNotNull then
    for DistanceUnit := Low(TDistanceUnit) to High(TDistanceUnit) do
      if (FDistanceUnit = TDistanceUnitDescription[DistanceUnit]) then
        Exit(DistanceUnit);
end;

function TRouteParameters.GetMetric: TMetric;
begin
  if FMetric.IsNull then
    Result := TMetric.UndefinedMetric
  else
    Result := TMetric(FMetric.Value);
end;

function TRouteParameters.GetOptimize: TOptimize;
var
  Optimize: TOptimize;
begin
  Result := TOptimize.NoneOptimize;
  if FOptimize.IsNotNull then
    for Optimize := Low(TOptimize) to High(TOptimize) do
      if (FOptimize = TOptimizeDescription[Optimize]) then
        Exit(Optimize);
end;

function TRouteParameters.GetTravelMode: TTravelMode;
var
  TravelMode: TTravelMode;
begin
  Result := TTravelMode.UnknownMode;
  if FTravelMode.IsNotNull then
    for TravelMode := Low(TTravelMode) to High(TTravelMode) do
      if (FTravelMode = TTravelModeDescription[TravelMode]) then
        Exit(TravelMode);
end;

procedure TRouteParameters.SetAlgorithmType(const Value: TAlgorithmType);
begin
  FAlgorithmType := Integer(Value);
end;

procedure TRouteParameters.SetAvoid(const Value: TAvoid);
begin
  FAvoid := TAvoidDescription[Value];
end;

procedure TRouteParameters.SetDeviceType(const Value: TDeviceType);
begin
  FDeviceType := TDeviceTypeDescription[Value];
end;

procedure TRouteParameters.SetDistanceUnit(const Value: TDistanceUnit);
begin
  FDistanceUnit := TDistanceUnitDescription[Value];
end;

procedure TRouteParameters.SetMetric(const Value: TMetric);
begin
  FMetric := Integer(Value);
end;

procedure TRouteParameters.SetOptimize(const Value: TOptimize);
begin
  FOptimize := TOptimizeDescription[Value];
end;

procedure TRouteParameters.SetTravelMode(const Value: TTravelMode);
begin
  FTravelMode := TTravelModeDescription[Value];
end;

end.
