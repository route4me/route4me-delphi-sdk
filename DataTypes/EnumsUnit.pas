unit EnumsUnit;

interface

type
  TMemberType = (
    mtPrimaryAccount,
    mtSubAccountAdmin,
    mtSubAccountRegionalManager,
    mtSubAccountDispatcher,
    mtSubAccountPlanner,
    mtSubAccountDriver,
    mtSubAccountAnalyst,
    mtSubAccountVendor,
    mtSubAccountCustomerService,
    mtUnknown
  );

  TCompassDirection = (
    cdNorth,
    cdSouth,
    cdWest,
    cdEast,
    cdNorthWest,
    cdNorthEast,
    cdSouthWest,
    cdSouthEast,
    cdUndefined
  );

  TDirectionEnum = (
    dHead,
    dGoStraight,
    dTurnLeft,
    dTurnRight,
    dTurnSlightLeft,
    dTurnSlightRight,
    dTurnSharpLeft,
    dTurnSharpRight,
    dRoundaboutLeft,
    dRoundaboutRight,
    dUturnLeft,
    dUturnRight,
    dRampLeft,
    dRampRight,
    dForkLeft,
    dForkRight,
    dKeepLeft,
    dKeepRight,
    dFerry,
    dFerryTrain,
    dMerge,
    dReachedYourDestination,
    dUnknown
  );

  TRoutePathOutput = (
    rpoPoints,
    rpoNone,
    rpoUndefined);

  TAddressStopType = (
    astPickup,
    astDelivery,
    astBreak,
    astMeetup,
    astUnknown
  );

  TFormatEnum = (
    Csv,
    Serialized,
    Xml,
    UndefinedFormat
  );

  TOptimizationParametersFormat = (
    opJson,
    opXml,
    opSerialized,
    opFriendly,
    opCsv,
    opUndefined
  );

  /// <summary>
  /// Territory type (circle, rectangle, polygon)
  /// </summary>
  TTerritoryType = (
    ttCircle,
    ttPoly,
    ttRect,
    ttUndefined
  );

  TStatusUpdateType = (
    Pickup,
    DropOff,
    NoAnswer,
    NotFound,
    NotPaid,
    Paid,
    WrongDelivery,
    WrongAddressRecipient,
    NotPresent,
    PartsMissing,
    ServiceRendered,
    FollowUp,
    LeftInformation,
    SpokeWithDecisionMaker,
    SpokeWithDecisionInfluencer,
    CompetitiveAccount,
    ScheduledFollowUpMeeting,
    ScheduledLunch,
    ScheduledProductDemo,
    ScheduledClinicalDemo,
    NoOpportunity
  );

  //an optimization problem can be at one state at any given time
  //every state change invokes a socket notification to the associated member id
  //every state change invokes a callback webhook event invocation if it was provided during the initial optimization
  TOptimizationState = (
    Initial             = 1,
    MatrixProcessing    = 2,
    Optimizing          = 3,
    Optimized           = 4,
    Error               = 5,
    ComputingDirections = 6
  );

  HttpMethodType = (
    Get,
    Put,
    Post,
    Delete
  );

  TAlgorithmType =
  (
    TSP        = 1, //single depot, single driver route
    VRP        = 2, //single depot, multiple driver, no constraints, no time windows, no capacities
    CVRP_TW_SD = 3, //single depot, multiple driver, capacitated, time windows
    CVRP_TW_MD = 4, //multiple depot, multiple driver, capacitated, time windows
    TSP_TW     = 5, //single depot, single driver, time windows
    TSP_TW_CR  = 6, //single depot, single driver, time windows, continuous optimization (minimal location shifting)
    BBCVRP     = 7, //shifts addresses from one route to another over time on a recurring schedule
    NoneAlgorithmType = 8
  );

  TMetric =
  (
    Euclidean = 1, //measures point to point distance as a straight line
    Manhattan = 2, //measures point to point distance as taxicab geometry line
    Geodesic  = 3, //measures point to point distance approximating curvature of the earth
    Matrix    = 4, //measures point to point distance by traversing the actual road network
    Exact_2D  = 5,  //measures point to point distance using 2d rectilinear distance
    UndefinedMetric = 6
  );

  TDeviceType = (Web, IPhone, IPad, AndroidPhone, AndroidTablet, UnknownDevice);
  TDistanceUnit = (MI, KM, Undefinded);
  TOptimize = (Distance, Time, TimeWithTraffic, NoneOptimize);
  TTravelMode = (Driving, Walking, Trucking, UnknownMode);
  TAvoid = (Highways, Tolls, MinimizeHighways, MinimizeTolls, Empty);
  TUploadType = (DriverImg, VehicleImg, AddressImg, CsvFile, XlsFile, AnyFile, UnknownUploadType);

var
  TDeviceTypeDescription: array[TDeviceType] of String = ('web', 'iphone', 'ipad', 'android_phone', 'android_tablet', 'UnknownDevice');
  TDistanceUnitDescription: array[TDistanceUnit] of String = ('mi', 'km', 'undefinded');
  TOptimizeDescription: array[TOptimize] of String = ('Distance', 'Time', 'timeWithTraffic', 'None');
  TTravelModeDescription: array[TTravelMode] of String = ('Driving', 'Walking', 'Trucking', 'UnknownMode');
  TOptimizationDescription: array[TOptimizationState] of String =
    ('Initial', 'MatrixProcessing', 'Optimizing', 'Optimized', 'Error', 'ComputingDirections');
  TUploadTypeDescription: array[TUploadType] of String = ('DRIVER_IMG', 'VEHICLE_IMG', 'ADDRESS_IMG', 'CSV_FILE', 'XLS_FILE', 'ANY_FILE', 'UnknownUploadType');
  TStatusUpdateTypeDescription: array[TStatusUpdateType] of String =
    ('pickup', 'dropoff', 'noanswer', 'notfound', 'notpaid', 'paid', 'wrongdelivery',
    'wrongaddressrecipient', 'notpresent', 'parts_missing', 'service_rendered',
    'follow_up', 'left_information', 'spoke_with_decision_maker', 'spoke_with_decision_influencer',
    'competitive_account', 'scheduled_follow_up_meeting', 'scheduled_lunch',
    'scheduled_product_demo', 'scheduled_clinical_demo', 'no_opportunity');
  TTerritoryTypeDescription: array[TTerritoryType] of String = ('circle', 'poly', 'rect', 'Undefined');
  TFormatDescription: array[TFormatEnum] of String = ('csv', 'serialized', 'xml', 'UndefinedFormat');
  TAddressStopTypeDescription: array[TAddressStopType] of String = ('PICKUP', 'DELIVERY', 'BREAK', 'MEETUP', 'Unknown');
  TCompassDirectionDescription: array[TCompassDirection] of String = ('N', 'S', 'W', 'E', 'NW', 'NE', 'SW', 'SE', 'Undefined');
  TOptimizationParametersFormatDescription: array[TOptimizationParametersFormat] of String =
    ('json', 'xml', 'serialized', 'friendly', 'csv', 'undefined');
  TAvoidDescription: array[TAvoid] of String = ('Highways', 'Tolls', 'minimizeHighways', 'minimizeTolls', '');
  TRoutePathOutputDescription: array[TRoutePathOutput] of String = ('Points', 'None', 'Undefined');
  TDirectionDescription: array[TDirectionEnum] of String = (
    'Head', 'Go Straight', 'Turn Left', 'Turn Right', 'Turn Slight Left',
    'Turn Slight Right', 'Turn Sharp Left', 'Turn Sharp Right', 'Roundabout Left',
    'Roundabout Right', 'Uturn Left', 'Uturn Right', 'Ramp Left', 'Ramp Right',
    'Fork Left', 'Fork Right', 'Keep Left', 'Keep Right', 'Ferry', 'Ferry Train',
    'Merge', 'Reached Your Destination', 'Unknown');
  TManeuverTypeDescription: array[TDirectionEnum] of String = (
    'HEAD', 'GO_STRAIGHT', 'TURN_LEFT', 'TURN_RIGHT', 'TURN_SLIGHT_LEFT',
    'TURN_SLIGHT_RIGHT', 'TURN_SHARP_LEFT', 'TURN_SHARP_RIGHT', 'ROUNDABOUT_LEFT',
    'ROUNDABOUT_RIGHT', 'UTURN_LEFT', 'UTURN_RIGHT', 'RAMP_LEFT', 'RAMP_RIGHT',
    'FORK_LEFT', 'FORK_RIGHT', 'KEEP_LEFT', 'KEEP_RIGHT', 'FERRY', 'FERRY_TRAIN',
    'MERGE', 'REACHED_YOUR_DESTINATION', 'Unknown');
  TMemberTypeDescription: array[TMemberType] of String = (
    'PRIMARY_ACCOUNT', 'SUB_ACCOUNT_ADMIN', 'SUB_ACCOUNT_REGIONAL_MANAGER',
    'SUB_ACCOUNT_DISPATCHER', 'SUB_ACCOUNT_PLANNER', 'SUB_ACCOUNT_DRIVER',
    'SUB_ACCOUNT_ANALYST', 'SUB_ACCOUNT_VENDOR', 'SUB_ACCOUNT_CUSTOMER_SERVICE', 'Unknown');

implementation

end.
