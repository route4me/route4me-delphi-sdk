unit EnumsUnit;

interface

type
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
    BBCVRP     = 7  //shifts addresses from one route to another over time on a recurring schedule
  );

  TMetric =
  (
    Euclidean = 1, //measures point to point distance as a straight line
    Manhattan = 2, //measures point to point distance as taxicab geometry line
    Geodesic  = 3, //measures point to point distance approximating curvature of the earth
    Matrix    = 4, //measures point to point distance by traversing the actual road network
    Exact_2D  = 5  //measures point to point distance using 2d rectilinear distance
  );

  TDeviceType = (Web, IPhone, IPad, AndroidPhone, AndroidTablet);
  TDistanceUnit = (MI, KM);
  TOptimize = (Distance, Time, TimeWithTraffic);
  TTravelMode = (Driving, Walking, Trucking);

var
  TDeviceTypeDescription: array[TDeviceType] of String = ('web', 'iphone', 'ipad', 'android_phone', 'android_tablet');
  TDistanceUnitDescription: array[TDistanceUnit] of String = ('mi', 'km');
  TOptimizeDescription: array[TOptimize] of String = ('Distance', 'Time', 'timeWithTraffic');
  TTravelModeDescription: array[TTravelMode] of String = ('Driving', 'Walking', 'Trucking');
  TOptimizationDescription: array[TOptimizationState] of String =
    ('Initial', 'MatrixProcessing', 'Optimizing', 'Optimized', 'Error', 'ComputingDirections');


implementation

end.
