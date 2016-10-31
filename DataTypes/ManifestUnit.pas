unit ManifestUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections,
  JSONNullableAttributeUnit,
  NullableBasicTypesUnit;

type
  /// <summary>
  /// The manifest contains values derived from other values
  /// </summary>
  TManifest = class
  private
    [JSONName('running_service_time')]
    [Nullable]
    FRunningServiceTime: NullableInteger;

    [JSONName('running_travel_time')]
    [Nullable]
    FRunningTravelTime: NullableInteger;

    [JSONName('running_wait_time')]
    [Nullable]
    FRunningWaitTime: NullableInteger;

    [JSONName('wait_time_to_next_destination')]
    [Nullable]
    FWaitTimeToNextDestination: NullableInteger;

    [JSONName('running_distance')]
    [Nullable]
    FRunningDistance: NullableDouble;

    [JSONName('fuel_from_start')]
    [Nullable]
    FFuelFromStart: NullableDouble;

    [JSONName('fuel_cost_from_start')]
    [Nullable]
    FFuelCostFromStart: NullableDouble;

    [JSONName('projected_arrival_time_ts')]
    [Nullable]
    FProjectedArrivalTimeTs: NullableInteger;

    [JSONName('projected_departure_time_ts')]
    [Nullable]
    FProjectedDepartureTimeTs: NullableInteger;

    [JSONName('actual_arrival_time_ts')]
    [Nullable]
    FActualArrivalTimeTs: NullableInteger;

    [JSONName('actual_departure_time_ts')]
    [Nullable]
    FActualDepartureTimeTs: NullableInteger;

    [JSONName('estimated_arrival_time_ts')]
    [Nullable]
    FEstimatedArrivalTimeTs: NullableInteger;

    [JSONName('estimated_departure_time_ts')]
    [Nullable]
    FEstimatedDepartureTimeTs: NullableInteger;

    [JSONName('time_impact')]
    [Nullable]
    FTimeImpact: NullableInteger;
  public
    /// <remarks>
    ///  Constructor with 0-arguments must be and be public.
    ///  For JSON-deserialization.
    /// </remarks>
    constructor Create;

    function Equals(Obj: TObject): Boolean; override;

    /// <summary>
    ///  How much time is to be spent on service from the start in seconds
    /// </summary>
    property RunningServiceTime: NullableInteger read FRunningServiceTime write FRunningServiceTime;

    /// <summary>
    ///  How much time is spent driving from the start in seconds
    /// </summary>
    property RunningTravelTime: NullableInteger read FRunningTravelTime write FRunningTravelTime;

    /// <summary>
    ///  Running Wait Time
    /// </summary>
    property RunningWaitTime: NullableInteger read FRunningWaitTime write FRunningWaitTime;

    /// <summary>
    ///  Wait time to next destination
    /// </summary>
    property WaitTimeToNextDestination: NullableInteger read FWaitTimeToNextDestination write FWaitTimeToNextDestination;

    /// <summary>
    ///  Distance traversed before reaching this address
    /// </summary>
    property RunningDistance: NullableDouble read FRunningDistance write FRunningDistance;

    /// <summary>
    ///  Fuel From Start
    /// </summary>
    property FuelFromStart: NullableDouble read FFuelFromStart write FFuelFromStart;

    /// <summary>
    ///  Fuel Cost From Start
    /// </summary>
    property FuelCostFromStart: NullableDouble read FFuelCostFromStart write FFuelCostFromStart;

    /// <summary>
    ///  Projected arrival time UTC unixtime
    /// </summary>
    property ProjectedArrivalTimeTs: NullableInteger read FProjectedArrivalTimeTs write FProjectedArrivalTimeTs;

    /// <summary>
    ///  Estimated departure time UTC unixtime
    /// </summary>
    property ProjectedDepartureTimeTs: NullableInteger read FProjectedDepartureTimeTs write FProjectedDepartureTimeTs;

    /// <summary>
    ///  Time when the address was marked as visited UTC unixtime. This is actually equal to timestamp_last_visited most of the time
    /// </summary>
    property ActualArrivalTimeTs: NullableInteger read FActualArrivalTimeTs write FActualArrivalTimeTs;

    /// <summary>
    ///  Time when the address was mared as departed UTC. This is actually equal to timestamp_last_departed most of the time
    /// </summary>
    property ActualDepartureTimeTs: NullableInteger read FActualDepartureTimeTs write FActualDepartureTimeTs;

    /// <summary>
    ///  Estimated arrival time based on the current route progress, i.e. based on the last known actual_arrival_time
    /// </summary>
    property EstimatedArrivalTimeTs: NullableInteger read FEstimatedArrivalTimeTs write FEstimatedArrivalTimeTs;

    /// <summary>
    ///  Estimated departure time based on the current route progress
    /// </summary>
    property EstimatedDepartureTimeTs: NullableInteger read FEstimatedDepartureTimeTs write FEstimatedDepartureTimeTs;

    /// <summary>
    ///  This is the difference between the originally projected arrival time and Actual Arrival Time
    /// </summary>
    property TimeImpact: NullableInteger read FTimeImpact write FTimeImpact;
  end;

implementation

{ TManifest }

constructor TManifest.Create;
begin
  Inherited;

  FRunningServiceTime := NullableInteger.Null;
  FRunningTravelTime := NullableInteger.Null;
  FRunningWaitTime := NullableInteger.Null;
  FWaitTimeToNextDestination := NullableInteger.Null;
  FRunningDistance := NullableDouble.Null;
  FFuelFromStart := NullableDouble.Null;
  FFuelCostFromStart := NullableDouble.Null;
  FProjectedArrivalTimeTs := NullableInteger.Null;
  FProjectedDepartureTimeTs := NullableInteger.Null;
  FActualArrivalTimeTs := NullableInteger.Null;
  FActualDepartureTimeTs := NullableInteger.Null;
  FEstimatedArrivalTimeTs := NullableInteger.Null;
  FEstimatedDepartureTimeTs := NullableInteger.Null;
  FTimeImpact := NullableInteger.Null;
end;

function TManifest.Equals(Obj: TObject): Boolean;
var
  Other: TManifest;
begin
  Result := False;

  if not (Obj is TManifest) then
    Exit;

  Other := TManifest(Obj);

  Result :=
    (FRunningServiceTime = Other.FRunningServiceTime) and
    (FRunningTravelTime = Other.FRunningTravelTime) and
    (FRunningWaitTime = Other.FRunningWaitTime) and
    (FWaitTimeToNextDestination = Other.FWaitTimeToNextDestination) and
    (FRunningDistance = Other.FRunningDistance) and
    (FFuelFromStart = Other.FFuelFromStart) and
    (FFuelCostFromStart = Other.FFuelCostFromStart) and
    (FProjectedArrivalTimeTs = Other.FProjectedArrivalTimeTs) and
    (FProjectedDepartureTimeTs = Other.FProjectedDepartureTimeTs) and
    (FActualArrivalTimeTs = Other.FActualArrivalTimeTs) and
    (FActualDepartureTimeTs = Other.FActualDepartureTimeTs) and
    (FEstimatedArrivalTimeTs = Other.FEstimatedArrivalTimeTs) and
    (FEstimatedDepartureTimeTs = Other.FEstimatedDepartureTimeTs) and
    (FTimeImpact = Other.FTimeImpact);
end;

end.
