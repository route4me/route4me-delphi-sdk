unit DataObjectUnit;

interface

uses
  System.Generics.Collections, Generics.Defaults,
  REST.Json.Types,
  JSONNullableAttributeUnit,
  EnumsUnit, AddressUnit, RouteParametersUnit, DirectionUnit,
  NullableBasicTypesUnit, LinksUnit, TrackingHistoryUnit, DirectionPathPointUnit;

type
  TDataObjectRoute = class;

  TDataObject = class
  private
    [JSONName('optimization_problem_id')]
    FOptimizationProblemId: String;

    [JSONName('state')]
    FState: Integer; //TOptimizationState;

    [JSONName('user_errors')]
    FUserErrors: TArray<String>;

    [JSONName('sent_to_background')]
    FIsSentToBackground: boolean;

    [JSONName('addresses')]
    FAddresses: TArray<TAddress>;

    [JSONName('parameters')]
    FParameters: TRouteParameters;

    [JSONName('routes')]
    FRoutes: TArray<TDataObjectRoute>;

    [JSONName('links')]
    FLinks: TLinks;

    [JSONName('tracking_history')]
    FTrackingHistory: TArray<TTrackingHistory>;

    [JSONName('directions')]
    FDirection: TArray<TDirection>;

    [JSONName('path')]
    FDirectionPathPoint: TArray<TDirectionPathPoint>;
  public
    constructor Create;

    function SortAddresses: TArray<TAddress>;

    property OptimizationProblemId: String read FOptimizationProblemId write FOptimizationProblemId;
    property State: Integer {TOptimizationState} read FState write FState;
    property UserErrors: TArray<String> read FUserErrors write FUserErrors;
    property IsSentToBackground: boolean read FIsSentToBackground write FIsSentToBackground;
    property Addresses: TArray<TAddress> read FAddresses write FAddresses;
    property Parameters: TRouteParameters read FParameters write FParameters;
    property Routes: TArray<TDataObjectRoute> read FRoutes write FRoutes;
    property Links: TLinks read FLinks write FLinks;
    property TrackingHistory: TArray<TTrackingHistory> read FTrackingHistory write FTrackingHistory;
    property Direction: TArray<TDirection> read FDirection write FDirection;
    property DirectionPathPoint: TArray<TDirectionPathPoint> read FDirectionPathPoint write FDirectionPathPoint;
  end;

  TDataObjectRoute = class(TDataObject)
  private
    [JSONName('route_id')]
    [Nullable]
    FRouteId: NullableString;

    [JSONName('member_id')]
    [Nullable]
    FMemberId: NullableString;

    [JSONName('member_email')]
    [Nullable]
    FMemberEmail: NullableString;

    [JSONName('vehicle_alias')]
    [Nullable]
    FVehicleAlias: NullableString;

    [JSONName('driver_alias')]
    [Nullable]
    FDriverAlias: NullableString;

    [JSONName('route_cost')]
    [Nullable]
    FRouteCost: NullableDouble;

    [JSONName('route_revenue')]
    [Nullable]
    FRouteRevenue: NullableDouble;

    [JSONName('net_revenue_per_distance_unit')]
    [Nullable]
    FNetRevenuePerDistanceUnit: NullableDouble;

    [JSONName('created_timestamp')]
    [Nullable]
    FCreatedTimestamp: NullableInteger;

    [JSONName('mpg')]
    [Nullable]
    FMpg: NullableString;

    [JSONName('trip_distance')]
    [Nullable]
    FTripDistance: NullableDouble;

    [JSONName('gas_price')]
    [Nullable]
    FGasPrice: NullableDouble;

    [JSONName('route_duration_sec')]
    [Nullable]
    FRouteDurationSec: NullableInteger;
  public
    /// <remarks>
    ///  Constructor with 0-arguments must be and be public.
    ///  For JSON-deserialization.
    /// </remarks>
    constructor Create; overload;

    function Equals(Obj: TObject): Boolean; override;

    property RouteId: NullableString read FRouteId write FRouteId;
    property MemberId: NullableString read FMemberId write FMemberId;
    property MemberEmail: NullableString read FMemberEmail write FMemberEmail;
    property VehicleAlias: NullableString read FVehicleAlias write FVehicleAlias;
    property DriverAlias: NullableString read FDriverAlias write FDriverAlias;
    property RouteCost: NullableDouble read FRouteCost write FRouteCost;
    property RouteRevenue: NullableDouble read FRouteRevenue write FRouteRevenue;
    property NetRevenuePerDistanceUnit: NullableDouble read FNetRevenuePerDistanceUnit write FNetRevenuePerDistanceUnit;
    property CreatedTimestamp: NullableInteger read FCreatedTimestamp write FCreatedTimestamp;
    property Mpg: NullableString read FMpg write FMpg;
    property TripDistance: NullableDouble read FTripDistance write FTripDistance;
    property GasPrice: NullableDouble read FGasPrice write FGasPrice;
    property RouteDurationSec: NullableInteger read FRouteDurationSec write FRouteDurationSec;
  end;

implementation

uses
  Math;

{ TDataObject }

constructor TDataObject.Create;
begin
  SetLength(FUserErrors, 0);
  SetLength(FAddresses, 0);
  FParameters := nil;
  SetLength(FRoutes, 0);
  FLinks := nil;
  SetLength(FTrackingHistory, 0);
  SetLength(FDirection, 0);
  SetLength(FDirectionPathPoint, 0);
end;

function TDataObject.SortAddresses: TArray<TAddress>;
begin
  SetLength(Result, Length(FAddresses));
  TArray.Copy<TAddress>(FAddresses, Result, Length(FAddresses));
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


{ TDataObjectRoute }

constructor TDataObjectRoute.Create;
begin
  FRouteId := NullableString.Null;
  FMemberId := NullableString.Null;
  FMemberEmail := NullableString.Null;
  FVehicleAlias := NullableString.Null;
  FDriverAlias := NullableString.Null;
  FRouteCost := NullableDouble.Null;
  FRouteRevenue := NullableDouble.Null;
  FNetRevenuePerDistanceUnit := NullableDouble.Null;
  FCreatedTimestamp := NullableInteger.Null;
  FMpg := NullableString.Null;
  FTripDistance := NullableDouble.Null;
  FGasPrice := NullableDouble.Null;
  FRouteDurationSec := NullableInteger.Null;
end;

{ TDataObjectRoute }

function TDataObjectRoute.Equals(Obj: TObject): Boolean;
var
  Other: TDataObjectRoute;
begin
  Result := False;

  if not (Obj is TDataObjectRoute) then
    Exit;

  Other := TDataObjectRoute(Obj);

  Result := (RouteId = Other.RouteId) and
    (MemberId = Other.MemberId) and
    (MemberEmail = Other.MemberEmail) and
    (VehicleAlias = Other.VehicleAlias) and
    (DriverAlias = Other.DriverAlias) and
    (RouteCost = Other.RouteCost) and
    (RouteRevenue = Other.RouteRevenue) and
    (NetRevenuePerDistanceUnit = Other.NetRevenuePerDistanceUnit) and
    (CreatedTimestamp = Other.CreatedTimestamp) and
    (Mpg = Other.Mpg) and
    (TripDistance = Other.TripDistance) and
    (GasPrice = Other.GasPrice) and
    (RouteDurationSec = Other.RouteDurationSec);
end;

end.
