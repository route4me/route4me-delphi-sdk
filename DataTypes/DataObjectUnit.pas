unit DataObjectUnit;

interface

uses
  System.Generics.Collections, SysUtils, Generics.Defaults,
  REST.Json.Types, JSONNullableAttributeUnit,
  EnumsUnit, AddressUnit, RouteParametersUnit, DirectionUnit,
  NullableBasicTypesUnit, LinksUnit, TrackingHistoryUnit, DirectionPathPointUnit;

type
  TDataObjectRoute = class;
  TDataObjectRouteArray = TArray<TDataObjectRoute>;

  TDataObject = class
  strict private
    [JSONName('optimization_problem_id')]
    FOptimizationProblemId: String;

    [JSONName('state')]
    FState: Integer; //TOptimizationState;

    [JSONName('user_errors')]
    FUserErrors: TArray<String>;

    [JSONName('sent_to_background')]
    FIsSentToBackground: boolean;

    [JSONName('addresses')]
    FAddresses: TAddressesArray;

    [JSONName('parameters')]
    FParameters: TRouteParameters;

    [JSONName('routes')]
    FRoutes: TDataObjectRouteArray;

    [JSONName('links')]
    FLinks: TLinks;

    [JSONName('tracking_history')]
    FTrackingHistories: TTrackingHistoryArray;

    [JSONName('directions')]
    FDirections: TDirectionArray;

    [JSONName('path')]
    FDirectionPathPoints: TDirectionPathPointArray;
  public
    constructor Create;

    function Equals(Obj: TObject): Boolean; override;

    property OptimizationProblemId: String read FOptimizationProblemId write FOptimizationProblemId;
    property State: Integer {TOptimizationState} read FState write FState;
    property UserErrors: TArray<String> read FUserErrors write FUserErrors;
    property IsSentToBackground: boolean read FIsSentToBackground write FIsSentToBackground;
    property Addresses: TAddressesArray read FAddresses write FAddresses;
    property Parameters: TRouteParameters read FParameters write FParameters;
    property Routes: TDataObjectRouteArray read FRoutes write FRoutes;
    property Links: TLinks read FLinks write FLinks;
    property TrackingHistories: TTrackingHistoryArray read FTrackingHistories write FTrackingHistories;
    property Directions: TDirectionArray read FDirections write FDirections;
    property DirectionPathPoints: TDirectionPathPointArray read FDirectionPathPoints write FDirectionPathPoints;
  end;

  TDataObjectRoute = class(TDataObject)
  strict private
    [JSONName('route_id')]
    FRouteId: String;

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

    constructor Create(RouteId: String); overload;

    function Equals(Obj: TObject): Boolean; override;

    property RouteId: String read FRouteId write FRouteId;
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

  function SortRoutes(Routes: TDataObjectRouteArray): TDataObjectRouteArray;

implementation

uses
  Math, CommonTypesUnit;

function SortRoutes(Routes: TDataObjectRouteArray): TDataObjectRouteArray;
begin
  SetLength(Result, Length(Routes));
  TArray.Copy<TDataObjectRoute>(Routes, Result, Length(Routes));
  TArray.Sort<TDataObjectRoute>(Result, TComparer<TDataObjectRoute>.Construct(
    function (const Route1, Route2: TDataObjectRoute): Integer
    begin
      Result := String.Compare(Route1.RouteId, Route2.RouteId);
    end));
end;

{ TDataObject }

constructor TDataObject.Create;
begin
  SetLength(FUserErrors, 0);
  SetLength(FAddresses, 0);
  FParameters := nil;
  SetLength(FRoutes, 0);
  FLinks := nil;
  SetLength(FTrackingHistories, 0);
  SetLength(FDirections, 0);
  SetLength(FDirectionPathPoints, 0);
end;

function TDataObject.Equals(Obj: TObject): Boolean;
var
  Other: TDataObject;
  i: integer;
  Index: integer;
  SortedAddresses1, SortedAddresses2: TAddressesArray;
  SortedUserErrors1, SortedUserErrors2: TStringArray;
  SortedRoutes1, SortedRoutes2: TDataObjectRouteArray;
  SortedTrackingHistory1, SortedTrackingHistory2: TTrackingHistoryArray;
  SortedDirections1, SortedDirections2: TDirectionArray;
  SortedDirectionPathPoints1, SortedDirectionPathPoints2: TDirectionPathPointArray;
begin
  Result := False;

  if not (Obj is TDataObject) then
    Exit;

  Other := TDataObject(Obj);


  Result := (OptimizationProblemId = Other.OptimizationProblemId) and
    (State = Other.State) and
    (IsSentToBackground = Other.IsSentToBackground) and
    (Parameters.Equals(Other.Parameters)) and
    (Links.Equals(Other.Links));

  if (not Result) then
    Exit;

  if (Length(UserErrors) <> Length(Other.UserErrors)) or
    (Length(Addresses) <> Length(Other.Addresses)) or
    (Length(Routes) <> Length(Other.Routes)) or
    (Length(TrackingHistories) <> Length(Other.TrackingHistories)) or
    (Length(Directions) <> Length(Other.Directions)) or
    (Length(DirectionPathPoints) <> Length(Other.DirectionPathPoints)) then
    Exit;

  SortedUserErrors1 := SortStringArray(UserErrors);
  SortedUserErrors2 := SortStringArray(Other.UserErrors);
  for i := 0 to Length(SortedUserErrors1) - 1 do
    if not SortedUserErrors1[i].Equals(SortedUserErrors2[i]) then
      Exit;

  SortedAddresses1 := AddressUnit.SortAddresses(Addresses);
  SortedAddresses2 := AddressUnit.SortAddresses(Other.Addresses);
  for i := 0 to Length(SortedAddresses1) - 1 do
    if (not SortedAddresses1[i].Equals(SortedAddresses2[i])) then
      Exit;

  SortedRoutes1 :=  SortRoutes(Routes);
  SortedRoutes2 := SortRoutes(Other.Routes);
  for i := 0 to Length(SortedRoutes1) - 1 do
    if (not SortedRoutes1[i].Equals(SortedRoutes2[i])) then
      Exit;

  SortedTrackingHistory1 := TrackingHistoryUnit.SortTrackingHistory(TrackingHistories);
  SortedTrackingHistory2 := TrackingHistoryUnit.SortTrackingHistory(Other.TrackingHistories);
  for i := 0 to Length(SortedTrackingHistory1) - 1 do
    if (not SortedTrackingHistory1[i].Equals(SortedTrackingHistory2[i])) then
      Exit;

  SortedDirections1 := DirectionUnit.SortDirections(Directions);
  SortedDirections2 := DirectionUnit.SortDirections(Other.Directions);
  for i := 0 to Length(SortedDirections1) - 1 do
    if (not SortedDirections1[i].Equals(SortedDirections2[i])) then
      Exit;

  SortedDirectionPathPoints1 := DirectionPathPointUnit.SortDirectionPathPoints(DirectionPathPoints);
  SortedDirectionPathPoints2 := DirectionPathPointUnit.SortDirectionPathPoints(Other.DirectionPathPoints);
  for i := 0 to Length(SortedDirectionPathPoints1) - 1 do
    if (not SortedDirectionPathPoints1[i].Equals(SortedDirectionPathPoints2[i])) then
      Exit;
end;

{ TDataObjectRoute }

constructor TDataObjectRoute.Create;
begin
  FRouteId := EmptyStr;
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

constructor TDataObjectRoute.Create(RouteId: String);
begin
  Create;

  FRouteId := RouteId;
end;

function TDataObjectRoute.Equals(Obj: TObject): Boolean;
var
  Other: TDataObjectRoute;
begin
  Result := inherited Equals(Obj);

  if not Result then
    Exit;

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
