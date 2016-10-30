unit DataObjectUnit;

interface

uses
  System.Generics.Collections, SysUtils, Generics.Defaults,
  REST.Json.Types, JSONNullableAttributeUnit,
  EnumsUnit, AddressUnit, RouteParametersUnit, DirectionUnit, CommonTypesUnit,
  NullableBasicTypesUnit, LinksUnit, TrackingHistoryUnit, DirectionPathPointUnit;

type
  TDataObjectRoute = class;
  TDataObjectRouteArray = TArray<TDataObjectRoute>;

  /// <remarks>
  ///  https://github.com/route4me/json-schemas/blob/master/Optimization_response.dtd
  /// </remarks>
  TDataObject = class
  strict private
    [JSONName('optimization_problem_id')]
    FOptimizationProblemId: String;

    [JSONName('state')]
    FState: Integer;

    [JSONName('user_errors')]
    FUserErrors: TStringArray;

    [JSONName('sent_to_background')]
    [Nullable]
    FIsSentToBackground: NullableBoolean;

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

    function GetState: TOptimizationState;
    procedure SetState(const Value: TOptimizationState);
  public
    constructor Create;
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; override;

    procedure AddAddress(Address: TAddress);
    procedure AddRoute(Route: TDataObjectRoute);

    /// <summary>
    ///  Optimization which generated this route
    /// </summary>
    property OptimizationProblemId: String read FOptimizationProblemId write FOptimizationProblemId;

    /// <summary>
    ///  An optimization problem can be at one state at any given time
    ///  Initial = 1
    ///  MatrixProcessing = 2
    ///  Optimizing = 3
    ///  Optimized = 4
    ///  Error = 5
    ///  ComputingDirections = 6
    /// </summary>
    property State: TOptimizationState read GetState write SetState;

    /// <summary>
    ///  User Errors
    /// </summary>
    property UserErrors: TStringArray read FUserErrors write FUserErrors;

    /// <summary>
    ///  If true it means the solution was not returned (it is being computed in the background)
    /// </summary>
    property IsSentToBackground: NullableBoolean read FIsSentToBackground write FIsSentToBackground;

    /// <summary>
    ///  Route Addresses
    /// </summary>
    property Addresses: TAddressesArray read FAddresses;

    /// <summary>
    ///  Route Parameters
    /// </summary>
    property Parameters: TRouteParameters read FParameters write FParameters;

    /// <summary>
    ///  Routes
    /// </summary>
    property Routes: TDataObjectRouteArray read FRoutes;

    /// <summary>
    ///  Links to the GET operations for the optimization problem
    /// </summary>
    property Links: TLinks read FLinks write FLinks;

    /// <summary>
    ///  A collection of device tracking data with coordinates, speed, and timestamps
    /// </summary>
    property TrackingHistories: TTrackingHistoryArray read FTrackingHistories write FTrackingHistories;
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

    [JSONName('directions')]
    FDirections: TDirectionArray;

    [JSONName('path')]
    FDirectionPathPoints: TDirectionPathPointArray;
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

    /// <summary>
    ///  Edge by edge turn-by-turn directions.
    ///  Note: For round-trip routes (parameters.rt = true), the return to the start address is returned as well
    /// </summary>
    property Directions: TDirectionArray read FDirections write FDirections;

    /// <summary>
    ///  Edge-wise path to be drawn on the map
    /// </summary>
    property Path: TDirectionPathPointArray read FDirectionPathPoints write FDirectionPathPoints;
  end;
  TDataObjectRouteList = TList<TDataObjectRoute>;

  function SortRoutes(Routes: TDataObjectRouteArray): TDataObjectRouteArray;

implementation

uses
  Math;

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

procedure TDataObject.AddAddress(Address: TAddress);
begin
  SetLength(FAddresses, Length(FAddresses) + 1);
  FAddresses[High(FAddresses)] := Address;
end;

procedure TDataObject.AddRoute(Route: TDataObjectRoute);
begin
  SetLength(FRoutes, Length(FRoutes) + 1);
  FRoutes[High(FRoutes)] := Route;
end;

constructor TDataObject.Create;
begin
  FState := 0;
  FIsSentToBackground := NullableBoolean.Null;
  SetLength(FUserErrors, 0);
  SetLength(FAddresses, 0);
  FParameters := nil;
  SetLength(FRoutes, 0);
  FLinks := nil;
  SetLength(FTrackingHistories, 0);
end;

destructor TDataObject.Destroy;
var
  i: integer;
begin
  for i := Length(FAddresses) - 1 downto 0 do
    FreeAndNil(FAddresses[i]);
  for i := Length(FRoutes) - 1 downto 0 do
    FreeAndNil(FRoutes[i]);

  inherited;
end;

function TDataObject.Equals(Obj: TObject): Boolean;
var
  Other: TDataObject;
  i: integer;
  SortedAddresses1, SortedAddresses2: TAddressesArray;
  SortedUserErrors1, SortedUserErrors2: TStringArray;
  SortedRoutes1, SortedRoutes2: TDataObjectRouteArray;
  SortedTrackingHistory1, SortedTrackingHistory2: TTrackingHistoryArray;
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
    (Length(TrackingHistories) <> Length(Other.TrackingHistories)) then
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

  Result := True;
end;

{ TDataObjectRoute }

function TDataObject.GetState: TOptimizationState;
begin
  Result := TOptimizationState(FState);
end;

procedure TDataObject.SetState(const Value: TOptimizationState);
begin
  FState := Integer(Value);
end;

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

  SetLength(FDirections, 0);
  SetLength(FDirectionPathPoints, 0);
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
  SortedDirections1, SortedDirections2: TDirectionArray;
  SortedDirectionPathPoints1, SortedDirectionPathPoints2: TDirectionPathPointArray;
  i: integer;
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

  if (not Result) then
    Exit;

  if (Length(Directions) <> Length(Other.Directions)) or
    (Length(Path) <> Length(Other.Path)) then
    Exit;

  SortedDirections1 := DirectionUnit.SortDirections(Directions);
  SortedDirections2 := DirectionUnit.SortDirections(Other.Directions);
  for i := 0 to Length(SortedDirections1) - 1 do
    if (not SortedDirections1[i].Equals(SortedDirections2[i])) then
      Exit;

  SortedDirectionPathPoints1 := DirectionPathPointUnit.SortDirectionPathPoints(Path);
  SortedDirectionPathPoints2 := DirectionPathPointUnit.SortDirectionPathPoints(Other.Path);
  for i := 0 to Length(SortedDirectionPathPoints1) - 1 do
    if (not SortedDirectionPathPoints1[i].Equals(SortedDirectionPathPoints2[i])) then
      Exit;

  Result := True;
end;

end.
