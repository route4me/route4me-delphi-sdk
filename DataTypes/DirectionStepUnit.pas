unit DirectionStepUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections, Generics.Defaults,
  JSONNullableAttributeUnit,
  DirectionLocationUnit, DirectionPathPointUnit, EnumsUnit,
  NullableBasicTypesUnit;

type
  /// <remarks>
  ///  https://github.com/route4me/json-schemas/blob/master/Direction.dtd
  /// </remarks>
  TDirectionStep = class
  private
    [JSONName('direction')]
    [Nullable]
    FDirection: NullableString;

    [JSONName('directions')]
    [Nullable]
    FDirections: NullableString;

    [JSONName('distance')]
    [Nullable]
    FDistance: NullableDouble;

    [JSONName('distance_unit')]
    [Nullable]
    FDistanceUnit: NullableString;

    [JSONName('maneuverType')]
    [Nullable]
    FManeuverType: NullableString;

    [JSONName('compass_direction')]
    [Nullable]
    FCompassDirection: NullableString;

    [JSONName('duration_sec')]
    [Nullable]
    FDurationSec: NullableInteger;

    [JSONName('maneuverPoint')]
    [NullableObject(TDirectionPathPoint)]
    FManeuverPoint: NullableObject;

    function GetCompassDirection: TCompassDirection;
    function GetDirections: TDirectionEnum;
    function GetDistanceUnit: TDistanceUnit;
    function GetManeuverPoint: TDirectionPathPoint;
    function GetManeuverType: TDirectionEnum;
    procedure SetCompassDirection(const Value: TCompassDirection);
    procedure SetDirections(const Value: TDirectionEnum);
    procedure SetDistanceUnit(const Value: TDistanceUnit);
    procedure SetManeuverPoint(const Value: TDirectionPathPoint);
    procedure SetManeuverType(const Value: TDirectionEnum);
  public
    constructor Create;
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; override;

    /// <summary>
    ///  Text for current direction
    /// </summary>
    property Direction: NullableString read FDirection write FDirection;

    /// <summary>
    ///  Directions of movement
    /// </summary>
    property Directions: TDirectionEnum read GetDirections write SetDirections;

    /// <summary>
    ///  Step distance
    /// </summary>
    property Distance: NullableDouble read FDistance write FDistance;

    /// <summary>
    ///  Distance unit - 'mi' or 'km'
    /// </summary>
    property DistanceUnit: TDistanceUnit read GetDistanceUnit write SetDistanceUnit;

    /// <summary>
    ///  Maneuver type
    /// </summary>
    property ManeuverType: TDirectionEnum read GetManeuverType write SetManeuverType;

    /// <summary>
    ///  Compass direction
    /// </summary>
    property CompassDirection: TCompassDirection read GetCompassDirection write SetCompassDirection;

    /// <summary>
    ///  Step duration (sec)
    /// </summary>
    property DurationSec: NullableInteger read FDurationSec write FDurationSec;

    /// <summary>
    ///  Step distance
    /// </summary>
    property ManeuverPoint: TDirectionPathPoint read GetManeuverPoint write SetManeuverPoint;
  end;

  TDirectionStepArray = TArray<TDirectionStep>;
  TDirectionStepList = TList<TDirectionStep>;
//  TDirectionStepListClass = class(TDirectionStepList);

  function SortDirectionSteps(DirectionSteps: TDirectionStepArray): TDirectionStepArray;

implementation

function SortDirectionSteps(DirectionSteps: TDirectionStepArray): TDirectionStepArray;
begin
  SetLength(Result, Length(DirectionSteps));
  if Length(DirectionSteps) = 0 then
    Exit;

  TArray.Copy<TDirectionStep>(DirectionSteps, Result, Length(DirectionSteps));
  TArray.Sort<TDirectionStep>(Result, TComparer<TDirectionStep>.Construct(
    function (const DirectionStep1, DirectionStep2: TDirectionStep): Integer
    begin
      Result := DirectionStep1.Direction.Compare(DirectionStep2.Direction);
    end));
end;

{ TDirectionStep }

constructor TDirectionStep.Create;
begin
  FDirection := NullableString.Null;
  FDirections := NullableString.Null;
  FDistance := NullableDouble.Null;
  FDistanceUnit := NullableString.Null;
  FManeuverType := NullableString.Null;
  FCompassDirection := NullableString.Null;
  FDurationSec := NullableInteger.Null;
  FManeuverPoint := NullableObject.Null;
end;

destructor TDirectionStep.Destroy;
begin
  FManeuverPoint.Free;
  inherited;
end;

function TDirectionStep.Equals(Obj: TObject): Boolean;
var
  Other: TDirectionStep;
begin
  Result := False;

  if not (Obj is TDirectionStep) then
    Exit;

  Other := TDirectionStep(Obj);

  Result :=
    (FDirection = Other.FDirection) and
    (FDirections = Other.FDirections) and
    (FDistance = Other.FDistance) and
    (FDistanceUnit = Other.FDistanceUnit) and
    (FManeuverType = Other.FManeuverType) and
    (FCompassDirection = Other.FCompassDirection) and
    (FDurationSec = Other.FDurationSec) and
    (FManeuverPoint = Other.FManeuverPoint);
end;

function TDirectionStep.GetCompassDirection: TCompassDirection;
var
  CompassDirection: TCompassDirection;
begin
  if FCompassDirection.IsNull then
    Exit(TCompassDirection.cdUndefined);

  for CompassDirection := Low(TCompassDirection) to High(TCompassDirection) do
    if (FCompassDirection = TCompassDirectionDescription[CompassDirection]) then
      Exit(CompassDirection);
end;

function TDirectionStep.GetDirections: TDirectionEnum;
var
  DirectionEnum: TDirectionEnum;
begin
  if FDirections.IsNull then
    Exit(TDirectionEnum.dUnknown);

  for DirectionEnum := Low(TDirectionEnum) to High(TDirectionEnum) do
    if (FDirections = TDirectionDescription[DirectionEnum]) then
      Exit(DirectionEnum);
end;

function TDirectionStep.GetDistanceUnit: TDistanceUnit;
var
  DistanceUnit: TDistanceUnit;
begin
  if FDistanceUnit.IsNull then
    Exit(TDistanceUnit.Undefinded);

  for DistanceUnit := Low(TDistanceUnit) to High(TDistanceUnit) do
    if (FDistanceUnit = TDistanceUnitDescription[DistanceUnit]) then
      Exit(DistanceUnit);
end;

function TDirectionStep.GetManeuverPoint: TDirectionPathPoint;
begin
  if FManeuverPoint.IsNull then
    Result := nil
  else
    Result := FManeuverPoint.Value as TDirectionPathPoint;
end;

function TDirectionStep.GetManeuverType: TDirectionEnum;
var
  ManeuverType: TDirectionEnum;
begin
  if FDistanceUnit.IsNull then
    Exit(TDirectionEnum.dUnknown);

  for ManeuverType := Low(TDirectionEnum) to High(TDirectionEnum) do
    if (FManeuverType = TManeuverTypeDescription[ManeuverType]) then
      Exit(ManeuverType);
end;

procedure TDirectionStep.SetCompassDirection(const Value: TCompassDirection);
begin
  FCompassDirection := TCompassDirectionDescription[Value];
end;

procedure TDirectionStep.SetDirections(const Value: TDirectionEnum);
begin
  FDirections := TDirectionDescription[Value];
end;

procedure TDirectionStep.SetDistanceUnit(const Value: TDistanceUnit);
begin
  FDistanceUnit := TDistanceUnitDescription[Value];
end;

procedure TDirectionStep.SetManeuverPoint(const Value: TDirectionPathPoint);
begin
  FManeuverPoint := Value;
end;

procedure TDirectionStep.SetManeuverType(const Value: TDirectionEnum);
begin
  FManeuverType := TManeuverTypeDescription[Value];
end;

end.
