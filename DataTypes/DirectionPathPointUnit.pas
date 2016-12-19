unit DirectionPathPointUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections, Generics.Defaults,
  JSONNullableAttributeUnit,
  NullableBasicTypesUnit;

type
  /// <summary>
  ///  Path - an array of the geographic points, which are laying on a path
  /// </summary>
  /// <remarks>
  ///  https://github.com/route4me/json-schemas/blob/master/Path.dtd
  /// </remarks>
  TDirectionPathPoint = class
  strict private
    [JSONName('lat')]
    [Nullable]
    FLatitude: NullableDouble;

    [JSONName('lng')]
    [Nullable]
    FLongitude: NullableDouble;
  public
    /// <remarks>
    ///  Constructor with 0-arguments must be and be public.
    ///  For JSON-deserialization.
    /// </remarks>
    constructor Create; overload;
    constructor Create(Latitude, Longitude: double); reintroduce; overload;

    function Equals(Obj: TObject): Boolean; override;

    /// <summary>
    ///  Latitude
    /// </summary>
    property Latitude: NullableDouble read FLatitude write FLatitude;
    /// <summary>
    ///  Longitude
    /// </summary>
    property Longitude: NullableDouble read FLongitude write FLongitude;
  end;

  TDirectionPathPointArray = TArray<TDirectionPathPoint>;
  TDirectionPathPointList = TList<TDirectionPathPoint>;

function SortDirectionPathPoints(DirectionPathPoints: TDirectionPathPointArray): TDirectionPathPointArray;

implementation

function SortDirectionPathPoints(DirectionPathPoints: TDirectionPathPointArray): TDirectionPathPointArray;
begin
  SetLength(Result, Length(DirectionPathPoints));
  if Length(DirectionPathPoints) = 0 then
    Exit;

  TArray.Copy<TDirectionPathPoint>(DirectionPathPoints, Result, Length(DirectionPathPoints));
  TArray.Sort<TDirectionPathPoint>(Result, TComparer<TDirectionPathPoint>.Construct(
    function (const Direction1, Direction2: TDirectionPathPoint): Integer
    begin
      Result := Direction1.Latitude.Compare(Direction2.Latitude);
      if (Result = 0) then
        Result := Direction1.Longitude.Compare(Direction2.Longitude);
    end));
end;

{ TDirectionPathPoint }

constructor TDirectionPathPoint.Create;
begin
  inherited;

  FLatitude := NullableDouble.Null;
  FLongitude := NullableDouble.Null;
end;

constructor TDirectionPathPoint.Create(Latitude, Longitude: double);
begin
  FLatitude := Latitude;
  FLongitude := Longitude;
end;

function TDirectionPathPoint.Equals(Obj: TObject): Boolean;
var
  Other: TDirectionPathPoint;
begin
  Result := False;

  if not (Obj is TDirectionPathPoint) then
    Exit;

  Other := TDirectionPathPoint(Obj);

  Result :=
    (FLatitude = Other.FLatitude) and
    (FLongitude = Other.FLongitude);
end;

end.
