unit DirectionLocationUnit;

interface

uses
  REST.Json.Types, SysUtils,
  JSONNullableAttributeUnit, NullableBasicTypesUnit;

type
  /// <remarks>
  ///  https://github.com/route4me/json-schemas/blob/master/Direction.dtd
  /// </remarks>
  TDirectionLocation = class
  private
    [JSONName('name')]
    [Nullable]
    FName: NullableString;

    [JSONName('time')]
    [Nullable]
    FTime: NullableInteger;

    [JSONName('segment_distance')]
    [Nullable]
    FSegmentDistance: NullableDouble;

    [JSONName('start_location')]
    [Nullable]
    FStartLocation: NullableString;

    [JSONName('end_location')]
    [Nullable]
    FEndLocation: NullableString;

    [JSONName('directions_error')]
    [Nullable]
    FDirectionsError: NullableString;

    [JSONName('error_code')]
    [Nullable]
    FErrorCode: NullableInteger;
  public
    constructor Create;

    function Equals(Obj: TObject): Boolean; override;

    function CompareTo(Obj: TObject): Integer;

    /// <summary>
    ///  Direction name
    /// </summary>
    property Name: NullableString read FName write FName;

    /// <summary>
    ///  Segment time (seconds)
    /// </summary>
    property Time: NullableInteger read FTime write FTime;

    /// <summary>
    ///  Current segment length
    /// </summary>
    property SegmentDistance: NullableDouble read FSegmentDistance write FSegmentDistance;

    /// <summary>
    ///  Start location name
    /// </summary>
    property StartLocation: NullableString read FStartLocation write FStartLocation;

    /// <summary>
    ///  End location name
    /// </summary>
    property EndLocation: NullableString read FEndLocation write FEndLocation;

    /// <summary>
    ///  Directions error message
    /// </summary>
    property DirectionsError: NullableString read FDirectionsError write FDirectionsError;

    /// <summary>
    ///  Error code
    /// </summary>
    property ErrorCode: NullableInteger read FErrorCode write FErrorCode;
  end;

implementation

{ TDirectionLocation }

function TDirectionLocation.CompareTo(Obj: TObject): Integer;
begin
  Result := Name.Compare((Obj as TDirectionLocation).Name);
end;

constructor TDirectionLocation.Create;
begin
  Inherited;

  FName := NullableString.Null;
  FTime := NullableInteger.Null;
  FSegmentDistance := NullableDouble.Null;
  FStartLocation := NullableString.Null;
  FEndLocation := NullableString.Null;
  FDirectionsError := NullableString.Null;
  FErrorCode := NullableInteger.Null;
end;

function TDirectionLocation.Equals(Obj: TObject): Boolean;
var
  Other: TDirectionLocation;
begin
  Result := False;

  if not (Obj is TDirectionLocation) then
    Exit;

  Other := TDirectionLocation(Obj);

  Result :=
    (Name = Other.Name) and
    (Time = Other.Time) and
    (SegmentDistance = Other.SegmentDistance) and
    (StartLocation = Other.StartLocation) and
    (EndLocation = Other.EndLocation) and
    (DirectionsError = Other.DirectionsError) and
    (ErrorCode = Other.ErrorCode);
end;

end.
