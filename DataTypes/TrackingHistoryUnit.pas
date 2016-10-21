unit TrackingHistoryUnit;

interface

uses
  REST.Json.Types, System.Rtti, Classes, SysUtils,
  JSONNullableAttributeUnit,
  NullableBasicTypesUnit;

type
  TTrackingHistory = class
  private
    [JSONName('s')]
    [Nullable]
    FSpeed: NullableDouble;

    [JSONName('lt')]
    [Nullable]
    FLatitude: NullableDouble;

    [JSONName('lg')]
    [Nullable]
    FLongitude: NullableDouble;

    [JSONName('d')]
    [Nullable]
    FD: NullableString;

    [JSONName('ts')]
    [Nullable]
    FTimeStamp: NullableString;

    [JSONName('ts_friendly')]
    [Nullable]
    FTimeStampFriendly: NullableString;
  public
    /// <remarks>
    ///  Constructor with 0-arguments must be and be public.
    ///  For JSON-deserialization.
    /// </remarks>
    constructor Create; overload;

    function Equals(Obj: TObject): Boolean; override;

    property Speed: NullableDouble read FSpeed write FSpeed;
    property Latitude: NullableDouble read FLatitude write FLatitude;
    property Longitude: NullableDouble read FLongitude write FLongitude;
    property D: NullableString read FD write FD;
    property TimeStamp: NullableString read FTimeStamp write FTimeStamp;
    property TimeStampFriendly: NullableString read FTimeStampFriendly write FTimeStampFriendly;
  end;

implementation

{ TTrackingHistory }

constructor TTrackingHistory.Create;
begin
  FSpeed := NullableDouble.Null;
  FLatitude := NullableDouble.Null;
  FLongitude := NullableDouble.Null;
  FD := NullableString.Null;
  FTimeStamp := NullableString.Null;
  FTimeStampFriendly := NullableString.Null;
end;

function TTrackingHistory.Equals(Obj: TObject): Boolean;
var
  Other: TTrackingHistory;
begin
  Result := False;

  if not (Obj is TTrackingHistory) then
    Exit;

  Other := TTrackingHistory(Obj);

  Result := (Speed = Other.Speed) and
    (Latitude = Other.Latitude) and
    (Longitude = Other.Longitude) and
    (D = Other.D) and
    (TimeStamp = Other.TimeStamp) and
    (TimeStampFriendly = Other.TimeStampFriendly);
end;

end.
