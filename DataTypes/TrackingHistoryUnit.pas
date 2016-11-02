unit TrackingHistoryUnit;

interface

uses
  REST.Json.Types, System.Rtti, Classes, SysUtils, System.Generics.Collections,
  Generics.Defaults,
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

    /// <summary>
    ///  Speed at the time of the location transaction event
    /// </summary>
    property Speed: NullableDouble read FSpeed write FSpeed;

    /// <summary>
    ///  Latitude at the time of the location transaction event
    /// </summary>
    property Latitude: NullableDouble read FLatitude write FLatitude;

    /// <summary>
    ///  Longitude at the time of the location transaction event
    /// </summary>
    property Longitude: NullableDouble read FLongitude write FLongitude;

    /// <summary>
    ///  Direction/Heading at the time of the location transaction event
    /// </summary>
    property Direction: NullableString read FD write FD;

    /// <summary>
    ///  The original timestamp in unix timestamp format at the moment location transaction event
    /// </summary>
    property TimeStamp: NullableString read FTimeStamp write FTimeStamp;

    /// <summary>
    ///  The original timestamp in a human readable timestamp format at the moment location transaction event
    /// </summary>
    property TimeStampFriendly: NullableString read FTimeStampFriendly write FTimeStampFriendly;
  end;

  TTrackingHistoryArray = TArray<TTrackingHistory>;

function SortTrackingHistory(TrackingHistories: TTrackingHistoryArray): TTrackingHistoryArray;

implementation

function SortTrackingHistory(TrackingHistories: TTrackingHistoryArray): TTrackingHistoryArray;
begin
  SetLength(Result, Length(TrackingHistories));
  if Length(TrackingHistories) = 0 then
    Exit;

  TArray.Copy<TTrackingHistory>(TrackingHistories, Result, Length(TrackingHistories));
  TArray.Sort<TTrackingHistory>(Result, TComparer<TTrackingHistory>.Construct(
    function (const History1, History2: TTrackingHistory): Integer
    begin
      Result := History1.Latitude.Compare(History2.Latitude);
      if (Result = 0) then
        Result := History1.Longitude.Compare(History2.Longitude);
    end));
end;

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

  Result :=
    (FSpeed = Other.FSpeed) and
    (FLatitude = Other.FLatitude) and
    (FLongitude = Other.FLongitude) and
    (FD = Other.FD) and
    (FTimeStamp = Other.FTimeStamp) and
    (FTimeStampFriendly = Other.FTimeStampFriendly);
end;

end.
