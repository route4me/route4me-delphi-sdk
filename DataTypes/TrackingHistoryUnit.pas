unit TrackingHistoryUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections,
  Generics.Defaults,
  JSONNullableAttributeUnit,
  NullableBasicTypesUnit;

type
  /// <summary>
  ///  Tracking History
  /// </summary>
  /// <remarks>
  ///  https://github.com/route4me/json-schemas/blob/master/TrackingHistory.dtd
  /// </remarks>
  TTrackingHistory = class
  private
    [JSONName('s')]
    [Nullable]
    FSpeed: NullableString;

    [JSONName('lt')]
    [Nullable]
    FLatitude: NullableString;

    [JSONName('lg')]
    [Nullable]
    FLongitude: NullableString;

    [JSONName('d')]
    [Nullable]
    FD: NullableString;

    [JSONName('m')]
    [Nullable]
    FM: NullableString;

    [JSONName('ts')]
    [Nullable]
    FTimeStamp: NullableString;

    [JSONName('src')]
    [Nullable]
    FSrc: NullableString;

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
    property Speed: NullableString read FSpeed write FSpeed;

    /// <summary>
    ///  Latitude at the time of the location transaction event
    /// </summary>
    property Latitude: NullableString read FLatitude write FLatitude;

    /// <summary>
    ///  Longitude at the time of the location transaction event
    /// </summary>
    property Longitude: NullableString read FLongitude write FLongitude;

    /// <summary>
    ///  Direction/Heading at the time of the location transaction event
    /// </summary>
    property Direction: NullableString read FD write FD;

    /// <summary>
    ///  The original timestamp in unix timestamp format at the moment location transaction event
    /// </summary>
    property TimeStamp: NullableString read FTimeStamp write FTimeStamp;

    /// <summary>
    ///
    /// </summary>
    property M: NullableString read FM;

    /// <summary>
    ///
    /// </summary>
    property Src: NullableString read FSrc;

    /// <summary>
    ///  The original timestamp in a human readable timestamp format at the moment location transaction event
    /// </summary>
    property TimeStampFriendly: NullableString read FTimeStampFriendly write FTimeStampFriendly;
  end;

  TTrackingHistoryArray = TArray<TTrackingHistory>;
  TTrackingHistoryList = TObjectList<TTrackingHistory>;

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
  FSpeed := NullableString.Null;
  FLatitude := NullableString.Null;
  FLongitude := NullableString.Null;
  FD := NullableString.Null;
  FTimeStamp := NullableString.Null;
  FTimeStampFriendly := NullableString.Null;
  FM := NullableString.Null;
  FSrc := NullableString.Null;
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
    (FM = Other.FM) and
    (FSrc = Other.FSrc) and
    (FTimeStamp = Other.FTimeStamp) and
    (FTimeStampFriendly = Other.FTimeStampFriendly);
end;

end.
