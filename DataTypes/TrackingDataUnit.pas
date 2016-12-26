unit TrackingDataUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections,
  Generics.Defaults,
  JSONNullableAttributeUnit,
  NullableBasicTypesUnit, EnumsUnit, CommonTypesUnit, GenericParametersUnit,
  JSONDictionaryIntermediateObjectUnit;

type
  TStatusHistory = class(TGenericParameters)
  private
    [JSONName('unix_timestamp')]
    [Nullable]
    FUnixTimestamp: NullableInteger;

    [JSONName('info')]
    [Nullable]
    FInfo: NullableString;

    function GetInfo: TTrackingInfo;
  public
    constructor Create; override;

    property Timestamp: NullableInteger read FUnixTimestamp;
    property Info: TTrackingInfo read GetInfo;
  end;
  TStatusHistoryArray = TArray<TStatusHistory>;

  TTimeWindow = class(TGenericParameters)
  private
    [JSONName('start_time')]
    [Nullable]
    FStartTime: NullableString;

    [JSONName('end_time')]
    [Nullable]
    FEndTime: NullableString;
  public
    constructor Create; override;

    property StartTime: NullableString read FStartTime;
    property EndTime: NullableString read FEndTime;
  end;
  TTimeWindowArray = TArray<TTimeWindow>;

  TTrackingArrival = class(TGenericParameters)
  private
    [JSONName('from_unix_timestamp')]
    [Nullable]
    FFromUnixTimestamp: NullableInteger;

    [JSONName('to_unix_timestamp')]
    [Nullable]
    FToUnixTimestamp: NullableInteger;
  public
    constructor Create; override;

    property FromUnixTimestamp: NullableInteger read FFromUnixTimestamp;
    property ToUnixTimestamp: NullableInteger read FToUnixTimestamp;
  end;
  TTrackingArrivalArray = TArray<TTrackingArrival>;

  TTrackingLocation = class(TGenericParameters)
  private
    [JSONName('lat')]
    [Nullable]
    FLatitude: NullableDouble;

    [JSONName('lng')]
    [Nullable]
    FLongitude: NullableDouble;

    [JSONName('info')]
    [Nullable]
    FInfo: NullableString;

    [JSONName('show_info')]
    [Nullable]
    FShowInfo: NullableBoolean;

    [JSONName('icon')]
    [Nullable]
    FIcon: NullableString;

    [JSONName('size')]
    [Nullable]
    FSize: NullableInteger;

    [JSONName('anchor')]
    FAnchors: TIntegerArray;

    [JSONName('popupAnchor')]
    FPopupAnchor: TIntegerArray;

    [JSONName('angle')]
    [Nullable]
    FAngle: NullableInteger;

    [JSONName('custom_data')]
    FCustomData: TStringArray;

    [JSONName('time_windows')]
    [NullableArray(TTimeWindow)]
    FTimeWindows: TTimeWindowArray;

  public
    constructor Create; override;

    property Latitude: NullableDouble read FLatitude;
    property Longitude: NullableDouble read FLongitude;
    property Info: NullableString read FInfo;
    property ShowInfo: NullableBoolean read FShowInfo;
    property Icon: NullableString read FIcon;
    property Size: NullableInteger read FSize;
    property Anchors: TIntegerArray read FAnchors;
    property PopupAnchor: TIntegerArray read FPopupAnchor;
    property Angle: NullableInteger read FAngle;
    property CustomData: TStringArray read FCustomData;
    property TimeWindows: TTimeWindowArray read FTimeWindows;
  end;
  TTrackingLocationArray = TArray<TTrackingLocation>;

  /// <summary>
  ///  Tracking numbers are numbers given to packages when they are shipped
  /// </summary>
  /// <remarks>
  ///  https://github.com/route4me/json-schemas/blob/master/Tracking_number.dtd
  /// </remarks>
  TTrackingData = class(TGenericParameters)
  private
    [JSONName('tracking_number')]
    [Nullable]
    FTrackingNumber: NullableString;

    [JSONName('status_history')]
    [NullableArray(TStatusHistory)]
    FStatusHistory: TStatusHistoryArray;

    [JSONName('locations')]
    [NullableArray(TTrackingLocation)]
    FLocations: TTrackingLocationArray;

    [JSONName('custom_data')]
    [NullableObject(TDictionaryStringIntermediateObject)]
    FCustomData: NullableObject;

    [JSONName('arrival')]
    [NullableArray(TTrackingArrival)]
    FArrivals: TTrackingArrivalArray;

    [JSONName('delivered')]
    [Nullable]
    FDelivered: NullableBoolean;
  public
    /// <remarks>
    ///  Constructor with 0-arguments must be and be public.
    ///  For JSON-deserialization.
    /// </remarks>
    constructor Create; override;

    /// <summary>
    ///  An unique internal ID of a tracking number object
    /// </summary>
    property TrackingNumber: NullableString read FTrackingNumber write FTrackingNumber;

    /// <summary>
    /// Status History
    /// </summary>
    property StatusHistory: TStatusHistoryArray read FStatusHistory;

    /// <summary>
    ///  Locations
    /// </summary>
    property Locations: TTrackingLocationArray read FLocations;

    /// <summary>
    ///  CustomData
    /// </summary>
    property CustomData: NullableObject read FCustomData;

    /// <summary>
    ///  Arrivals
    /// </summary>
    property Arrivals: TTrackingArrivalArray read FArrivals;

    /// <summary>
    ///  The original timestamp in unix timestamp format at the moment location transaction event
    /// </summary>
    property Delivered: NullableBoolean read FDelivered;
  end;

  TTrackingDataArray = TArray<TTrackingData>;
  TTrackingDataList = TObjectList<TTrackingData>;

implementation

{ TTrackingData }

constructor TTrackingData.Create;
begin
  FTrackingNumber := NullableString.Null;
  FCustomData := NullableObject.Null;
  FDelivered := NullableBoolean.Null;

  SetLength(FStatusHistory, 0);
  SetLength(FLocations, 0);
  SetLength(FArrivals, 0);
end;

{ TStatusHistory }

constructor TStatusHistory.Create;
begin
  inherited;

  FUnixTimestamp := NullableInteger.Null;
  FInfo := NullableString.Null;
end;

function TStatusHistory.GetInfo: TTrackingInfo;
var
  TrackingInfo: TTrackingInfo;
begin
  Result := TTrackingInfo.tiUnknown;
  if FInfo.IsNotNull then
    for TrackingInfo := Low(TTrackingInfo) to High(TTrackingInfo) do
      if (FInfo = TTrackingInfoDescription[TrackingInfo]) then
        Exit(TrackingInfo);
end;

{ TTimeWindow }

constructor TTimeWindow.Create;
begin
  inherited;

  FStartTime := NullableString.Null;
  FEndTime := NullableString.Null;
end;

{ TTrackingLocation }

constructor TTrackingLocation.Create;
begin
  inherited;

  FLatitude := NullableDouble.Null;
  FLongitude := NullableDouble.Null;
  FInfo := NullableString.Null;
  FShowInfo := NullableBoolean.Null;
  FIcon := NullableString.Null;
  FSize := NullableInteger.Null;
  FAngle := NullableInteger.Null;

  SetLength(FAnchors, 0);
  SetLength(FPopupAnchor, 0);
  SetLength(FCustomData, 0);
  SetLength(FTimeWindows, 0);
end;

{ TTrackingArrival }

constructor TTrackingArrival.Create;
begin
  inherited;

  FFromUnixTimestamp := NullableInteger.Null;
  FToUnixTimestamp := NullableInteger.Null;
end;

end.
