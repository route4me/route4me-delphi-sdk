unit GeocodingUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections, SysUtils,
  Generics.Defaults,
  JSONNullableAttributeUnit,
  NullableBasicTypesUnit, EnumsUnit;

type
  /// <summary>
  ///  Geocoding
  /// </summary>
  TGeocoding = class
  private
    [JSONName('destination')]
    [Nullable]
    FDestination: NullableString;

    [JSONName('lat')]
    [Nullable]
    FLatitude: NullableDouble;

    [JSONName('lng')]
    [Nullable]
    FLongitude: NullableDouble;

    [JSONName('confidence')]
    [Nullable]
    FConfidence: NullableString;

    [JSONName('type')]
    [Nullable]
    FType: NullableString;

    [JSONName('original')]
    [Nullable]
    FOriginal: NullableString;

    function GetConfidence: TConfidenceType;
    procedure SetConfidence(const Value: TConfidenceType);
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///  Specific description of the geocoding result
    /// </summary>
    property Destination: NullableString read FDestination write FDestination;

    /// <summary>
    ///  Latitude
    /// </summary>
    property Latitude: NullableDouble read FLatitude write FLatitude;

    /// <summary>
    ///  Longitude
    /// </summary>
    property Longitude: NullableDouble read FLongitude write FLongitude;

    /// <summary>
    ///  Confidence ("high", "medium", "low")
    /// </summary>
    property Confidence: TConfidenceType read GetConfidence write SetConfidence;
    procedure SetConfidenceAsString(Value: NullableString);

    /// <summary>
    ///  Non-standardized. Is used for tooltip ("Street", "City" etc)
    /// </summary>
    property Type_: NullableString read FType write FType;

    /// <summary>
    /// </summary>
    property Original: NullableString read FOriginal write FOriginal;
  end;

  TGeocodingArray = TArray<TGeocoding>;
  TGeocodingList = TObjectList<TGeocoding>;

implementation

{ TGeocoding }

constructor TGeocoding.Create;
begin
  Inherited;

  FDestination := NullableString.Null;
  FLatitude := NullableDouble.Null;
  FLongitude := NullableDouble.Null;
  FConfidence := NullableString.Null;
  FType := NullableString.Null;
  FOriginal := NullableString.Null;
end;

destructor TGeocoding.Destroy;
begin
  inherited;
end;

function TGeocoding.GetConfidence: TConfidenceType;
var
  ConfidenceType: TConfidenceType;
begin
  Result := TConfidenceType.ctUnknown;
  if FConfidence.IsNotNull then
    for ConfidenceType := Low(TConfidenceType) to High(TConfidenceType) do
      if (FConfidence = TConfidenceTypeDescription[ConfidenceType]) then
        Exit(ConfidenceType);
end;

procedure TGeocoding.SetConfidence(const Value: TConfidenceType);
begin
  FConfidence := TConfidenceTypeDescription[Value];
end;

procedure TGeocoding.SetConfidenceAsString(Value: NullableString);
begin
  FConfidence := Value;
end;

end.
