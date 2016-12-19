unit AddressGeocodingUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections, SysUtils,
  Generics.Defaults,
  JSONNullableAttributeUnit,
  NullableBasicTypesUnit, DirectionPathPointUnit;

type
  /// <summary>
  ///  Geocoding
  /// </summary>
  /// <remarks>
  ///  https://github.com/route4me/json-schemas/blob/master/geocoding.dtd
  /// </remarks>
  TAddressGeocoding = class
  private
    [JSONName('key')]
    [Nullable]
    FKey: NullableString;

    [JSONName('name')]
    [Nullable]
    FName: NullableString;

    [JSONName('bbox')]
    [NullableArray(TDirectionPathPoint)]
    FBoundaryBox: TDirectionPathPointArray;

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

    [JSONName('postalCode')]
    [Nullable]
    FPostalCode: NullableString;

    [JSONName('countryRegion')]
    [Nullable]
    FCountryRegion: NullableString;

    [JSONName('curbside_coordinates')]
    [NullableArray(TDirectionPathPoint)]
    FCurbsideCoordinates: TDirectionPathPointArray;

  public
    constructor Create;
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; override;

    /// <summary>
    ///  A unique identifier for the geocoding
    /// </summary>
    property Key: NullableString read FKey write FKey;

    /// <summary>
    ///  Specific description of the geocoding result
    /// </summary>
    property Name: NullableString read FName write FName;

    /// <summary>
    ///  Boundary box
    /// </summary>
    property BoundaryBox: TDirectionPathPointArray read FBoundaryBox;
    procedure AddBoundaryBox(Value: TDirectionPathPoint);

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
    property Confidence: NullableString read FConfidence write FConfidence;

    /// <summary>
    ///  Non-standardized. Is used for tooltip ("Street", "City" etc)
    /// </summary>
    property Type_: NullableString read FType write FType;

    /// <summary>
    ///  If the result has a postal code, it's returned here
    /// </summary>
    property PostalCode: NullableString read FPostalCode write FPostalCode;

    /// <summary>
    ///  If the region is known, it's returned here
    /// </summary>
    property CountryRegion: NullableString read FCountryRegion write FCountryRegion;

    /// <summary>
    ///  Curbside Coordinates
    /// </summary>
    property CurbsideCoordinates: TDirectionPathPointArray read FCurbsideCoordinates;
    procedure AddCurbsideCoordinate(Value: TDirectionPathPoint);
  end;

  TAddressGeocodingArray = TArray<TAddressGeocoding>;

function SorTAddressGeocodings(Geocodings: TAddressGeocodingArray): TAddressGeocodingArray;

implementation

function SorTAddressGeocodings(Geocodings: TAddressGeocodingArray): TAddressGeocodingArray;
begin
  SetLength(Result, Length(Geocodings));
  if Length(Geocodings) = 0 then
    Exit;

  TArray.Copy<TAddressGeocoding>(Geocodings, Result, Length(Geocodings));
  TArray.Sort<TAddressGeocoding>(Result, TComparer<TAddressGeocoding>.Construct(
    function (const Geocoding1, Geocoding2: TAddressGeocoding): Integer
    begin
      Result := Geocoding1.Key.Compare(Geocoding2.Key);
    end));
end;

{ TAddressGeocoding }

procedure TAddressGeocoding.AddBoundaryBox(Value: TDirectionPathPoint);
begin
  SetLength(FBoundaryBox, Length(FBoundaryBox) + 1);
  FBoundaryBox[High(FBoundaryBox)] := Value;
end;

procedure TAddressGeocoding.AddCurbsideCoordinate(Value: TDirectionPathPoint);
begin
  SetLength(FCurbsideCoordinates, Length(FCurbsideCoordinates) + 1);
  FCurbsideCoordinates[High(FCurbsideCoordinates)] := Value;
end;

constructor TAddressGeocoding.Create;
begin
  Inherited;

  FKey := NullableString.Null;
  FName := NullableString.Null;
  FLatitude := NullableDouble.Null;
  FLongitude := NullableDouble.Null;
  FConfidence := NullableString.Null;
  FType := NullableString.Null;
  FPostalCode := NullableString.Null;
  FCountryRegion := NullableString.Null;

  SetLength(FBoundaryBox, 0);
  SetLength(FCurbsideCoordinates, 0);
end;

destructor TAddressGeocoding.Destroy;
var
  i: integer;
begin
  for i := Length(FBoundaryBox) - 1 downto 0 do
    FreeAndNil(FBoundaryBox[i]);

  for i := Length(FCurbsideCoordinates) - 1 downto 0 do
    FreeAndNil(FCurbsideCoordinates[i]);

  inherited;
end;

function TAddressGeocoding.Equals(Obj: TObject): Boolean;
var
  Other: TAddressGeocoding;
  i: integer;
  SortedBoundaryBox1, SortedBoundaryBox2: TDirectionPathPointArray;
  SortedCurbsideCoordinates1, SortedCurbsideCoordinates2: TDirectionPathPointArray;
begin
  Result := False;

  if not (Obj is TAddressGeocoding) then
    Exit;

  Other := TAddressGeocoding(Obj);

  Result :=
    (FKey = Other.FKey) and
    (FName = Other.FName) and
    (FLongitude = Other.FLongitude) and
    (FConfidence = Other.FConfidence) and
    (FType = Other.FType) and
    (FPostalCode = Other.FPostalCode) and
    (FCountryRegion = Other.FCountryRegion);

  if not Result then
    Exit;

  Result := False;

  if (Length(FBoundaryBox) <> Length(Other.FBoundaryBox)) or
    (Length(FCurbsideCoordinates) <> Length(Other.FCurbsideCoordinates)) then
    Exit;

  SortedBoundaryBox1 := DirectionPathPointUnit.SortDirectionPathPoints(BoundaryBox);
  SortedBoundaryBox2 := DirectionPathPointUnit.SortDirectionPathPoints(Other.BoundaryBox);
  for i := 0 to Length(SortedBoundaryBox1) - 1 do
    if (not SortedBoundaryBox1[i].Equals(SortedBoundaryBox2[i])) then
      Exit;

  SortedCurbsideCoordinates1 := DirectionPathPointUnit.SortDirectionPathPoints(CurbsideCoordinates);
  SortedCurbsideCoordinates2 := DirectionPathPointUnit.SortDirectionPathPoints(Other.CurbsideCoordinates);
  for i := 0 to Length(SortedCurbsideCoordinates1) - 1 do
    if (not SortedCurbsideCoordinates1[i].Equals(SortedCurbsideCoordinates2[i])) then
      Exit;

  Result := True;
end;

end.
