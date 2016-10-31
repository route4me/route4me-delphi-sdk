unit GeocodingUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections, System.Rtti, Classes, SysUtils,
  Generics.Defaults,
  JSONNullableAttributeUnit,
  NullableBasicTypesUnit, EnumsUnit, DirectionPathPointUnit;

type
  /// <summary>
  /// Address
  /// </summary>
  TGeocoding = class
  private
    [JSONName('key')]
    [Nullable]
    FKey: NullableString;

    [JSONName('name')]
    [Nullable]
    FName: NullableString;

    [JSONName('bbox')]
    [NullableObject(TDirectionPathPointListClass)]
    FBoundaryBox: NullableObject;

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
    [NullableObject(TDirectionPathPointListClass)]
    FCurbsideCoordinates: NullableObject;

    function GetBoundaryBox: TDirectionPathPointList;
    function GetCurbsideCoordinates: TDirectionPathPointList;
    procedure SetBoundaryBox(const Value: TDirectionPathPointList);
    procedure SetCurbsideCoordinates(const Value: TDirectionPathPointList);

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
    property BoundaryBox: TDirectionPathPointList read GetBoundaryBox write SetBoundaryBox;

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
    property CurbsideCoordinates: TDirectionPathPointList read GetCurbsideCoordinates write SetCurbsideCoordinates;
  end;

  TGeocodingArray = TArray<TGeocoding>;
  TGeocodingList = TList<TGeocoding>;
  TGeocodingListClass = class(TGeocodingList);

function SortGeocodings(Geocodings: TGeocodingArray): TGeocodingArray;

implementation

function SortGeocodings(Geocodings: TGeocodingArray): TGeocodingArray;
begin
  SetLength(Result, Length(Geocodings));
  TArray.Copy<TGeocoding>(Geocodings, Result, Length(Geocodings));
  TArray.Sort<TGeocoding>(Result, TComparer<TGeocoding>.Construct(
    function (const Geocoding1, Geocoding2: TGeocoding): Integer
    begin
      Result := Geocoding1.Key.Compare(Geocoding2.Key);
    end));
end;

{ TGeocoding }

constructor TGeocoding.Create;
begin
  Inherited;

  FKey := NullableString.Null;
  FName := NullableString.Null;
  FBoundaryBox := NullableObject.Null;
  FLatitude := NullableDouble.Null;
  FLongitude := NullableDouble.Null;
  FConfidence := NullableString.Null;
  FType := NullableString.Null;
  FPostalCode := NullableString.Null;
  FCountryRegion := NullableString.Null;
  FCurbsideCoordinates := NullableObject.Null;
end;

destructor TGeocoding.Destroy;
begin
  FCurbsideCoordinates.Free;
  FBoundaryBox.Free;

  inherited;
end;

function TGeocoding.Equals(Obj: TObject): Boolean;
var
  Other: TGeocoding;
  i: integer;
  SortedBoundaryBox1, SortedBoundaryBox2: TDirectionPathPointArray;
  SortedCurbsideCoordinates1, SortedCurbsideCoordinates2: TDirectionPathPointArray;
begin
  Result := False;

  if not (Obj is TGeocoding) then
    Exit;

  Other := TGeocoding(Obj);

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

  if (FBoundaryBox.IsNull and Other.FBoundaryBox.IsNotNull) or
    (FBoundaryBox.IsNotNull and Other.FBoundaryBox.IsNull) or
    (FCurbsideCoordinates.IsNull and Other.FCurbsideCoordinates.IsNotNull) or
    (FCurbsideCoordinates.IsNotNull and Other.FCurbsideCoordinates.IsNull) then
    Exit;

  if (BoundaryBox <> nil) then
  begin
    if (BoundaryBox.Count <> Other.BoundaryBox.Count) then
      Exit;

    SortedBoundaryBox1 := DirectionPathPointUnit.SortDirectionPathPoints(BoundaryBox.ToArray);
    SortedBoundaryBox2 := DirectionPathPointUnit.SortDirectionPathPoints(Other.BoundaryBox.ToArray);
    for i := 0 to Length(SortedBoundaryBox1) - 1 do
      if (not SortedBoundaryBox1[i].Equals(SortedBoundaryBox2[i])) then
        Exit;
  end;

  if (CurbsideCoordinates <> nil) then
  begin
    if (CurbsideCoordinates.Count <> Other.CurbsideCoordinates.Count) then
      Exit;

    SortedCurbsideCoordinates1 := DirectionPathPointUnit.SortDirectionPathPoints(CurbsideCoordinates.ToArray);
    SortedCurbsideCoordinates2 := DirectionPathPointUnit.SortDirectionPathPoints(Other.CurbsideCoordinates.ToArray);
    for i := 0 to Length(SortedCurbsideCoordinates1) - 1 do
      if (not SortedCurbsideCoordinates1[i].Equals(SortedCurbsideCoordinates2[i])) then
        Exit;
  end;

  Result := true;
end;

function TGeocoding.GetBoundaryBox: TDirectionPathPointList;
begin
  if (FBoundaryBox.IsNull) then
    Result := nil
  else
    Result := FBoundaryBox.Value as TDirectionPathPointList;
end;

function TGeocoding.GetCurbsideCoordinates: TDirectionPathPointList;
begin
  if (FCurbsideCoordinates.IsNull) then
    Result := nil
  else
    Result := FCurbsideCoordinates.Value as TDirectionPathPointList;
end;

procedure TGeocoding.SetBoundaryBox(const Value: TDirectionPathPointList);
begin
  FBoundaryBox := Value;
end;

procedure TGeocoding.SetCurbsideCoordinates(const Value: TDirectionPathPointList);
begin
  FCurbsideCoordinates := Value;
end;

end.
