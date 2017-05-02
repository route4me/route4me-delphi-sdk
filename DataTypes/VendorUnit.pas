unit VendorUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections, SysUtils,
  JSONNullableAttributeUnit, GenericParametersUnit, NullableBasicTypesUnit,
  EnumsUnit;

type
  TVendorFeature = class(TGenericParameters)
  private
    [JSONName('id')]
    [Nullable]
    FId: NullableString;

    [JSONName('name')]
    [Nullable]
    FName: NullableString;

    [JSONName('slug')]
    [Nullable]
    FSlug: NullableString;

    [JSONName('feature_group')]
    [Nullable]
    FFeatureGroup: NullableString;
  public
    constructor Create(); override;

    property Id: NullableString read FId write FId;
    property Name: NullableString read FName write FName;
    property Slug: NullableString read FSlug write FSlug;
    property FeatureGroup: NullableString read FFeatureGroup write FFeatureGroup;
  end;
  TVendorFeatureArray = TArray<TVendorFeature>;


  TVendorCountry = class(TGenericParameters)
  private
    [JSONName('id')]
    [Nullable]
    FId: NullableString;

    [JSONName('country_code')]
    [Nullable]
    FCode: NullableString;

    [JSONName('country_name')]
    [Nullable]
    FName: NullableString;

  public
    constructor Create(); override;

    property Id: NullableString read FId write FId;
    property Name: NullableString read FName write FName;
    property Code: NullableString read FCode write FCode;
  end;
  TVendorCountryArray = TArray<TVendorCountry>;

  /// <summary>
  ///  Vendor
  /// </summary>
  TVendor = class(TGenericParameters)
  private
    [JSONName('id')]
    [Nullable]
    FId: NullableString;

    [JSONName('name')]
    [Nullable]
    FName: NullableString;

    [JSONName('title')]
    [Nullable]
    FTitle: NullableString;

    [JSONName('slug')]
    [Nullable]
    FSlug: NullableString;

    [JSONName('description')]
    [Nullable]
    FDescription: NullableString;

    [JSONName('website_url')]
    [Nullable]
    FWebsiteUrl: NullableString;

    [JSONName('logo_url')]
    [Nullable]
    FLogoUrl: NullableString;

    [JSONName('api_docs_url')]
    [Nullable]
    FApiDocsUrl: NullableString;

    [JSONName('is_integrated')]
    [Nullable]
    FIsIntegrated: NullableString;

    [JSONName('size')]
    [Nullable]
    FSize: NullableString;

    [JSONNameAttribute('features')]
    [NullableArray(TVendorFeature)]
    FFeatures: TVendorFeatureArray;

    [JSONNameAttribute('countries')]
    [NullableArray(TVendorCountry)]
    FCountries: TVendorCountryArray;

    function GetIsIntegrated: NullableBoolean;
    procedure SetIsIntegrated(const Value: NullableBoolean);
    function GetSize: TVendorSizeType;
    procedure SetSize(const Value: TVendorSizeType);
  public
    /// <remarks>
    ///  Constructor with 0-arguments must be and be public.
    ///  For JSON-deserialization.
    /// </remarks>
    constructor Create; override;
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; override;

    /// <summary>
    /// Vendor ID
    /// </summary>
    property Id: NullableString read FId write FId;

    /// <summary>
    /// Vendor name
    /// </summary>
    property Name: NullableString read FName write FName;

    /// <summary>
    /// Vendor title
    /// </summary>
    property Title: NullableString read FTitle write FTitle;

    /// <summary>
    /// Slug
    /// </summary>
    property Slug: NullableString read FSlug write FSlug;

    /// <summary>
    ///  Description of a vendor
    /// </summary>
    property Description: NullableString read FDescription write FDescription;

    /// <summary>
    /// URL to a vendor's logo
    /// </summary>
    property LogoUrl: NullableString read FLogoUrl write FLogoUrl;

    /// <summary>
    /// Website URL of a vendor
    /// </summary>
    property WebsiteUrl: NullableString read FWebsiteUrl write FWebsiteUrl;

    /// <summary>
    /// URL to a vendor's API documentation.
    /// </summary>
    property ApiDocsUrl: NullableString read FApiDocsUrl write FApiDocsUrl;

    /// <summary>
    /// If True, the vendor is integrated in the Route4Me system.
    /// </summary>
    property IsIntegrated: NullableBoolean read GetIsIntegrated write SetIsIntegrated;

    /// <summary>
    /// Filter vendors by size. Accepted values: global, regional, local.
    /// </summary>
    property Size: TVendorSizeType read GetSize write SetSize;

    property Features: TVendorFeatureArray read FFeatures;

    property Countries: TVendorCountryArray read FCountries;
  end;

  TVendorArray = TArray<TVendor>;
  TVendorList = TObjectList<TVendor>;

implementation

{ TVendor }

constructor TVendor.Create;
begin
  Inherited;

  FId := NullableString.Null;
  FName := NullableString.Null;
  FTitle := NullableString.Null;
  FSlug := NullableString.Null;
  FDescription := NullableString.Null;
  FLogoUrl := NullableString.Null;
  FWebsiteUrl := NullableString.Null;
  FApiDocsUrl := NullableString.Null;
  FIsIntegrated := NullableString.Null;
  FSize := NullableString.Null;

  SetLength(FFeatures, 0);
  SetLength(FCountries, 0);
end;

destructor TVendor.Destroy;
var
  i: Integer;
begin
  for i := High(FFeatures) downto 0 do
    FreeAndNil(FFeatures[i]);
  Finalize(FFeatures);

  for i := High(FCountries) downto 0 do
    FreeAndNil(FCountries[i]);
  Finalize(FCountries);

  inherited;
end;

function TVendor.Equals(Obj: TObject): Boolean;
var
  Other: TVendor;
begin
  Result := False;

  if not (Obj is TVendor) then
    Exit;

  Other := TVendor(Obj);

  Result :=
    (FId = Other.FId) and
    (FName = Other.FName) and
    (FTitle = Other.FTitle) and
    (FSlug = Other.FSlug) and
    (FDescription = Other.FDescription) and
    (FLogoUrl = Other.FLogoUrl) and
    (FWebsiteUrl = Other.FWebsiteUrl) and
    (FApiDocsUrl = Other.FApiDocsUrl) and
    (FSize = Other.FSize) and
    (FIsIntegrated = Other.FIsIntegrated);
end;

function TVendor.GetIsIntegrated: NullableBoolean;
begin
  if (FIsIntegrated.IsNull) then
    Result := NullableBoolean.Null
  else
    Result := (FIsIntegrated.Value = '1');
end;

function TVendor.GetSize: TVendorSizeType;
var
  Size: TVendorSizeType;
begin
  Result := TVendorSizeType.vsGlobal;
  if FSize.IsNotNull then
    for Size := Low(TVendorSizeType) to High(TVendorSizeType) do
      if (FSize = TVendorSizeTypeDescription[Size]) then
        Exit(Size);
end;

procedure TVendor.SetIsIntegrated(const Value: NullableBoolean);
begin
  if Value.IsNull then
    FIsIntegrated := NullableString.Null
  else
    if Value.Value then
      FIsIntegrated := '1'
    else
      FIsIntegrated := '0';
end;

procedure TVendor.SetSize(const Value: TVendorSizeType);
begin
  FSize := TVendorSizeTypeDescription[Value];
end;

{ TVendorFeature }

constructor TVendorFeature.Create;
begin
  inherited;

  FId := NullableString.Null;
  FName := NullableString.Null;
  FSlug := NullableString.Null;
  FFeatureGroup := NullableString.Null;
end;

{ TVendorCountry }

constructor TVendorCountry.Create;
begin
  inherited;

  FId := NullableString.Null;
  FName := NullableString.Null;
  FCode := NullableString.Null;
end;

end.
