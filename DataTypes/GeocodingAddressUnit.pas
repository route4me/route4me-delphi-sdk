unit GeocodingAddressUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections,
  JSONNullableAttributeUnit, GenericParametersUnit, NullableBasicTypesUnit;

type
  TGeocodingAddress = class(TGenericParameters)
  private
    [JSONName('zipcode')]
    [Nullable]
    FZipCode: NullableString;

    [JSONName('street_name')]
    [Nullable]
    FStreetName: NullableString;
  public
    /// <remarks>
    ///  Constructor with 0-arguments must be and be public.
    ///  For JSON-deserialization.
    /// </remarks>
    constructor Create; override;

    /// <summary>
    /// Zip Code
    /// </summary>
    property ZipCode: NullableString read FZipCode write FZipCode;

    /// <summary>
    /// Street Name
    /// </summary>
    property StreetName: NullableString read FStreetName write FStreetName;
  end;

  TGeocodingAddressArray = TArray<TGeocodingAddress>;
  TGeocodingAddressList = TObjectList<TGeocodingAddress>;

implementation

constructor TGeocodingAddress.Create;
begin
  Inherited;

  FZipCode := NullableString.Null;
  FStreetName := NullableString.Null;
end;

end.
