unit ManifestUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections,
  JSONNullableAttributeUnit,
  NullableBasicTypesUnit;

type
  /// <summary>
  /// Manifest
  /// </summary>
  TManifest = class
  private
    [JSONName('lat')]
    [Nullable]
    FLatitude: NullableDouble;

    [JSONName('lng')]
    [Nullable]
    FLongitude: NullableDouble;
  public
    /// <remarks>
    ///  Constructor with 0-arguments must be and be public.
    ///  For JSON-deserialization.
    /// </remarks>
    constructor Create;

    function Equals(Obj: TObject): Boolean; override;

    /// <summary>
    ///  Latitude
    /// </summary>
    property Latitude: NullableDouble read FLatitude write FLatitude;

    /// <summary>
    ///  Longitude
    /// </summary>
    property Longitude: NullableDouble read FLongitude write FLongitude;
  end;

  TManifestList = TList<TManifest>;

implementation

{ TManifest }

constructor TManifest.Create;
begin
  Inherited;

  FLatitude := NullableDouble.Null;
  FLongitude := NullableDouble.Null;
end;

function TManifest.Equals(Obj: TObject): Boolean;
var
  Other: TManifest;
begin
  Result := False;

  if not (Obj is TManifest) then
    Exit;

  Other := TManifest(Obj);

  Result :=
    (Latitude = Other.Latitude) and
    (Longitude = Other.Longitude);
end;

end.
