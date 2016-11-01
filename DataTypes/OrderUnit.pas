unit OrderUnit;

interface

uses
  REST.Json.Types, Classes, SysUtils, System.Generics.Collections,
  JSONNullableAttributeUnit,
  GenericParametersUnit, NullableBasicTypesUnit,
  JSONDictionaryIntermediateObjectUnit;

type
  TOrder = class(TGenericParameters)
  private
    [JSONName('Order_id')]
    [Nullable]
    FOrderId: NullableString;

    [JSONName('address_1')]
    FAddress1: String;

    [JSONName('address_2')]
    [Nullable]
    FAddress2: NullableString;

    [JSONName('address_alias')]
    FAddressAlias: String;

    [JSONName('cached_lat')]
    FCachedLatitude: double;

    [JSONName('cached_lng')]
    FCachedLongitude: double;

    [JSONName('curbside_lat')]
    [Nullable]
    FCurbsideLatitude: NullableDouble;

    [JSONName('curbside_lng')]
    [Nullable]
    FCurbsideLongitude: NullableDouble;

    [JSONName('address_city')]
    [Nullable]
    FAddressCity: NullableString;

    [JSONName('address_state_id')]
    [Nullable]
    FAddressStateId: NullableString;

    [JSONName('address_country_id')]
    [Nullable]
    FAddressCountryId: NullableString;

    [JSONName('address_zip')]
    [Nullable]
    FAddressZIP: NullableString;

    [JSONName('order_status_id')]
    [Nullable]
    FOrderStatusId: NullableString;

    [JSONName('member_id')]
    [Nullable]
    FMemberId: NullableString;

    [JSONName('EXT_FIELD_first_name')]
    [Nullable]
    FFirstName: NullableString;

    [JSONName('EXT_FIELD_last_name')]
    [Nullable]
    FLastName: NullableString;

    [JSONName('EXT_FIELD_email')]
    [Nullable]
    FEmail: NullableString;

    [JSONName('EXT_FIELD_phone')]
    [Nullable]
    FPhone: NullableString;

    [JSONName('EXT_FIELD_custom_data')]
    [NullableObject(TDictionaryStringIntermediateObject)]
    FCustomData: NullableObject;
  public
    /// <remarks>
    ///  Constructor with 0-arguments must be and be public.
    ///  For JSON-deserialization.
    /// </remarks>
    constructor Create; overload; override;
    constructor Create(Address: String; AddressAlias: String;
      Latitude, Longitude: double); overload;

    function Equals(Obj: TObject): Boolean; override;

    /// <summary>
    /// Order Id
    /// </summary>
    property OrderId: NullableString read FOrderId write FOrderId;

    /// <summary>
    /// Address 1 field
    /// </summary>
    property Address1: String read FAddress1 write FAddress1;

    /// <summary>
    /// Address 2 field
    /// </summary>
    property Address2: NullableString read FAddress2 write FAddress2;

    /// <summary>
    /// Address Alias.
    /// </summary>
    property AddressAlias: String read FAddressAlias write FAddressAlias;

    /// <summary>
    /// Geo latitude
    /// </summary>
    property CachedLatitude: double read FCachedLatitude write FCachedLatitude;

    /// <summary>
    /// Geo longitude
    /// </summary>
    property CachedLongitude: double read FCachedLongitude write FCachedLongitude;

    /// <summary>
    /// Generate optimal routes and driving directions to this curbside latitude
    /// </summary>
    property CurbsideLatitude: NullableDouble read FCurbsideLatitude write FCurbsideLatitude;

    /// <summary>
    /// Generate optimal routes and driving directions to the curbside langitude
    /// </summary>
    property CurbsideLongitude: NullableDouble read FCurbsideLongitude write FCurbsideLongitude;

    /// <summary>
    /// Address City
    /// </summary>
    property AddressCity: NullableString read FAddressCity write FAddressCity;

    /// <summary>
    /// Address state ID
    /// </summary>
    property AddressStateId: NullableString read FAddressStateId write FAddressStateId;

    /// <summary>
    /// Address country ID
    /// </summary>
    property AddressCountryId: NullableString read FAddressCountryId write FAddressCountryId;

    /// <summary>
    /// Address ZIP
    /// </summary>
    property AddressZIP: NullableString read FAddressZIP write FAddressZIP;

    /// <summary>
    /// Order status ID
    /// </summary>
    property OrderStatusId: NullableString read FOrderStatusId write FOrderStatusId;

    /// <summary>
    /// The id of the member inside the route4me system
    /// </summary>
    property MemberId: NullableString read FMemberId write FMemberId;

    /// <summary>
    /// First name
    /// </summary>
    property FirstName: NullableString read FFirstName write FFirstName;

    /// <summary>
    /// Last name
    /// </summary>
    property LastName: NullableString read FLastName write FLastName;

    /// <summary>
    /// Email
    /// </summary>
    property Email: NullableString read FEmail write FEmail;

    /// <summary>
    /// Phone number
    /// </summary>
    property Phone: NullableString read FPhone write FPhone;

    /// <summary>
    /// Custom data
    /// </summary>
    property CustomData: NullableObject read FCustomData;
    procedure AddCustomField(Key: String; Value: String);
  end;

  TOrderArray = TArray<TOrder>;
//  TOrderList = TList<TOrder>;

implementation

{ TOrder }

constructor TOrder.Create;
begin
  Inherited;

  FAddress1 := EmptyStr;
  FAddressAlias := EmptyStr;
  FCachedLatitude := Double.NaN;
  FCachedLongitude := Double.NaN;

  FOrderId := NullableString.Null;
  FAddress2 := NullableString.Null;
  FCurbsideLatitude := NullableDouble.Null;
  FCurbsideLongitude := NullableDouble.Null;
  FAddressCity := NullableString.Null;
  FAddressStateId := NullableString.Null;
  FAddressCountryId := NullableString.Null;
  FAddressZIP := NullableString.Null;
  FOrderStatusId := NullableString.Null;
  FMemberId := NullableString.Null;
  FFirstName := NullableString.Null;
  FLastName := NullableString.Null;
  FEmail := NullableString.Null;
  FPhone := NullableString.Null;
  FCustomData := NullableObject.Null;
end;

procedure TOrder.AddCustomField(Key, Value: String);
var
  Dic: TDictionaryStringIntermediateObject;
begin
  if (FCustomData.IsNull) then
    FCustomData := TDictionaryStringIntermediateObject.Create();
  Dic := FCustomData.Value as TDictionaryStringIntermediateObject;
  Dic.Add(Key, Value);
end;

constructor TOrder.Create(Address, AddressAlias: String; Latitude,
  Longitude: double);
begin
  Create;

  FAddress1 := Address;
  FAddressAlias := AddressAlias;
  FCachedLatitude := Latitude;
  FCachedLongitude := Longitude;
end;

function TOrder.Equals(Obj: TObject): Boolean;
var
  Other: TOrder;
begin
  Result := False;

  if not (Obj is TOrder) then
    Exit;

  Other := TOrder(Obj);

  Result := (FOrderId = Other.FOrderId) and
    (FAddress1 = Other.FAddress1) and
    (FAddress2 = Other.FAddress2) and
    (FAddressAlias = Other.FAddressAlias) and
    (FMemberId = Other.FMemberId) and
    (FCachedLatitude = Other.FCachedLatitude) and
    (FCachedLongitude = Other.FCachedLongitude) and
    (FCurbsideLatitude = Other.FCurbsideLatitude) and
    (FCurbsideLongitude = Other.FCurbsideLongitude) and
    (FAddressCity = Other.FAddressCity) and
    (FAddressStateId = Other.FAddressStateId) and
    (FAddressCountryId = Other.FAddressCountryId) and
    (FAddressZIP = Other.FAddressZIP) and
    (FOrderStatusId = Other.FOrderStatusId) and
    (FMemberId = Other.FMemberId) and
    (FFirstName = Other.FFirstName) and
    (FLastName = Other.FLastName) and
    (FEmail = Other.FEmail) and
    (FPhone = Other.FPhone) and
    (FCustomData = Other.FCustomData);
end;

end.
