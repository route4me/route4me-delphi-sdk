unit AddressBookContactUnit;

interface

uses NullableBasicTypesUnit, GenericParametersUnit;

type

  TAddressBookContact = class(TGenericParameters)
  private
    [JSONName('address_id')]
    [JSONNullable]
    FId: NullableString;

    [JSONName('address_group')]
    [JSONNullable]
    FAddressGroup: NullableString;

    [JSONName('address_alias')]
    [JSONNullable]
    FAlias: NullableString;

    [JSONName('address_1')]
    FAddress: String;

    [JSONName('address_2')]
    [JSONNullable]
    FAddress2: NullableString;

    [JSONName('first_name')]
    [JSONNullable]
    FFirstName: NullableString;

    [JSONName('last_name')]
    [JSONNullable]
    FLastName: NullableString;

    [JSONName('address_email')]
    [JSONNullable]
    FEmail: NullableString;

    [JSONName('address_phone_number')]
    [JSONNullable]
    FPhoneNumber: NullableString;

    [JSONName('address_city')]
    [JSONNullable]
    FCity: NullableString;

    [JSONName('address_state_id')]
    [JSONNullable]
    FStateId: NullableString;

    [JSONName('address_country_id')]
    [JSONNullable]
    FCountryId: NullableString;

    [JSONName('address_zip')]
    [JSONNullable]
    FZip: NullableString;

    [JSONName('cached_lat')]
    [JSONNullable]
    FLatitude: Double;

    [JSONName('cached_lng')]
    [JSONNullable]
    FLongitude: Double;

    [JSONName('color')]
    [JSONNullable]
    FColor: NullableString;

    [JSONName('address_icon')]
    [JSONNullable]
    FAddressIcon: NullableString;

    [JSONName('address_custom_data')]
    [JSONNullable]
    FCustomData: NullableObject;

    constructor Create; overload;
  public
    constructor Create(Address: String; Latitude, Longitude: Double); overload;

    procedure AddCustomData(Key: String; Value: String);

    property Id: NullableString read FId write FId;
    /// <summary>
    ///  Address group
    /// </summary>
    property AddressGroup: NullableString read FAddressGroup write FAddressGroup;

    /// <summary>
    ///  Address alias
    /// </summary>
    property Alias: NullableString read FAlias write FAlias;

    /// <summary>
    ///  The route Address Line 1
    /// </summary>
    property Address: String read FAddress write FAddress;

    /// <summary>
    ///  The route Address Line 2
    /// </summary>
    property Address2: NullableString read FAddress2 write FAddress2;

    /// <summary>
    ///  First name
    /// </summary>
    property FirstName: NullableString read FFirstName write FFirstName;

    /// <summary>
    ///  Last name
    /// </summary>
    property LastName: NullableString read FLastName write FLastName;

    /// <summary>
    ///  Address email
    /// </summary>
    property Email: NullableString read FEmail write FEmail;

    /// <summary>
    ///  Address phone number
    /// </summary>
    property PhoneNumber: NullableString read FPhoneNumber write FPhoneNumber;

    /// <summary>
    ///  Address city
    /// </summary>
    property City: NullableString read FCity write FCity;

    /// <summary>
    ///  Address state ID
    /// </summary>
    property StateId: NullableString read FStateId write FStateId;

    /// <summary>
    ///  Address country ID
    /// </summary>
    property CountryId: NullableString read FCountryId write FCountryId;

    /// <summary>
    ///  Address zip code
    /// </summary>
    property Zip: NullableString read FZip write FZip;

    /// <summary>
    ///  Cached latitude
    /// </summary>
    property Latitude: Double read FLatitude write FLatitude;

    /// <summary>
    ///  Cached longitude
    /// </summary>
    property Longitude: Double read FLongitude write FLongitude;

    /// <summary>
    ///  Color of an address
    /// </summary>
    property Color: NullableString read FColor write FColor;

    /// <summary>
    ///  URL to an address icon file
    /// </summary>
    property AddressIcon: NullableString read FAddressIcon write FAddressIcon;

    /// <summary>
    ///  Address custom data
    /// </summary>
    property CustomData: NullableObject read FCustomData write FCustomData;
  end;

implementation

{ TAddressBookContact }

uses JSONDictionaryInterceptorObjectUnit;

constructor TAddressBookContact.Create;
begin
  FId := NullableString.Null;
  FAddressGroup := NullableString.Null;
  FAlias := NullableString.Null;
  FAddress2 := NullableString.Null;
  FFirstName := NullableString.Null;
  FLastName := NullableString.Null;
  FEmail := NullableString.Null;
  FPhoneNumber := NullableString.Null;
  FCity := NullableString.Null;
  FStateId := NullableString.Null;
  FCountryId := NullableString.Null;
  FZip := NullableString.Null;
  FColor := NullableString.Null;
  FAddressIcon := NullableString.Null;
  FCustomData := NullableObject.Null;
end;

{ TAddressBookContact }

procedure TAddressBookContact.AddCustomData(Key, Value: String);
var
  Dic: TDictionaryStringIntermediateObject;
begin
  if (FCustomData.IsNull) then
    FCustomData := TDictionaryStringIntermediateObject.Create();
  Dic := FCustomData.Value as TDictionaryStringIntermediateObject;
  Dic.Add(Key, Value);
end;

constructor TAddressBookContact.Create(Address: String; Latitude, Longitude: Double);
begin
  Create;

  FAddress := Address;
  FLatitude := Latitude;
  FLongitude := Longitude;
end;

end.
