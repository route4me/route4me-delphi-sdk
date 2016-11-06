unit AddressBookContactUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections,
  JSONNullableAttributeUnit, NullableBasicTypesUnit, GenericParametersUnit,
  JSONDictionaryIntermediateObjectUnit;

type
  /// <summary>
  ///  Address Book Contact
  /// </summary>
  /// <remarks>
  ///  https://github.com/route4me/json-schemas/blob/master/Addressbook_v4.dtd
  /// </remarks>
  TAddressBookContact = class(TGenericParameters)
  private
    [JSONName('address_id')]
    [Nullable]
    FId: NullableInteger;

    [JSONName('address_group')]
    [Nullable]
    FAddressGroup: NullableString;

    [JSONName('address_alias')]
    [Nullable]
    FAlias: NullableString;

    [JSONName('address_1')]
    FAddress: String;

    [JSONName('address_2')]
    [Nullable]
    FAddress2: NullableString;

    [JSONName('first_name')]
    [Nullable]
    FFirstName: NullableString;

    [JSONName('last_name')]
    [Nullable]
    FLastName: NullableString;

    [JSONName('address_email')]
    [Nullable]
    FEmail: NullableString;

    [JSONName('address_phone_number')]
    [Nullable]
    FPhoneNumber: NullableString;

    [JSONName('address_city')]
    [Nullable]
    FCity: NullableString;

    [JSONName('address_state_id')]
    [Nullable]
    FStateId: NullableString;

    [JSONName('address_country_id')]
    [Nullable]
    FCountryId: NullableString;

    [JSONName('address_zip')]
    [Nullable]
    FZip: NullableString;

    [JSONName('cached_lat')]
    FLatitude: Double;

    [JSONName('cached_lng')]
    FLongitude: Double;

    [JSONName('curbside_lat')]
    [Nullable]
    FCurbsideLatitude: NullableDouble;

    [JSONName('curbside_lng')]
    [Nullable]
    FCurbsideLongitude: NullableDouble;

    [JSONName('color')]
    [Nullable]
    FColor: NullableString;

    [JSONName('address_icon')]
    [Nullable]
    FAddressIcon: NullableString;

    [JSONName('address_custom_data')]
    [NullableObject(TDictionaryStringIntermediateObject)]
    FCustomData: NullableObject;

    function GetCustomData: TDictionaryStringIntermediateObject;
  public
    /// <remarks>
    ///  Constructor with 0-arguments must be and be public.
    ///  For JSON-deserialization.
    /// </remarks>
    constructor Create; overload; override;
    constructor Create(Address: String; Latitude, Longitude: Double); reintroduce; overload;
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; override;

    /// <summary>
    ///  Address unique ID
    /// </summary>
    property Id: NullableInteger read FId write FId;

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
    ///  The route Address Line 2 which is not used for geocoding
    /// </summary>
    property Address2: NullableString read FAddress2 write FAddress2;

    /// <summary>
    ///  The first name of the receiving address
    /// </summary>
    property FirstName: NullableString read FFirstName write FFirstName;

    /// <summary>
    ///  The last name of the receiving party
    /// </summary>
    property LastName: NullableString read FLastName write FLastName;

    /// <summary>
    ///  Address email
    /// </summary>
    property Email: NullableString read FEmail write FEmail;

    /// <summary>
    ///  The phone number for the address
    /// </summary>
    property PhoneNumber: NullableString read FPhoneNumber write FPhoneNumber;

    /// <summary>
    ///  The city the address is located in
    /// </summary>
    property City: NullableString read FCity write FCity;

    /// <summary>
    ///  The state the address is located in
    /// </summary>
    property StateId: NullableString read FStateId write FStateId;

    /// <summary>
    ///  The country the address is located in
    /// </summary>
    property CountryId: NullableString read FCountryId write FCountryId;

    /// <summary>
    ///  The zip code the address is located in
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
    ///  Curbside latitude
    /// </summary>
    property CurbsideLatitude: NullableDouble read FCurbsideLatitude write FCurbsideLatitude;

    /// <summary>
    ///  Curbside longitude
    /// </summary>
    property CurbsideLongitude: NullableDouble read FCurbsideLongitude write FCurbsideLongitude;

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
    property CustomData: TDictionaryStringIntermediateObject read GetCustomData;
    procedure AddCustomData(Key: String; Value: String);
  end;

  TAddressBookContactArray = TArray<TAddressBookContact>;


implementation

{ TAddressBookContact }

constructor TAddressBookContact.Create;
begin
  Inherited Create;

  FId := NullableInteger.Null;
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
  FCurbsideLatitude := NullableDouble.Null;
  FCurbsideLongitude := NullableDouble.Null;

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

destructor TAddressBookContact.Destroy;
begin
  if FCustomData.IsNotNull then
    FCustomData.Free;
  inherited;
end;

function TAddressBookContact.Equals(Obj: TObject): Boolean;
var
  Other: TAddressBookContact;
begin
  Result := False;

  if not (Obj is TAddressBookContact) then
    Exit;

  Other := TAddressBookContact(Obj);

  Result :=
    (FId = Other.FId) and
    (FAddressGroup = Other.FAddressGroup) and
    (FAlias = Other.FAlias) and
    (FAddress = Other.FAddress) and
    (FAddress2 = Other.FAddress2) and
    (FFirstName = Other.FFirstName) and
    (FLastName = Other.FLastName) and
    (FEmail = Other.FEmail) and
    (FPhoneNumber = Other.FPhoneNumber) and
    (FCity = Other.FCity) and
    (FStateId = Other.FStateId) and
    (FCountryId = Other.FCountryId) and
    (FZip = Other.FZip) and
    (FLatitude = Other.FLatitude) and
    (FLongitude = Other.FLongitude) and
    (FCurbsideLatitude = Other.FCurbsideLatitude) and
    (FCurbsideLongitude = Other.FCurbsideLongitude) and
    (FColor = Other.FColor) and
    (FAddressIcon = Other.FAddressIcon) and
    (FCustomData = Other.FCustomData);
end;

function TAddressBookContact.GetCustomData: TDictionaryStringIntermediateObject;
begin
  if (FCustomData.IsNull) then
    Result := nil
  else
    Result := FCustomData.Value as TDictionaryStringIntermediateObject;
end;

end.
