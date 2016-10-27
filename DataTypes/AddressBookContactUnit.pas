unit AddressBookContactUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections, JSONNullableAttributeUnit,
  NullableBasicTypesUnit, GenericParametersUnit,
  JSONDictionaryInterceptorObjectUnit;

type

  TAddressBookContact = class(TGenericParameters)
  private
    [JSONName('address_id')]
    [Nullable]
    FId: NullableString;

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

    [JSONName('color')]
    [Nullable]
    FColor: NullableString;

    [JSONName('address_icon')]
    [Nullable]
    FAddressIcon: NullableString;

    [JSONName('address_custom_data')]
    [NullableObject(TDictionaryStringIntermediateObject)]
    FCustomData: NullableObject;

  public
    /// <remarks>
    ///  Constructor with 0-arguments must be and be public.
    ///  For JSON-deserialization.
    /// </remarks>
    constructor Create; overload; override;
    constructor Create(Address: String; Latitude, Longitude: Double); overload;
    destructor Destroy; override;

    procedure AddCustomData(Key: String; Value: String);

    function Equals(Obj: TObject): Boolean; override;

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

  TAddressBookContactList = TList<TAddressBookContact>;

implementation

{ TAddressBookContact }

constructor TAddressBookContact.Create;
begin
  Inherited Create;

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

  Result := (Id = Other.Id) and
    (AddressGroup = Other.AddressGroup) and
    (Alias = Other.Alias) and
    (Address = Other.Address) and
    (Address2 = Other.Address2) and
    (FirstName = Other.FirstName) and
    (LastName = Other.LastName) and
    (Email = Other.Email) and
    (PhoneNumber = Other.PhoneNumber) and
    (City = Other.City) and
    (StateId = Other.StateId) and
    (CountryId = Other.CountryId) and
    (Zip = Other.Zip) and
    (Latitude = Other.Latitude) and
    (Longitude = Other.Longitude) and
    (Color = Other.Color) and
    (AddressIcon = Other.AddressIcon) and
    (CustomData = Other.CustomData);
end;

end.
