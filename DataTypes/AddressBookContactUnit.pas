unit AddressBookContactUnit;

interface

uses NullableBasicTypesUnit, GenericParametersUnit;

type

  TAddressBookContact = class(TGenericParameters)
  private
    [JSONName('address_id')]
    [JSONNullable]
    FRouteDestinationId: NullableString;

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
  public
    constructor Create;

    property RouteDestinationId: NullableString read FRouteDestinationId write FRouteDestinationId;
    property AddressGroup: NullableString read FAddressGroup write FAddressGroup;
    property Alias: NullableString read FAlias write FAlias;
    property Address: String read FAddress write FAddress;
    property Address2: NullableString read FAddress2 write FAddress2;
    property FirstName: NullableString read FFirstName write FFirstName;
    property LastName: NullableString read FLastName write FLastName;
    property Email: NullableString read FEmail write FEmail;
    property PhoneNumber: NullableString read FPhoneNumber write FPhoneNumber;
    property City: NullableString read FCity write FCity;
    property StateId: NullableString read FStateId write FStateId;
    property CountryId: NullableString read FCountryId write FCountryId;
    property Zip: NullableString read FZip write FZip;
    property Latitude: Double read FLatitude write FLatitude;
    property Longitude: Double read FLongitude write FLongitude;
    property Color: NullableString read FColor write FColor;
  end;

implementation

{ TAddressBookContact }

{ TAddressBookContact }

constructor TAddressBookContact.Create;
begin
    FRouteDestinationId := NullableString.Null;
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
end;

end.
