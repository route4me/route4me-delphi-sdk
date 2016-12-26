unit BulkGeocodingRequestUnit;

interface

uses
  REST.Json.Types, SysUtils,
  JSONNullableAttributeUnit, HttpQueryMemberAttributeUnit,
  GenericParametersUnit, NullableBasicTypesUnit;

type
  TAddressInfo = class(TGenericParameters)
  private
    [JSONName('address')]
    [Nullable]
    FAddress: NullableString;

    [JSONName('email')]
    [Nullable]
    FEmail: NullableString;

    [JSONName('username')]
    [Nullable]
    FUsername: NullableString;

    [JSONName('web-site')]
    [Nullable]
    FWebSite: NullableString;

    [JSONName('phone')]
    [Nullable]
    FPhone: NullableString;

    [JSONName('first_name')]
    [Nullable]
    FFirstName: NullableString;

    [JSONName('last_name')]
    [Nullable]
    FLastName: NullableString;
  public
    constructor Create(Address, EMail, Username, Website, Phone, FirstName, LastName: String); reintroduce;

    property Address: NullableString read FAddress write FAddress;
    property Email: NullableString read FEmail write FEmail;
    property Username: NullableString read FUsername write FUsername;
    property WebSite: NullableString read FWebSite write FWebSite;
    property Phone: NullableString read FPhone write FPhone;
    property FirstName: NullableString read FFirstName write FFirstName;
    property LastName: NullableString read FLastName write FLastName;
  end;
  TAddressInfoArray = TArray<TAddressInfo>;

  TBulkGeocodingRequest = class(TGenericParameters)
  private
    [JSONName('rows')]
    FAddresses: TArray<TAddressInfo>;
  public
    constructor Create(); override;
    destructor Destroy; override;

    procedure AddAddress(Address: TAddressInfo);

    property Addresses: TArray<TAddressInfo> read FAddresses;
  end;

implementation

procedure TBulkGeocodingRequest.AddAddress(Address: TAddressInfo);
begin
  SetLength(FAddresses, Length(FAddresses) + 1);
  FAddresses[High(FAddresses)] := Address;
end;

constructor TBulkGeocodingRequest.Create();
begin
  inherited Create;

  SetLength(FAddresses, 0);
end;

destructor TBulkGeocodingRequest.Destroy;
var
  i: integer;
begin
  for i := Length(FAddresses) - 1 downto 0 do
    FreeAndNil(FAddresses[i]);

  inherited;
end;

{ TAddressInfo }

constructor TAddressInfo.Create(
  Address, EMail, Username, Website, Phone, FirstName, LastName: String);
begin
  FAddress := Address;
  FEmail := EMail;
  FUsername := Username;
  FWebSite := Website;
  FPhone := Phone;
  FFirstName := FirstName;
  FLastName := LastName;
end;

end.
