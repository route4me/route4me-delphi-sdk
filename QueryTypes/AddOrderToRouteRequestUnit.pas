unit AddOrderToRouteRequestUnit;

interface

uses
  REST.Json.Types, SysUtils,
  JSONNullableAttributeUnit, HttpQueryMemberAttributeUnit,
  GenericParametersUnit, RouteParametersUnit, AddressUnit,
  NullableBasicTypesUnit, EnumsUnit;

type
  TOrderedAddress = class(TAddress)
  private
    [JSONName('order_id')]
    [Nullable]
    FOrderId: NullableString;
  public
    constructor Create(); override;

    property OrderId: NullableString read FOrderId write FOrderId;
  end;
  TOrderedAddressArray = TArray<TOrderedAddress>;

  TAddOrderToRouteRequest = class(TGenericParameters)
  private
    [JSONMarshalled(False)]
    [HttpQueryMember('route_id')]
    [Nullable]
    FRouteId: NullableString;

    [JSONMarshalled(False)]
    [HttpQueryMember('redirect')]
    [Nullable]
    FRedirect: NullableBoolean;

    [JSONNameAttribute('addresses')]
    [NullableArray(TOrderedAddress)]
    FAddresses: TOrderedAddressArray;

    [JSONNameAttribute('parameters')]
    [NullableObject(TRouteParameters)]
    FParameters: NullableObject;

    [JSONMarshalled(False)]
    FIsOwned: boolean;

    function GetAddress(AddressString: String; Addresses: TOrderedAddressArray): TOrderedAddress;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; override;

    /// <summary>
    ///  Route Parameters.
    /// </summary>
    property Parameters: NullableObject read FParameters write FParameters;

    /// <summary>
    ///  Route Addresses.
    /// </summary>
    property Addresses: TOrderedAddressArray read FAddresses;
    procedure AddAddress(Address: TOrderedAddress);

    property RouteId: NullableString read FRouteId write FRouteId;
    property Redirect: NullableBoolean read FRedirect write FRedirect;

    property IsOwned: boolean read FIsOwned write FIsOwned;
  end;

implementation

{ TAddOrderToRouteRequest }

procedure TAddOrderToRouteRequest.AddAddress(Address: TOrderedAddress);
begin
  SetLength(FAddresses, Length(FAddresses) + 1);
  FAddresses[High(FAddresses)] := Address;
end;

constructor TAddOrderToRouteRequest.Create;
begin
  Inherited;

  FIsOwned := True;

  SetLength(FAddresses, 0);
  FParameters := NullableObject.Null;

  FRouteId := NullableString.Null;
  FRedirect := NullableBoolean.Null;
end;

destructor TAddOrderToRouteRequest.Destroy;
var
  i: integer;
begin
  if FIsOwned then
  begin
    for i := Length(FAddresses) - 1 downto 0 do
      FreeAndNil(FAddresses[i]);

    FParameters.Free;
  end;

  inherited;
end;

function TAddOrderToRouteRequest.Equals(Obj: TObject): Boolean;
var
  Other: TAddOrderToRouteRequest;
  Address, OtherAddress: TAddress;
  AddressEquals: boolean;
begin
  Result := False;

  if not (Obj is TAddOrderToRouteRequest) then
    Exit;

  Other := TAddOrderToRouteRequest(Obj);

  Result :=
    (FParameters = Other.FParameters) and
    (FRouteId = Other.FRouteId) and
    (FRedirect = Other.FRedirect);

  if (not Result) then
    Exit;

  Result := False;

  if (Length(FAddresses) <> Length(Other.Addresses)) then
    Exit;

  AddressEquals := True;
  for Address in FAddresses do
  begin
    OtherAddress := GetAddress(Address.AddressString, Other.Addresses);
    if (OtherAddress = nil) then
      Exit;

    AddressEquals := AddressEquals and Address.Equals(OtherAddress);
    if not AddressEquals then
      Break;
  end;
end;

function TAddOrderToRouteRequest.GetAddress(AddressString: String;
  Addresses: TOrderedAddressArray): TOrderedAddress;
var
  Address: TOrderedAddress;
begin
  Result := nil;
  for Address in Addresses do
    if (Address.AddressString = AddressString) then
      Exit(Address);
end;

{ TOrderedAddress }

constructor TOrderedAddress.Create();
begin
  Inherited Create;

  FOrderId := NullableString.Null;
end;

end.
