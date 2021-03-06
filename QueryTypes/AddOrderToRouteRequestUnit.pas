unit AddOrderToRouteRequestUnit;

interface

uses
  REST.Json.Types, SysUtils,
  JSONNullableAttributeUnit, HttpQueryMemberAttributeUnit,
  GenericParametersUnit, RouteParametersUnit, AddressUnit,
  NullableBasicTypesUnit, EnumsUnit;

type
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

  SetLength(FAddresses, 0);
  FParameters := NullableObject.Null;

  FRouteId := NullableString.Null;
  FRedirect := NullableBoolean.Null;
end;

destructor TAddOrderToRouteRequest.Destroy;
begin
// Request does not own objects

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

end.
