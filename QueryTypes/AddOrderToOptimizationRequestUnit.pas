unit AddOrderToOptimizationRequestUnit;

interface

uses
  REST.Json.Types, SysUtils,
  JSONNullableAttributeUnit, HttpQueryMemberAttributeUnit,
  GenericParametersUnit, RouteParametersUnit, AddressUnit,
  NullableBasicTypesUnit, EnumsUnit;

type
  TAddOrderToOptimizationRequest = class(TGenericParameters)
  private
    [JSONMarshalled(False)]
    [HttpQueryMember('optimization_problem_id')]
    [Nullable]
    FOptimizationProblemId: NullableString;

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

    property OptimizationProblemId: NullableString read FOptimizationProblemId write FOptimizationProblemId;
    property Redirect: NullableBoolean read FRedirect write FRedirect;
  end;

implementation

{ TAddOrderToOptimizationRequest }

procedure TAddOrderToOptimizationRequest.AddAddress(Address: TOrderedAddress);
begin
  SetLength(FAddresses, Length(FAddresses) + 1);
  FAddresses[High(FAddresses)] := Address;
end;

constructor TAddOrderToOptimizationRequest.Create;
begin
  Inherited;

  SetLength(FAddresses, 0);
  FParameters := NullableObject.Null;

  FOptimizationProblemId := NullableString.Null;
  FRedirect := NullableBoolean.Null;
end;

destructor TAddOrderToOptimizationRequest.Destroy;
begin
// Request does not own objects

  inherited;
end;

function TAddOrderToOptimizationRequest.Equals(Obj: TObject): Boolean;
var
  Other: TAddOrderToOptimizationRequest;
  Address, OtherAddress: TAddress;
  AddressEquals: boolean;
begin
  Result := False;

  if not (Obj is TAddOrderToOptimizationRequest) then
    Exit;

  Other := TAddOrderToOptimizationRequest(Obj);

  Result :=
    (FParameters = Other.FParameters) and
    (FOptimizationProblemId = Other.FOptimizationProblemId) and
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

function TAddOrderToOptimizationRequest.GetAddress(AddressString: String;
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
