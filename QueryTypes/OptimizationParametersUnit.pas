unit OptimizationParametersUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections, SysUtils,
  JSONNullableAttributeUnit, HttpQueryMemberAttributeUnit,
  GenericParametersUnit, RouteParametersUnit, AddressUnit,
  NullableBasicTypesUnit;

type
  TOptimizationParameters = class(TGenericParameters)
  private
    [JSONMarshalled(False)]
    [HttpQueryMember('optimization_problem_id')]
    [Nullable]
    FOptimizationProblemID: NullableString;

    [JSONMarshalled(False)]
    [HttpQueryMember('reoptimize')]
    [Nullable]
    FReOptimize: NullableBoolean;

    [JSONMarshalled(False)]
    [HttpQueryMember('show_directions')]
    [Nullable]
    FShowDirections: NullableBoolean;

    [JSONNameAttribute('addresses')]
    FAddresses: TAddressesArray;

    [JSONNameAttribute('parameters')]
    FParameters: TRouteParameters;

    function GetAddress(AddressString: String; Addresses: TAddressesArray): TAddress;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure AddAddress(Address: TAddress);

    function Equals(Obj: TObject): Boolean; override;

    property OptimizationProblemID: NullableString read FOptimizationProblemID write FOptimizationProblemID;
    property ReOptimize: NullableBoolean read FReOptimize write FReOptimize;
    property ShowDirections: NullableBoolean read FShowDirections write FShowDirections;
    property Parameters: TRouteParameters read FParameters write FParameters;
    property Addresses: TAddressesArray read FAddresses;
  end;

implementation

{ TOptimizationParameters }

procedure TOptimizationParameters.AddAddress(Address: TAddress);
begin
  SetLength(FAddresses, Length(FAddresses) + 1);
  FAddresses[High(FAddresses)] := Address;
end;

constructor TOptimizationParameters.Create;
begin
  Inherited;

  FOptimizationProblemID := NullableString.Null;
  FReOptimize := NullableBoolean.Null;
  FShowDirections := NullableBoolean.Null;

  SetLength(FAddresses, 0);
  FParameters := nil;
end;

destructor TOptimizationParameters.Destroy;
var
  i: integer;
begin
  for i := Length(FAddresses) - 1 downto 0 do
    FreeAndNil(FAddresses[i]);
  FreeAndNil(FParameters);

  inherited;
end;

function TOptimizationParameters.Equals(Obj: TObject): Boolean;
var
  Other: TOptimizationParameters;
  Address, OtherAddress: TAddress;
  AddressEquals: boolean;
  ParametersEquals: boolean;
begin
  Result := False;

  if not (Obj is TOptimizationParameters) then
    Exit;

  Other := TOptimizationParameters(Obj);

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

  if ((FParameters <> nil) and (Other.Parameters = nil)) or
    ((FParameters = nil) and (Other.Parameters <> nil)) then
    Exit;

  if ((FParameters <> nil) and (Other.Parameters <> nil)) then
    ParametersEquals := FParameters.Equals(Other.Parameters)
  else
    ParametersEquals := True;

  Result := AddressEquals and ParametersEquals;
end;

function TOptimizationParameters.GetAddress(AddressString: String;
  Addresses: TAddressesArray): TAddress;
var
  Address: TAddress;
begin
  Result := nil;
  for Address in Addresses do
    if (Address.AddressString = AddressString) then
      Exit(Address);
end;

end.
