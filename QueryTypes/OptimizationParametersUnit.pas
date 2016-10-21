unit OptimizationParametersUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections,
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
    FAddresses: TArray<TAddress>;

    [JSONNameAttribute('parameters')]
    FParameters: TRouteParameters;

    function GetAddress(AddressString: String; Addresses: TArray<TAddress>): TAddress;
  public
    constructor Create;

    function Equals(Obj: TObject): Boolean; override;

    property OptimizationProblemID: NullableString read FOptimizationProblemID write FOptimizationProblemID;
    property ReOptimize: NullableBoolean read FReOptimize write FReOptimize;
    property ShowDirections: NullableBoolean read FShowDirections write FShowDirections;
    property Parameters: TRouteParameters read FParameters write FParameters;
    property Addresses: TArray<TAddress> read FAddresses write FAddresses;
  end;

implementation

{ TOptimizationParameters }

constructor TOptimizationParameters.Create;
begin
  Inherited;

  FOptimizationProblemID := NullableString.Null;
  FReOptimize := NullableBoolean.Null;
  FShowDirections := NullableBoolean.Null;

  SetLength(FAddresses, 0);
  FParameters := TRouteParameters.Create;
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
  Addresses: TArray<TAddress>): TAddress;
var
  Address: TAddress;
begin
  Result := nil;
  for Address in Addresses do
    if (Address.AddressString = AddressString) then
      Exit(Address);
end;

end.
