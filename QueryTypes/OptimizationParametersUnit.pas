unit OptimizationParametersUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections,
  JSONNullableAttributeUnit,
  GenericParametersUnit, RouteParametersUnit, AddressUnit,
  NullableBasicTypesUnit;

type
  TOptimizationParameters = class(TGenericParameters)
  private
    [JSONMarshalled(False)]
    FOptimizationProblemID: String;

    [JSONMarshalled(False)]
    FReOptimize: boolean;

    [JSONMarshalled(False)]
    FShowDirections: boolean;

    [JSONNameAttribute('addresses')]
    FAddresses: TArray<TAddress>;

    [JSONNameAttribute('parameters')]
    FParameters: TRouteParameters;

    function GetAddress(AddressString: String; Addresses: TArray<TAddress>): TAddress;
  public
    constructor Create;

    function Equals(Obj: TObject): Boolean; override;

//    [HttpQueryMemberAttribute(Name = 'optimization_problem_id', EmitDefaultValue = false)]
    property OptimizationProblemID: String read FOptimizationProblemID write FOptimizationProblemID;

//    [HttpQueryMemberAttribute(Name = 'reoptimize', EmitDefaultValue = false)]
    property ReOptimize: boolean read FReOptimize write FReOptimize;

//    [HttpQueryMemberAttribute(Name = 'show_directions', EmitDefaultValue = false)]
    property ShowDirections: boolean read FShowDirections write FShowDirections;

//    [DataMember(Name = 'parameters', EmitDefaultValue = false)]
    property Parameters: TRouteParameters read FParameters write FParameters;

//    [DataMember(Name = 'addresses', EmitDefaultValue = false)]
    property Addresses: TArray<TAddress> read FAddresses write FAddresses;
  end;

implementation

{ TOptimizationParameters }

constructor TOptimizationParameters.Create;
begin
  Inherited;

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
