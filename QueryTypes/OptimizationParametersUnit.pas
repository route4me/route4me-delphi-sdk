unit OptimizationParametersUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections, SysUtils,
  JSONNullableAttributeUnit, HttpQueryMemberAttributeUnit,
  GenericParametersUnit, RouteParametersUnit, AddressUnit,
  NullableBasicTypesUnit, EnumsUnit;

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

    [JSONNameAttribute('parameters')]
//    [NullableObject(TRouteParameters)]
    FParameters: TRouteParameters;
//    FParameters: NullableObject; todo: Если в NullableObject завернуть класс с Nullable-полями, то он не десериализуется

    [JSONNameAttribute('addresses')]
//    [NullableObject(TAddressesListClass)]
//    [Nullable]
    [NullableArray(TAddress)]
    FAddresses: TAddressesArray;//NullableObject;//TAddressesArray;

    [JSONNameAttribute('directions')]
    [Nullable]
    FDirections: NullableInteger;

    [JSONNameAttribute('format')]
    [Nullable]
    FFormat: NullableString;

    [JSONNameAttribute('route_path_output')]
    [Nullable]
    FRoutePathOutput: NullableString;

    [JSONNameAttribute('optimized_callback_url')]
    [Nullable]
    FOptimizedCallbackUrl: NullableString;

//    function GetAddresses: TAddressesList;
    function GetAddress(AddressString: String; Addresses: TAddressesArray): TAddress;

    function GetFormatEnum: TOptimizationParametersFormat;
    procedure SetFormatEnum(const Value: TOptimizationParametersFormat);
    function GetRoutePathOutput: TRoutePathOutput;
    procedure SetRoutePathOutput(const Value: TRoutePathOutput);
  public
    constructor Create; override;
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; override;

    /// <summary>
    ///  Optimization Problem ID
    /// </summary>
    property OptimizationProblemID: NullableString read FOptimizationProblemID write FOptimizationProblemID;

    /// <summary>
    ///  If 1, reoptimize, if 0 - no
    /// </summary>
    property ReOptimize: NullableBoolean read FReOptimize write FReOptimize;

    /// <summary>
    ///  Route Parameters. POST data
    /// </summary>
    property Parameters: {NullableObject}TRouteParameters read FParameters write FParameters;

    /// <summary>
    ///  Route Addresses. POST data
    /// </summary>
//    property Addresses: TAddressesList read GetAddresses;
    property Addresses: TAddressesArray read FAddresses;
    procedure AddAddress(Address: TAddress);

    /// <summary>
    ///  A flag to enable or disable driving and turn-by-turn directions being returned. Not returning them is more efficient for the database. Query string (GET fields).
    /// </summary>
    property Directions: NullableInteger read FDirections write FDirections;

    /// <summary>
    ///  The format in which to return the route data. This parameter is ignored for now, as only JSON is supported. Query string (GET fields).
    /// </summary>
    property FormatEnum: TOptimizationParametersFormat read GetFormatEnum write SetFormatEnum;

    /// <summary>
    ///  Return the path points when returning the newly created route... Query string (GET fields).
    /// </summary>
    property RoutePathOutput: TRoutePathOutput read GetRoutePathOutput write SetRoutePathOutput;

    /// <summary>
    ///  A URL that gets called when the optimization is solved, or if there is an error.
    ///  The callback is called with a POST request.
    ///  The POST data sent is:
    ///  - timestamp (Seconds):
    ///  - Server timestamp of request sent optimization_problem_id (Hash String):
    ///  - ID of the optimization state (Small Int).
    ///  The state can be one of the values:
    ///  4 = OPTIMIZATION_STATE_OPTIMIZED, which means the optimization was successful;
    ///  or 5 = OPTIMIZATION_STATE_ERROR, which means there was an error solving the optimization.. Query string (GET fields).
    /// </summary>
    property OptimizedCallbackUrl: NullableString read FOptimizedCallbackUrl write FOptimizedCallbackUrl;

  end;

implementation

{ TOptimizationParameters }

procedure TOptimizationParameters.AddAddress(Address: TAddress);
begin
{  if (FAddresses.IsNull) then
    FAddresses := TAddressesList.Create();
  (FAddresses.Value as TAddressesList).Add(Address);}
  SetLength(FAddresses, Length(FAddresses) + 1);
  FAddresses[High(FAddresses)] := Address;
end;

constructor TOptimizationParameters.Create;
begin
  Inherited;

  FOptimizationProblemID := NullableString.Null;
  FReOptimize := NullableBoolean.Null;
  FDirections := NullableInteger.Null;
  FFormat := NullableString.Null;
  FRoutePathOutput := NullableString.Null;
  FOptimizedCallbackUrl := NullableString.Null;
//  FAddresses := NullableObject.Null;
  SetLength(FAddresses, 0);

  FParameters := nil;
//  FParameters := NullableObject.Null;
end;

destructor TOptimizationParameters.Destroy;
var
  i: integer;
begin
{  for i := Addresses.Count - 1 downto 0 do
    Addresses[i].Free;
  FAddresses.Free;}
  for i := Length(FAddresses) - 1 downto 0 do
    FAddresses[i].Free;

  FreeAndNil(FParameters);

  inherited;
end;

function TOptimizationParameters.Equals(Obj: TObject): Boolean;
var
  Other: TOptimizationParameters;
  ParametersEquals: boolean;
//  SortedAddresses1, SortedAddresses2: TAddressesArray;
//  i: integer;
  Address, OtherAddress: TAddress;
  AddressEquals: boolean;
begin
  Result := False;

  if not (Obj is TOptimizationParameters) then
    Exit;

  Other := TOptimizationParameters(Obj);

  Result :=
    (OptimizationProblemID = Other.OptimizationProblemID) and
    (ReOptimize = Other.ReOptimize) and
    (Directions = Other.Directions) and
    (FormatEnum = Other.FormatEnum) and
    (RoutePathOutput = Other.RoutePathOutput) and
    (OptimizedCallbackUrl = Other.OptimizedCallbackUrl);

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

{  if (FAddresses.IsNull and Other.FAddresses.IsNotNull) or
    (FAddresses.IsNotNull and Other.FAddresses.IsNull) then
    Exit;

  if (Addresses <> nil) then
  begin
    if (Addresses.Count <> Other.Addresses.Count) then
      Exit;

    SortedAddresses1 := AddressUnit.SortAddresses(Addresses.ToArray);
    SortedAddresses2 := AddressUnit.SortAddresses(Other.Addresses.ToArray);
    for i := 0 to Length(SortedAddresses1) - 1 do
      if (not SortedAddresses1[i].Equals(SortedAddresses2[i])) then
        Exit;
  end;}

//  ParametersEquals := (FParameters = Other.Parameters);
  if ((FParameters <> nil) and (Other.Parameters = nil)) or
    ((FParameters = nil) and (Other.Parameters <> nil)) then
    Exit;

  if ((FParameters <> nil) and (Other.Parameters <> nil)) then
    ParametersEquals := FParameters.Equals(Other.Parameters)
  else
    ParametersEquals := True;

  Result := ParametersEquals;
end;

{function TOptimizationParameters.GetAddresses: TAddressesList;
begin
  if FAddresses.IsNull then
    Result := nil
  else
    Result := FAddresses.Value as TAddressesList;
end;}

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

function TOptimizationParameters.GetFormatEnum: TOptimizationParametersFormat;
var
  FormatEnum: TOptimizationParametersFormat;
begin
  if FFormat.IsNull then
    Exit(TOptimizationParametersFormat.opUndefined);

  for FormatEnum := Low(TOptimizationParametersFormat) to High(TOptimizationParametersFormat) do
    if (FFormat = TOptimizationParametersFormatDescription[FormatEnum]) then
      Exit(FormatEnum);
end;

function TOptimizationParameters.GetRoutePathOutput: TRoutePathOutput;
var
  RoutePathOutput: TRoutePathOutput;
begin
  if FRoutePathOutput.IsNull then
    Exit(TRoutePathOutput.rpoUndefined);

  for RoutePathOutput := Low(TRoutePathOutput) to High(TRoutePathOutput) do
    if (FRoutePathOutput = TRoutePathOutputDescription[RoutePathOutput]) then
      Exit(RoutePathOutput);
end;

procedure TOptimizationParameters.SetFormatEnum(
  const Value: TOptimizationParametersFormat);
begin
  FFormat := TOptimizationParametersFormatDescription[Value];
end;

procedure TOptimizationParameters.SetRoutePathOutput(
  const Value: TRoutePathOutput);
begin
  FRoutePathOutput := TRoutePathOutputDescription[Value];
end;

end.
