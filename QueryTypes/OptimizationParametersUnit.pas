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

    [JSONNameAttribute('addresses')]
    FAddresses: TAddressesArray;

    [JSONNameAttribute('parameters')]
//    [NullableObject(TRouteParameters)]
    FParameters: TRouteParameters;
//    FParameters: NullableObject; todo: Если в NullableObject завернуть класс с Nullable-полями, то он не десериализуется

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

    function GetAddress(AddressString: String; Addresses: TAddressesArray): TAddress;

    function GetFormatEnum: TOptimizationParametersFormat;
    procedure SetFormatEnum(const Value: TOptimizationParametersFormat);
    function GetRoutePathOutput: TRoutePathOutput;
    procedure SetRoutePathOutput(const Value: TRoutePathOutput);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure AddAddress(Address: TAddress);

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
    property Addresses: TAddressesArray read FAddresses;

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

  SetLength(FAddresses, 0);
  FParameters := nil;
//  FParameters := NullableObject.Null;
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

  Result :=
    (OptimizationProblemID = Other.OptimizationProblemID) and
    (ReOptimize = Other.ReOptimize) and
    (Directions = Other.Directions) and
    (FormatEnum = Other.FormatEnum) and
    (RoutePathOutput = Other.RoutePathOutput) and
    (OptimizedCallbackUrl = Other.OptimizedCallbackUrl);

  if (not Result) then
    Exit;

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

//  ParametersEquals := (FParameters = Other.Parameters);
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
