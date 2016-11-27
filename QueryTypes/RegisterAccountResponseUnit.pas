unit RegisterAccountResponseUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit, NullableBasicTypesUnit, JSONNullableAttributeUnit,
  CommonTypesUnit;

type
  TRegisterAccountResponse = class(TGenericParameters)
  private
    [JSONName('status')]
    FStatus: boolean;

    [JSONName('session_id')]
    [Nullable]
    FSessionId: NullableString;

    [JSONName('session_guid')]
    [Nullable]
    FSessionGuid: NullableString;

    [JSONName('member_id')]
    [Nullable]
    FMemberId: NullableInteger;

    [JSONName('api_key')]
    [Nullable]
    FApiKey: NullableString;

    [JSONName('account_type_id')]
    [Nullable]
    FAccountTypeId: NullableInteger;

    [JSONName('account_type_alias')]
    [Nullable]
    FAccountTypeAlias: NullableInteger;

    [JSONName('max_stops_per_route')]
    [Nullable]
    FMaxStopsPerRoute: NullableInteger;

    [JSONName('max_routes')]
    [Nullable]
    FMaxRoutes: NullableInteger;

    [JSONName('routes_planned')]
    [Nullable]
    FRoutesPlanned: NullableInteger;

    [JSONName('errors')]
    FErrors: TStringArray;
  public
    constructor Create; override;

    property Status: boolean read FStatus write FStatus;
    property SessionId: NullableString read FSessionId write FSessionId;
    property SessionGuid: NullableString read FSessionGuid write FSessionGuid;
    property MemberId: NullableInteger read FMemberId write FMemberId;
    property ApiKey: NullableString read FApiKey write FApiKey;
    property AccountTypeId: NullableInteger read FAccountTypeId write FAccountTypeId;
    property AccountTypeAlias: NullableInteger read FAccountTypeAlias write FAccountTypeAlias;
    property MaxStopsPerRoute: NullableInteger read FMaxStopsPerRoute write FMaxStopsPerRoute;
    property MaxRoutes: NullableInteger read FMaxRoutes write FMaxRoutes;
    property RoutesPlanned: NullableInteger read FRoutesPlanned write FRoutesPlanned;
    property Errors: TStringArray read FErrors write FErrors;
  end;

implementation

constructor TRegisterAccountResponse.Create;
begin
  inherited;

  SetLength(FErrors, 0);
  FSessionId := NullableString.Null;
  FSessionGuid := NullableString.Null;
  FMemberId := NullableInteger.Null;
  FApiKey := NullableString.Null;
  FAccountTypeId := NullableInteger.Null;
  FAccountTypeAlias := NullableInteger.Null;
  FMaxStopsPerRoute := NullableInteger.Null;
  FMaxRoutes := NullableInteger.Null;
  FRoutesPlanned := NullableInteger.Null;
end;

end.
