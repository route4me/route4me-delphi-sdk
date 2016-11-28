unit AuthenticationResponseUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit, NullableBasicTypesUnit, JSONNullableAttributeUnit;

type
  TPartOfBadAuthenticationResponse = class(TGenericParameters)
  private
    [JSONName('status')]
    FStatus: boolean;

    [JSONName('session_guid')]
    [Nullable]
    FSessionGuid: NullableString;

    [JSONName('error')]
    [Nullable]
    FError: NullableString;
  public
    constructor Create; override;

    property Status: boolean read FStatus write FStatus;
    property SessionGuid: NullableString read FSessionGuid write FSessionGuid;
    property Error: NullableString read FError write FError;
  end;

  TBadAuthenticationResponse = class(TGenericParameters)
  //  {"session":{"status":false,"error":"Invalid credentials username or password"}}
  private
    [JSONName('session')]
    FSession: TPartOfBadAuthenticationResponse;

    function GetSessionGuid: NullableString;
    function GetStatus: boolean;
    function GetError: NullableString;
  public
    property Status: boolean read GetStatus;
    property SessionGuid: NullableString read GetSessionGuid;
    property Error: NullableString read GetError;
  end;

  TGoodAuthenticationResponse = class(TGenericParameters)
  {"status":true,"geocoding_service":"http:\/\/validator.route4me.com","session_id":4434046,"session_guid":"38a7a2109334400d672a68aefea83d31","member_id":"387130","api_key":"B46DD4D348B02C249556BF135E800B61","tracking_ttl":60,"geofence_polygon_shape":null,"geofence_polygon_size":0,"geofence_time_onsite_trigger_secs":0,"geofence_minimum_trigger_speed":20,"is_subscription_past_due":null,"visited_departed_enabled":"true","long_press_enabled":"true","account_type_id":"126","account_type_alias":"Enterprise","member_type":"SUB_ACCOUNT_DISPATCHER","max_stops_per_route":"1000","max_routes":"1000","routes_planned":null,"preferred_units":null,"preferred_language":"en","HIDE_ROUTED_ADDRESSES":"FALSE","HIDE_VISITED_ADDRESSES":"FALSE","HIDE_NONFUTURE_ROUTES":"FALSE","READONLY_USER":"FALSE","auto_logout_ts":-1,"last_known_member_payment_device":"web"}
  private
    [JSONName('status')]
    FStatus: boolean;

    [JSONName('session_id')]
    FSessionId: integer;
  public
    property Status: boolean read FStatus write FStatus;
    property SessionId: integer read FSessionId write FSessionId;
  end;

implementation

constructor TPartOfBadAuthenticationResponse.Create;
begin
  inherited;

  FError := NullableString.Null;
  FSessionGuid := NullableString.Null;
end;

function TBadAuthenticationResponse.GetError: NullableString;
begin
  Result := FSession.Error;
end;

function TBadAuthenticationResponse.GetSessionGuid: NullableString;
begin
  Result := FSession.SessionGuid;
end;

function TBadAuthenticationResponse.GetStatus: boolean;
begin
  Result := FSession.Status;
end;

end.
