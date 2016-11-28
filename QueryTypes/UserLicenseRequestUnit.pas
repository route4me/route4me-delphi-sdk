unit UserLicenseRequestUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit, CommonTypesUnit, EnumsUnit;

type
  TUserLicenseRequest = class(TGenericParameters)
  private
    [JSONName('member_id')]
    FMemberId: integer;

    [JSONName('session_guid')]
    FSessionId: integer;

    [JSONName('device_id')]
    FDeviceId: String;

    [JSONName('device_type')]
    FDeviceType: String;

    [JSONName('subscription_name')]
    FSubscription: String;

    [JSONName('token')]
    FToken: String;

    [JSONName('payload')]
    FPayload: String;

    [JSONName('format')]
    FFormat: String;
  public
    constructor Create(MemberId, SessionId: integer; DeviceId: String;
      DeviceType: TDeviceType; Subscription: String; Token: String;
      Payload: String; Format: String = 'json'); reintroduce;
  end;

implementation

constructor TUserLicenseRequest.Create(MemberId, SessionId: integer;
  DeviceId: String; DeviceType: TDeviceType; Subscription: String;
  Token: String; Payload: String; Format: String = 'json');
begin
  Inherited Create;

  FMemberId := MemberId;
  FSessionId := SessionId;
  FDeviceId := DeviceId;
  FDeviceType := TDeviceTypeDescription[DeviceType];
  FSubscription := Subscription;
  FToken := Token;
  FPayload := Payload;
  FFormat := Format;
end;

end.
