unit UserLicenseUnit;

interface

uses SysUtils, BaseExampleUnit, EnumsUnit;

type
  TUserLicense = class(TBaseExample)
  public
    function Execute(MemberId, SessionId: integer;
      DeviceId: String; DeviceType: TDeviceType; Subscription: String;
      Token: String; Payload: String): boolean;
  end;

implementation

function TUserLicense.Execute(MemberId, SessionId: integer;
  DeviceId: String; DeviceType: TDeviceType; Subscription: String;
  Token: String; Payload: String): boolean;
var
  ErrorString: String;
begin
  Result := Route4MeManager.User.UserLicense(MemberId, SessionId, DeviceId,
    DeviceType, Subscription, Token, Payload, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
  begin
    if Result then
      WriteLn('UserLicense successfully, user is licensed')
    else
      WriteLn('UserLicense successfully, user is not licensed');
    WriteLn('');
  end
  else
    WriteLn(Format('UserLicense error: "%s"', [ErrorString]));
end;

end.
