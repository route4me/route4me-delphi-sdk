unit RegisterAccountUnit;

interface

uses SysUtils, BaseExampleUnit, EnumsUnit;

type
  TRegisterAccount = class(TBaseExample)
  public
    procedure Execute(Plan, Industry, FirstName, LastName, Email: String;
      Terms: boolean; DeviceType: TDeviceType;
      Password, PasswordConfirmation: String);
  end;

implementation

uses NullableBasicTypesUnit;

procedure TRegisterAccount.Execute(
  Plan, Industry, FirstName, LastName, Email: String;
  Terms: boolean; DeviceType: TDeviceType;
  Password, PasswordConfirmation: String);
var
  ErrorString: String;
  MemberId: NullableInteger;
begin
  MemberId := Route4MeManager.User.RegisterAccount(Plan, Industry, FirstName, LastName,
    Email, Terms, DeviceType, Password, PasswordConfirmation, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
  begin
    WriteLn(Format('Account registered successfully, MemberId = %d', [MemberId.Value]));
    WriteLn('');
  end
  else
    WriteLn(Format('RegisterAccount error: "%s"', [ErrorString]));
end;

end.
