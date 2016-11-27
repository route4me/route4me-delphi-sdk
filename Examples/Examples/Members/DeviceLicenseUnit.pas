unit DeviceLicenseUnit;

interface

uses SysUtils, BaseExampleUnit, EnumsUnit;

type
  TDeviceLicense = class(TBaseExample)
  public
    function Execute(DeviceId: String; DeviceType: TDeviceType): boolean;
  end;

implementation

function TDeviceLicense.Execute(DeviceId: String; DeviceType: TDeviceType): boolean;
var
  ErrorString: String;
begin
  Result := Route4MeManager.User.DeviceLicense(DeviceId, DeviceType, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
  begin
    if Result then
      WriteLn('DeviceLicense successfully, device is licensed')
    else
      WriteLn('DeviceLicense successfully, device is not licensed');
    WriteLn('');
  end
  else
    WriteLn(Format('DeviceLicense error: "%s"', [ErrorString]));
end;

end.
