unit DeviceLicenseRequestUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit, CommonTypesUnit, EnumsUnit;

type
  TDeviceLicenseRequest = class(TGenericParameters)
  private
    [JSONName('device_id')]
    FDeviceId: String;

    [JSONName('device_type')]
    FDeviceType: String;

    [JSONName('format')]
    FFormat: String;
  public
    constructor Create(DeviceId: String; DeviceType: TDeviceType; Format: String); reintroduce;
  end;

implementation

{ TDeviceLicenseRequest }

constructor TDeviceLicenseRequest.Create(DeviceId: String;
  DeviceType: TDeviceType; Format: String);
begin
  Inherited Create;

  FDeviceId := DeviceId;
  FDeviceType := TDeviceTypeDescription[DeviceType];
  FFormat := Format;
end;

end.
