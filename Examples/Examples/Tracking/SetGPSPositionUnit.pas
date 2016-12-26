unit SetGPSPositionUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TSetGPSPosition = class(TBaseExample)
  public
    procedure Execute(RouteId: String);
  end;

implementation

uses GPSParametersUnit, EnumsUnit;

procedure TSetGPSPosition.Execute(RouteId: String);
var
  ErrorString: String;
  Parameters: TGPSParameters;
begin
  Parameters := TGPSParameters.Create();
  try
    Parameters.Format := TFormatDescription[TFormatEnum.Xml];
    Parameters.RouteId := RouteId;
    Parameters.Latitude := 55.6884868;
    Parameters.Longitude := 12.5366426;
    Parameters.Course := 70;
    Parameters.Speed := 60;
    Parameters.DeviceType := TDeviceTypeDescription[TDeviceType.AndroidPhone];
    Parameters.MemberId := 1;
    Parameters.DeviceGuid := 'HK5454H0K454564WWER445';

    Route4MeManager.Tracking.SetGPS(Parameters, ErrorString);

    WriteLn('');

    if (ErrorString = EmptyStr) then
      WriteLn('SetGps success')
    else
      WriteLn(Format('SetGps error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
