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
  Response: String;
begin
  Parameters := TGPSParameters.Create();
  try
    Parameters.Format := TFormatDescription[TFormatEnum.Csv];
    Parameters.RouteId := RouteId;
    Parameters.Latitude := 33.14384;
    Parameters.Longitude := -83.22466;
    Parameters.Course := 1;
    Parameters.Speed := 120;
    Parameters.DeviceType := TDeviceTypeDescription[TDeviceType.IPhone];
    Parameters.MemberId := 1;
    Parameters.DeviceGuid := 'TEST_GPS';
    Parameters.DeviceTimestamp := '2014-06-14 17:43:35';

    Response := Route4MeManager.Tracking.SetGPS(Parameters, ErrorString);

    WriteLn('');

    if (ErrorString = EmptyStr) then
      WriteLn(Format('SetGps response: %s', [Response]))
    else
      WriteLn(Format('SetGps error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
