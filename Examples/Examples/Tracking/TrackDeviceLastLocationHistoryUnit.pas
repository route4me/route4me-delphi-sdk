unit TrackDeviceLastLocationHistoryUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TTrackDeviceLastLocationHistory = class(TBaseExample)
  public
    procedure Execute(RouteId: String);
  end;

implementation

uses GPSParametersUnit, EnumsUnit, GenericParametersUnit, DataObjectUnit,
  TrackingHistoryUnit;

procedure TTrackDeviceLastLocationHistory.Execute(RouteId: String);
var
  ErrorString: String;
  Parameters: TGPSParameters;
  GenericParameters: TGenericParameters;
  Response: String;
  DataObject: TDataObject;
  HistoryStep: TTrackingHistory;
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

    if (ErrorString <> EmptyStr) then
    begin
      WriteLn(Format('SetGps error: "%s"', [ErrorString]));
      Exit;
    end;

    WriteLn(Format('SetGps response: %s', [Response]));

    GenericParameters := TGenericParameters.Create();
    try
      GenericParameters.AddParameter('route_id', RouteId);
      GenericParameters.AddParameter('device_tracking_history', '1');

      DataObject := Route4MeManager.Tracking.GetLastLocation(GenericParameters, ErrorString);
      try
        WriteLn('');

        if (DataObject <> nil) then
        begin
          WriteLn('TrackDeviceLastLocationHistory executed successfully');
          WriteLn('');

          WriteLn(Format('Optimization Problem ID: %s', [DataObject.OptimizationProblemId]));
            WriteLn(Format('State: %s',
              [TOptimizationDescription[TOptimizationState(DataObject.State)]]));
          WriteLn('');

          for HistoryStep in DataObject.TrackingHistories do
          begin
            WriteLn(Format('Speed: %f', [HistoryStep.Speed.Value]));
            WriteLn(Format('Longitude: %f', [HistoryStep.Longitude.Value]));
            WriteLn(Format('Latitude: %f', [HistoryStep.Latitude.Value]));
            WriteLn(Format('Time Stamp: %s', [HistoryStep.TimeStamp.Value]));
            WriteLn('');
          end;
        end
        else
          WriteLn(Format('TrackDeviceLastLocationHistory error: "%s"', [ErrorString]));
      finally
        FreeAndNil(DataObject);
      end;
    finally
      FreeAndNil(GenericParameters);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
