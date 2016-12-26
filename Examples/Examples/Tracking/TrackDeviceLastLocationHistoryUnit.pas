unit TrackDeviceLastLocationHistoryUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TTrackDeviceLastLocationHistory = class(TBaseExample)
  public
    procedure Execute(RouteId: String);
  end;

implementation

uses EnumsUnit, DataObjectUnit,
  TrackingHistoryUnit;

procedure TTrackDeviceLastLocationHistory.Execute(RouteId: String);
var
  ErrorString: String;
  Route: TDataObjectRoute;
  HistoryStep: TTrackingHistory;
begin
  Route := Route4MeManager.Tracking.GetLastLocation(RouteId, ErrorString);
  try
    WriteLn('');

    if (Route <> nil) then
    begin
      WriteLn('TrackDeviceLastLocationHistory executed successfully');
      WriteLn('');

      WriteLn(Format('Optimization Problem ID: %s', [Route.OptimizationProblemId]));
        WriteLn(Format('State: %s',
          [TOptimizationDescription[TOptimizationState(Route.State)]]));
      WriteLn('');

      for HistoryStep in Route.TrackingHistories do
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
    FreeAndNil(Route);
  end;
end;

end.
