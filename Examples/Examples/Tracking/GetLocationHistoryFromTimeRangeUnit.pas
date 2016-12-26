unit GetLocationHistoryFromTimeRangeUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetLocationHistoryFromTimeRange = class(TBaseExample)
  public
    procedure Execute(RouteId: String; StartDate, EndDate: TDateTime);
  end;

implementation

uses
  TrackingHistoryUnit, TrackingHistoryResponseUnit;

procedure TGetLocationHistoryFromTimeRange.Execute(
  RouteId: String; StartDate, EndDate: TDateTime);
var
  ErrorString: String;
  Response: TTrackingHistoryResponse;
  HistoryStep: TTrackingHistory;
  LastPositionOnly: boolean;
begin
  LastPositionOnly := False;
  Response := Route4MeManager.Tracking.GetLocationHistory(
    RouteId, StartDate, EndDate, LastPositionOnly, ErrorString);
  try
    WriteLn('');

    if (Response <> nil) then
    begin
      WriteLn('GetLocationHistoryFromTimeRange executed successfully');
      WriteLn('');

      WriteLn(Format('Total location history steps: %d', [Length(Response.TrackingHistories)]));
      WriteLn('');

      for HistoryStep in Response.TrackingHistories do
      begin
        WriteLn(Format('Speed: %f', [HistoryStep.Speed.Value]));
        WriteLn(Format('Longitude: %f', [HistoryStep.Longitude.Value]));
        WriteLn(Format('Latitude: %f', [HistoryStep.Latitude.Value]));
        WriteLn(Format('Time Stamp: %s', [HistoryStep.TimeStamp.Value]));
        WriteLn('');
      end;
    end
    else
      WriteLn(Format('GetLocationHistoryFromTimeRange error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Response);
  end;
end;

end.
