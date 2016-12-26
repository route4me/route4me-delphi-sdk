unit GetAssetTrackingDataUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetAssetTrackingData = class(TBaseExample)
  public
    procedure Execute(TrackingNumber: String);
  end;

implementation

uses
  TrackingHistoryUnit, TrackingHistoryResponseUnit, TrackingDataUnit;

procedure TGetAssetTrackingData.Execute(TrackingNumber: String);
var
  ErrorString: String;
  TrackingData: TTrackingData;
begin
  TrackingData := Route4MeManager.Tracking.GetAssetTrackingData(
    TrackingNumber, ErrorString);
  try
    WriteLn('');

    if (TrackingData <> nil) then
    begin
      WriteLn('GetAssetTrackingData executed successfully');
      WriteLn('');

      if TrackingData.Delivered.IsNotNull then
        if TrackingData.Delivered.Value then
          WriteLn('Package was delivered')
        else
          WriteLn('Package was not delivered');
    end
    else
      WriteLn(Format('GetAssetTrackingData error: "%s"', [ErrorString]));
  finally
    FreeAndNil(TrackingData);
  end;
end;

end.
