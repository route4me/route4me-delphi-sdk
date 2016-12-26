unit TrackingActionsUnit;

interface

uses
  SysUtils, BaseActionUnit, DataObjectUnit, GPSParametersUnit, EnumsUnit,
  GenericParametersUnit, TrackingHistoryUnit, TrackingHistoryResponseUnit,
  TrackingDataUnit;

type
  TTrackingActions = class(TBaseAction)
  public
    function GetLastLocation(RouteId: String; out ErrorString: String): TDataObjectRoute;
    function GetLocationHistory(RouteId: String; Period: TPeriod;
      LastPositionOnly: boolean; out ErrorString: String): TTrackingHistoryResponse; overload;
    function GetLocationHistory(RouteId: String; StartDate, EndDate: TDateTime;
      LastPositionOnly: boolean; out ErrorString: String): TTrackingHistoryResponse; overload;

    function GetAssetTrackingData(TrackingNumber: String;
      out ErrorString: String): TTrackingData;

    procedure SetGPS(Parameters: TGPSParameters; out ErrorString: String);
  end;

implementation

uses
  SettingsUnit, StatusResponseUnit, TrackingHistoryRequestUnit, UtilsUnit;

function TTrackingActions.GetAssetTrackingData(TrackingNumber: String;
  out ErrorString: String): TTrackingData;
var
  Parameters: TGenericParameters;
begin
  Parameters := TGenericParameters.Create;
  try
    Parameters.AddParameter('tracking', TrackingNumber);

    Result := FConnection.Get(TSettings.EndPoints.TrackingStatus, Parameters,
      TTrackingData, ErrorString) as TTrackingData;
  finally
    FreeAndNil(Parameters);
  end;
end;

function TTrackingActions.GetLastLocation(RouteId: String;
  out ErrorString: String): TDataObjectRoute;
var
  Parameters: TGenericParameters;
begin
  Parameters := TGenericParameters.Create;
  try
    Parameters.AddParameter('route_id', RouteId);
    Parameters.AddParameter('device_tracking_history', '1');

    Result := FConnection.Get(TSettings.EndPoints.Route, Parameters,
      TDataObjectRoute, ErrorString) as TDataObjectRoute;
  finally
    FreeAndNil(Parameters);
  end;
end;

function TTrackingActions.GetLocationHistory(RouteId: String; Period: TPeriod;
  LastPositionOnly: boolean; out ErrorString: String): TTrackingHistoryResponse;
var
  Parameters: TTrackingHistoryRequest;
begin
  Parameters := TTrackingHistoryRequest.Create(RouteId);
  try
    Parameters.TimePeriod := TPeriodDescription[Period];
    Parameters.LastPosition := LastPositionOnly;
    Parameters.Format := 'json';

    Result := FConnection.Get(TSettings.EndPoints.GetDeviceLocation, Parameters,
      TTrackingHistoryResponse, ErrorString) as TTrackingHistoryResponse;
  finally
    FreeAndNil(Parameters);
  end;
end;

function TTrackingActions.GetLocationHistory(RouteId: String; StartDate,
  EndDate: TDateTime; LastPositionOnly: boolean;
  out ErrorString: String): TTrackingHistoryResponse;
var
  Parameters: TTrackingHistoryRequest;
begin
  Parameters := TTrackingHistoryRequest.Create(RouteId);
  try
    Parameters.TimePeriod := 'custom';
    Parameters.LastPosition := LastPositionOnly;
    Parameters.StartDate := IntToStr(TUtils.ConvertToUnixTimestamp(StartDate));
    Parameters.EndDate := IntToStr(TUtils.ConvertToUnixTimestamp(EndDate));
    Parameters.Format := 'json';

    Result := FConnection.Get(TSettings.EndPoints.GetDeviceLocation, Parameters,
      TTrackingHistoryResponse, ErrorString) as TTrackingHistoryResponse;
  finally
    FreeAndNil(Parameters);
  end;
end;

procedure TTrackingActions.SetGPS(Parameters: TGPSParameters;
  out ErrorString: String);
var
  Response: TStatusResponse;
begin
  ErrorString := EmptyStr;

  Response := FConnection.Get(TSettings.EndPoints.SetGps, Parameters,
    TStatusResponse, ErrorString) as TStatusResponse;
  try
    if ((Response = nil) and (ErrorString = EmptyStr)) or
      ((Response <> nil) and (not Response.Status) and (ErrorString = EmptyStr)) then
      ErrorString := 'SetGPS fault';
  finally
    FreeAndNil(Response);
  end;
end;

end.
