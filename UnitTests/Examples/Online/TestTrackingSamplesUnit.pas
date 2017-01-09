unit TestTrackingSamplesUnit;

interface

uses
  TestFramework, Classes, SysUtils,
  BaseTestOnlineExamplesUnit;

type
  TTestTrackingSamples = class(TTestOnlineExamples)
  private
    function GetRouteId: String;
  published
    procedure SetGPS;
    procedure TrackDeviceLastLocationHistory;
    procedure GetLocationHistory;
    procedure GetLocationHistoryFromTimeRange;
    procedure GetAssetTrackingData;
  end;

implementation

uses DateUtils, NullableBasicTypesUnit, GPSParametersUnit, EnumsUnit,
  DataObjectUnit, TrackingHistoryResponseUnit, TrackingDataUnit;

var
  FRouteId: NullableString;

procedure TTestTrackingSamples.GetAssetTrackingData;
var
  ErrorString: String;
  TrackingData: TTrackingData;
  TrackingNumber: String;
begin
  TrackingNumber := 'Q7G9P1L9';
  TrackingData := FRoute4MeManager.Tracking.GetAssetTrackingData(
    TrackingNumber, ErrorString);
  try
    CheckNotNull(TrackingData);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(TrackingData);
  end;

  TrackingNumber := 'qwe123';
  TrackingData := FRoute4MeManager.Tracking.GetAssetTrackingData(
    TrackingNumber, ErrorString);
  try
    CheckNull(TrackingData);
    CheckNotEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(TrackingData);
  end;
end;

procedure TTestTrackingSamples.GetLocationHistory;
const
  LastPositionOnly = False;
var
  ErrorString: String;
  Response: TTrackingHistoryResponse;
  RouteId: String;
  Period: TPeriod;
begin
  CheckTrue(FRouteId.IsNotNull);

  Period := pAllTime;

  Response := FRoute4MeManager.Tracking.GetLocationHistory(
    FRouteId, Period, LastPositionOnly, ErrorString);
  try
    CheckNotNull(Response);
    CheckTrue(Length(Response.TrackingHistories) > 0);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Response);
  end;

  RouteId := 'qwe';
  Response := FRoute4MeManager.Tracking.GetLocationHistory(
    RouteId, Period, LastPositionOnly, ErrorString);
  try
    CheckNull(Response);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Response);
  end;
end;

procedure TTestTrackingSamples.GetLocationHistoryFromTimeRange;
const
  LastPositionOnly = False;
var
  ErrorString: String;
  Response: TTrackingHistoryResponse;
  RouteId: String;
  StartDate, EndDate: TDateTime;
begin
  CheckTrue(FRouteId.IsNotNull);

  StartDate := EncodeDateTime(2016, 10, 20, 0, 0, 0, 0);
  EndDate := EncodeDateTime(2016, 10, 26, 23, 59, 59, 0);

  Response := FRoute4MeManager.Tracking.GetLocationHistory(
    FRouteId, StartDate, EndDate, LastPositionOnly, ErrorString);
  try
    CheckNotNull(Response);
    CheckTrue(Length(Response.TrackingHistories) > 0);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Response);
  end;

  RouteId := 'qwe';
  Response := FRoute4MeManager.Tracking.GetLocationHistory(
    RouteId, StartDate, EndDate, LastPositionOnly, ErrorString);
  try
    CheckNull(Response);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Response);
  end;

  StartDate := IncYear(Now);
  EndDate := IncDay(StartDate);
  Response := FRoute4MeManager.Tracking.GetLocationHistory(
    FRouteId, StartDate, EndDate, LastPositionOnly, ErrorString);
  try
    CheckNull(Response);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Response);
  end;
end;

function TTestTrackingSamples.GetRouteId: String;
var
  ErrorString: String;
  Routes: TDataObjectRouteList;
begin
  // todo 1: проинициализировать в реальное значение
  Routes := FRoute4MeManager.Route.GetList(1, 0, ErrorString);
  try
    CheckNotNull(Routes);
    CheckEquals(EmptyStr, ErrorString);
    CheckTrue(Routes.Count > 0);

    FRouteId := Routes[0].RouteId;
  finally
    FreeAndNil(Routes);
  end;
end;

procedure TTestTrackingSamples.SetGPS;
var
  ErrorString: String;
  Parameters: TGPSParameters;
  RouteId: String;
begin
  RouteId := '114B01238180A4227FD187E128C056F5';

  Parameters := TGPSParameters.Create;
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

    FRoute4MeManager.Tracking.SetGPS(Parameters, ErrorString);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Parameters);
  end;
end;

procedure TTestTrackingSamples.TrackDeviceLastLocationHistory;
var
  ErrorString: String;
  Route: TDataObjectRoute;
  RouteId: String;
begin
  RouteId := '814FB49CEA8188D134E9D4D4B8B0DAF7';

  Route := FRoute4MeManager.Tracking.GetLastLocation(RouteId, ErrorString);
  try
    CheckNotNull(Route);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Route);
  end;

  RouteId := 'qwe';
  Route := FRoute4MeManager.Tracking.GetLastLocation(RouteId, ErrorString);
  try
    CheckNull(Route);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Route);
  end;
end;

initialization
  RegisterTest('Examples\Online\Tracking\', TTestTrackingSamples.Suite);
end.
