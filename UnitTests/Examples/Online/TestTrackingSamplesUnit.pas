unit TestTrackingSamplesUnit;

interface

uses
  TestFramework, Classes, SysUtils,
  BaseTestOnlineExamplesUnit;

type
  TRec = record
    RouteId: String;
    MemberId: string;
  end;
  TRecs = array of TRec;

  TTestTrackingSamples = class(TTestOnlineExamples)
  private
{    procedure InitParameters(index: Integer; out RouteId: String; out MemberId: String);
    procedure SaveParameters();
    procedure LoadParameters();}
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

{var
  FRecs: TRecs;}

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
  RouteId := '814FB49CEA8188D134E9D4D4B8B0DAF7';
  Period := pAllTime;

  Response := FRoute4MeManager.Tracking.GetLocationHistory(
    RouteId, Period, LastPositionOnly, ErrorString);
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
    CheckNotEquals(EmptyStr, ErrorString);
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
  RouteId := '814FB49CEA8188D134E9D4D4B8B0DAF7';
  StartDate := EncodeDateTime(2016, 10, 20, 0, 0, 0, 0);
  EndDate := EncodeDateTime(2016, 10, 26, 23, 59, 59, 0);

  Response := FRoute4MeManager.Tracking.GetLocationHistory(
    RouteId, StartDate, EndDate, LastPositionOnly, ErrorString);
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
    CheckNotEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Response);
  end;

  StartDate := IncYear(Now);
  EndDate := IncDay(StartDate);
  Response := FRoute4MeManager.Tracking.GetLocationHistory(
    RouteId, StartDate, EndDate, LastPositionOnly, ErrorString);
  try
    CheckNull(Response);
    CheckNotEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Response);
  end;
end;

{procedure TTestTrackingSamples.InitParameters(index: Integer;
  out RouteId: String; out MemberId: String);
begin
  RouteId := FRecs[index].RouteId;
  MemberId := FRecs[index].MemberId;
end;

procedure TTestTrackingSamples.LoadParameters;
var
  st: TStringList;
  strings: TStringList;
  i: Integer;
begin
  if not FileExists('RouteIds.txt') then
    Exit;

  st := TStringList.Create;
  try
    st.LoadFromFile('RouteIds.txt');

    SetLength(FRecs, st.Count);
    for i := 0 to st.Count - 1 do
    begin
      strings := TStringList.Create;
      try
        ExtractStrings([';'], [' '], PWideChar(st[i]), strings);
        FRecs[i].RouteId := strings[0];
        FRecs[i].MemberId := strings[1];
      finally
        FreeAndNil(strings);
      end;
    end;
  finally
    FreeAndNil(st);
  end;
end;

procedure TTestTrackingSamples.SaveParameters;
const
  MaxPageCount = 5;
var
  ErrorString: String;
  Routes: TDataObjectRouteList;
  st: TStringList;
  offset: Integer;
  i: Integer;
begin
  st := TStringList.Create;

  begin
    Routes := FRoute4MeManager.Route.GetList(5000, 0, ErrorString);
    try
      CheckNotNull(Routes);
      CheckEquals(EmptyStr, ErrorString);
      CheckTrue(Routes.Count > 0);

      for i := 0 to Routes.Count - 1 do
        st.Add(Routes[i].RouteId + ';' + Routes[i].MemberId);
    finally
      FreeAndNil(Routes);
    end;
  end;

  st.SaveToFile('RouteIds.txt');
end;
                    }
procedure TTestTrackingSamples.SetGPS;
var
  ErrorString: String;
  Parameters: TGPSParameters;
  RouteId: String;
begin
  RouteId := '15D9B7219B4691E0CAB8937FAFF11E58';

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

    Parameters.MemberId := -1;
    FRoute4MeManager.Tracking.SetGPS(Parameters, ErrorString);
    CheckNotEquals(EmptyStr, ErrorString);
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
  RouteId := '15D9B7219B4691E0CAB8937FAFF11E58';

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
    CheckNotEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Route);
  end;
end;

initialization
  RegisterTest('Examples\Online\Tracking\', TTestTrackingSamples.Suite);
//  SetLength(FRecs, 0);
end.
