unit TestActivitiesSamplesUnit;

interface

uses
  TestFramework, Classes, SysUtils, DateUtils,
  BaseTestOnlineExamplesUnit;

type
  TTestActivitiesSamples = class(TTestOnlineExamples)
  private
    function GetRouteId: String;
  published
    procedure GetAllActivities;
    procedure GetTeamActivities;
    procedure LogCustomActivity;
  end;

implementation


uses EnumsUnit, ActivityUnit, CommonTypesUnit, DataObjectUnit;

procedure TTestActivitiesSamples.GetAllActivities;
var
  ErrorString: String;
  Activities: TActivityList;
  ActivityIds: TStringArray;
  i: integer;
  Limit, Offset: integer;
  Total: integer;
begin
  Limit := 5;
  Offset := 0;
  Activities := FRoute4MeManager.ActivityFeed.GetAllActivities(
    Limit, Offset, Total, ErrorString);
  try
    CheckNotNull(Activities);
    CheckEquals(EmptyStr, ErrorString);
    CheckEquals(5, Activities.Count);
    CheckTrue(Total > 0);

    SetLength(ActivityIds, Activities.Count);
    for i := 0 to Activities.Count - 1 do
      ActivityIds[i] := Activities[i].Id;
  finally
    FreeAndNil(Activities);
  end;

  Limit := 2;
  Offset := 0;
  Activities := FRoute4MeManager.ActivityFeed.GetAllActivities(
    Limit, Offset, Total, ErrorString);
  try
    CheckNotNull(Activities);
    CheckEquals(EmptyStr, ErrorString);
    CheckEquals(2, Activities.Count);
    CheckTrue(Total > 0);
    CheckTrue(ActivityIds[0] = Activities[0].Id);
    CheckTrue(ActivityIds[1] = Activities[1].Id);
  finally
    FreeAndNil(Activities);
  end;

  Limit := 2;
  Offset := 2;
  Activities := FRoute4MeManager.ActivityFeed.GetAllActivities(
    Limit, Offset, Total, ErrorString);
  try
    CheckNotNull(Activities);
    CheckEquals(EmptyStr, ErrorString);
    CheckEquals(2, Activities.Count);
    CheckTrue(Total > 0);
    CheckTrue(ActivityIds[2] = Activities[0].Id);
    CheckTrue(ActivityIds[3] = Activities[1].Id);
  finally
    FreeAndNil(Activities);
  end;

  Limit := 2;
  Offset := Total;
  Activities := FRoute4MeManager.ActivityFeed.GetAllActivities(
    Limit, Offset, Total, ErrorString);
  try
    CheckNotNull(Activities);
    CheckEquals(EmptyStr, ErrorString);
    CheckEquals(0, Activities.Count);
    CheckTrue(Total > 0);
  finally
    FreeAndNil(Activities);
  end;
end;

function TTestActivitiesSamples.GetRouteId: String;
{var
  Routes: TDataObjectRouteList;
  ErrorString: String;}
begin
{  Routes := FRoute4MeManager.Route.GetList(1, 1, ErrorString);
  try
    CheckTrue(Routes.Count > 0);
    Result := Routes[0].RouteId;
  finally
    FreeAndNil(Routes);
  end;}

  // по этому Id 13 activities есть в базе
  Result := 'B15C0ED469425DBD5FC3B04DAFF2A54D';
end;

procedure TTestActivitiesSamples.GetTeamActivities;
var
  ErrorString: String;
  Activities: TActivityList;
  ActivityIds: TStringArray;
  i: integer;
  Limit, Offset: integer;
  Total: integer;
  RouteId: String;
begin
  RouteId := GetRouteId();

  Limit := 5;
  Offset := 0;
  Activities := FRoute4MeManager.ActivityFeed.GetTeamActivities(
    RouteId, Limit, Offset, Total, ErrorString);
  try
    CheckNotNull(Activities);
    CheckEquals(EmptyStr, ErrorString);
    CheckEquals(5, Activities.Count);
    CheckTrue(Total > 0);

    SetLength(ActivityIds, Activities.Count);
    for i := 0 to Activities.Count - 1 do
      ActivityIds[i] := Activities[i].Id;
  finally
    FreeAndNil(Activities);
  end;

  Limit := 2;
  Offset := 0;
  Activities := FRoute4MeManager.ActivityFeed.GetTeamActivities(
    RouteId, Limit, Offset, Total, ErrorString);
  try
    CheckNotNull(Activities);
    CheckEquals(EmptyStr, ErrorString);
    CheckEquals(2, Activities.Count);
    CheckTrue(Total > 0);
    CheckTrue(ActivityIds[0] = Activities[0].Id);
    CheckTrue(ActivityIds[1] = Activities[1].Id);
  finally
    FreeAndNil(Activities);
  end;

  Limit := 2;
  Offset := 2;
  Activities := FRoute4MeManager.ActivityFeed.GetTeamActivities(
    RouteId, Limit, Offset, Total, ErrorString);
  try
    CheckNotNull(Activities);
    CheckEquals(EmptyStr, ErrorString);
    CheckEquals(2, Activities.Count);
    CheckTrue(Total > 0);
    CheckTrue(ActivityIds[2] = Activities[0].Id);
    CheckTrue(ActivityIds[3] = Activities[1].Id);
  finally
    FreeAndNil(Activities);
  end;

  Limit := 2;
  Offset := Total;
  Activities := FRoute4MeManager.ActivityFeed.GetTeamActivities(
    RouteId, Limit, Offset, Total, ErrorString);
  try
    CheckNotNull(Activities);
    CheckEquals(EmptyStr, ErrorString);
    CheckEquals(0, Activities.Count);
    CheckTrue(Total > 0);
  finally
    FreeAndNil(Activities);
  end;

  RouteId := 'qwe';
  Limit := 5;
  Offset := 0;
  Activities := FRoute4MeManager.ActivityFeed.GetTeamActivities(
    RouteId, Limit, Offset, Total, ErrorString);
  try
    CheckNotNull(Activities);
    CheckEquals(EmptyStr, ErrorString);
    CheckEquals(0, Activities.Count);
    CheckEquals(0, Total);
  finally
    FreeAndNil(Activities);
  end;

end;

procedure TTestActivitiesSamples.LogCustomActivity;
var
  RouteId: String;
  Message: String;
  ErrorString: String;
begin
  RouteId := 'qwe';
  Message := EmptyStr;
  CheckFalse(
    FRoute4MeManager.ActivityFeed.LogCustomActivity(RouteId, Message, ErrorString));
  CheckNotEquals(EmptyStr, ErrorString);

  RouteId := GetRouteId();
  CheckFalse(
    FRoute4MeManager.ActivityFeed.LogCustomActivity(RouteId, Message, ErrorString));
  CheckNotEquals(EmptyStr, ErrorString);

  RouteId := 'qwe';
  Message := 'Test';
  // todo: при несуществующем RouteId все равно сервер возвращает True. Спросил у Олега
  CheckTrue(
    FRoute4MeManager.ActivityFeed.LogCustomActivity(RouteId, Message, ErrorString));
  CheckEquals(EmptyStr, ErrorString);

  RouteId := GetRouteId();
  CheckTrue(
    FRoute4MeManager.ActivityFeed.LogCustomActivity(RouteId, Message, ErrorString));
  CheckEquals(EmptyStr, ErrorString);
end;

initialization
  RegisterTest('Examples\Online\Activities\', TTestActivitiesSamples.Suite);
end.
