unit TestActivitiesSamplesUnit;

interface

uses
  TestFramework, Classes, SysUtils, DateUtils,
  BaseTestOnlineExamplesUnit;

type
  TTestActivitiesSamples = class(TTestOnlineExamples)
  published
    procedure GetAllActivities;
    procedure GetTeamActivities;
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

procedure TTestActivitiesSamples.GetTeamActivities;
  function GetRouteId: String;
  var
    Routes: TDataObjectRouteList;
    ErrorString: String;
  begin
    Routes := FRoute4MeManager.Route.GetList(1, 1, ErrorString);
    try
      CheckTrue(Routes.Count > 0);
      Result := Routes[0].RouteId;
    finally
      FreeAndNil(Routes);
    end;
  end;

  function GetDummyRouteId: String;
  begin
    // по этому Id 13 activities есть в базе
    Result := 'B15C0ED469425DBD5FC3B04DAFF2A54D';
  end;

var
  ErrorString: String;
  Activities: TActivityList;
  ActivityIds: TStringArray;
  i: integer;
  Limit, Offset: integer;
  Total: integer;
  RouteId: String;
begin
//  RouteId := GetRouteId();
  RouteId := GetDummyRouteId();

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
end;

initialization
  RegisterTest('Examples\Online\Activities\', TTestActivitiesSamples.Suite);
end.
