unit TestActivitiesSamplesUnit;

interface

uses
  TestFramework, Classes, SysUtils, DateUtils,
  BaseTestOnlineExamplesUnit, EnumsUnit;

type
  TTestActivitiesSamples = class(TTestOnlineExamples)
  private
    function GetRouteId: String;

    procedure CheckActivitiesWithInvalidRouteId(ActivityType: TActivityType);
    procedure CheckActivitiesWithRouteId(ActivityType: TActivityType; RouteId: String);
    function CheckActivitiesWithoutRouteId(ActivityType: TActivityType;
      IsNullNow: boolean = False): String;
  published
    procedure GetAllActivities;
    procedure GetTeamActivities;
    procedure GetAreaAddedActivities;
    procedure GetAreaUpdatedActivities;
    procedure GetAreaRemovedActivities;
    procedure GetDestinationDeletedActivities;
    procedure GetDestinationOutOfSequenceActivities;
    procedure GetDriverArrivedEarlyActivities;
    procedure GetDriverArrivedLateActivities;
    procedure GetDriverArrivedOnTimeActivities;
    procedure GetGeofenceLeftActivities;
    procedure GetGeofenceEnteredActivities;
    procedure GetDestinationInsertedActivities;
    procedure GetDestinationMarkedAsDepartedActivities;
    procedure GetDestinationMarkedAsVisitedActivities;
    procedure GetMemberCreatedActivities;
    procedure GetMemberDeletedActivities;
    procedure GetMemberModifiedActivities;
    procedure GetDestinationMovedActivities;
    procedure GetNoteInsertedActivities;
    procedure GetRouteDeletedActivities;
    procedure GetRouteOptimizedActivities;
    procedure GetRouteOwnerChangedActivities;
    procedure GetDestinationUpdatedActivities;
    procedure LogSpecificMessage;
  end;

implementation


uses ActivityUnit, CommonTypesUnit, DataObjectUnit;

procedure TTestActivitiesSamples.GetAreaAddedActivities;
var
  RouteId: String;
  ActivityType: TActivityType;
begin
  ActivityType := TActivityType.atAreaAdded;

  CheckActivitiesWithInvalidRouteId(ActivityType);
  RouteId := CheckActivitiesWithoutRouteId(ActivityType);
  if (RouteId <> EmptyStr) then
    CheckActivitiesWithRouteId(ActivityType, RouteId);
end;

procedure TTestActivitiesSamples.GetAreaRemovedActivities;
var
  RouteId: String;
  ActivityType: TActivityType;
begin
  ActivityType := TActivityType.atAreaRemoved;

  CheckActivitiesWithInvalidRouteId(ActivityType);
  RouteId := CheckActivitiesWithoutRouteId(ActivityType);
  if (RouteId <> EmptyStr) then
    CheckActivitiesWithRouteId(ActivityType, RouteId);
end;

procedure TTestActivitiesSamples.GetAreaUpdatedActivities;
var
  RouteId: String;
  ActivityType: TActivityType;
begin
  ActivityType := TActivityType.atAreaUpdated;

  CheckActivitiesWithInvalidRouteId(ActivityType);
  RouteId := CheckActivitiesWithoutRouteId(ActivityType);
  if (RouteId <> EmptyStr) then
    CheckActivitiesWithRouteId(ActivityType, RouteId);
end;

procedure TTestActivitiesSamples.CheckActivitiesWithInvalidRouteId(
  ActivityType: TActivityType);
var
  RouteId: String;
  Activities: TActivityList;
  Limit, Offset, Total: integer;
  ErrorString: String;
begin
  RouteId := 'qwe';
  Limit := 2;
  Offset := 0;

  Activities := FRoute4MeManager.ActivityFeed.GetActivities(RouteId,
    ActivityType, Limit, Offset, Total, ErrorString);
  try
    CheckEquals(EmptyStr, ErrorString);
    CheckEquals(0, Total);
    CheckEquals(0, Activities.Count);
  finally
    FreeAndNil(Activities);
  end;
end;

function TTestActivitiesSamples.CheckActivitiesWithoutRouteId(
  ActivityType: TActivityType; IsNullNow: boolean = False): String;
var
  Activities: TActivityList;
  ErrorString: String;
  Total: integer;
  Limit, Offset: integer;
  i: integer;
begin
  Result := EmptyStr;

  Limit := 2;
  Offset := 0;
  Activities := FRoute4MeManager.ActivityFeed.GetActivities(
    ActivityType, Limit, Offset, Total, ErrorString);
  try
    if (Activities.Count > 0) then
      for i := 0 to Activities.Count - 1 do
        if Activities[0].RouteId.IsNotNull then
        begin
          Result := Activities[0].RouteId;
          Break;
        end;

    CheckEquals(EmptyStr, ErrorString);

    if (IsNullNow) then
    begin
      CheckEquals(0, Total);
      CheckEquals(0, Activities.Count);
    end
    else
    begin
      CheckTrue(Total > 0);
      CheckTrue((Activities.Count > 0) and (Activities.Count <= Limit));
    end;
  finally
    FreeAndNil(Activities);
  end;
end;

procedure TTestActivitiesSamples.CheckActivitiesWithRouteId(
  ActivityType: TActivityType; RouteId: String);
var
  Activities: TActivityList;
  ErrorString: String;
  Total: integer;
  Limit, Offset: integer;
begin
  Limit := 2;
  Offset := 0;

  Activities := FRoute4MeManager.ActivityFeed.GetActivities(
    RouteId, ActivityType, Limit, Offset, Total, ErrorString);
  try
    CheckEquals(EmptyStr, ErrorString);
    CheckTrue(Total > 0);
    CheckTrue((Activities.Count > 0) and (Activities.Count <= Limit));
  finally
    FreeAndNil(Activities);
  end;
end;

procedure TTestActivitiesSamples.GetDestinationDeletedActivities;
var
  RouteId: String;
  ActivityType: TActivityType;
begin
  ActivityType := TActivityType.atDeleteDestination;

  CheckActivitiesWithInvalidRouteId(ActivityType);
  RouteId := CheckActivitiesWithoutRouteId(ActivityType);
  if (RouteId <> EmptyStr) then
    CheckActivitiesWithRouteId(ActivityType, RouteId);
end;

procedure TTestActivitiesSamples.GetDestinationInsertedActivities;
var
  RouteId: String;
  ActivityType: TActivityType;
begin
  ActivityType := TActivityType.atInsertDestination;

  CheckActivitiesWithInvalidRouteId(ActivityType);
  RouteId := CheckActivitiesWithoutRouteId(ActivityType);
  if (RouteId <> EmptyStr) then
    CheckActivitiesWithRouteId(ActivityType, RouteId);
end;

procedure TTestActivitiesSamples.GetDestinationMarkedAsDepartedActivities;
var
  RouteId: String;
  ActivityType: TActivityType;
begin
  ActivityType := TActivityType.atMarkDestinationDeparted;

  CheckActivitiesWithInvalidRouteId(ActivityType);
  RouteId := CheckActivitiesWithoutRouteId(ActivityType);
  if (RouteId <> EmptyStr) then
    CheckActivitiesWithRouteId(ActivityType, RouteId);
end;

procedure TTestActivitiesSamples.GetDestinationMarkedAsVisitedActivities;
var
  RouteId: String;
  ActivityType: TActivityType;
begin
  ActivityType := TActivityType.atMarkDestinationVisited;

  CheckActivitiesWithInvalidRouteId(ActivityType);
  RouteId := CheckActivitiesWithoutRouteId(ActivityType);
  if (RouteId <> EmptyStr) then
    CheckActivitiesWithRouteId(ActivityType, RouteId);
end;

procedure TTestActivitiesSamples.GetDestinationMovedActivities;
var
  RouteId: String;
  ActivityType: TActivityType;
begin
  ActivityType := TActivityType.atMoveDestination;

  CheckActivitiesWithInvalidRouteId(ActivityType);
  RouteId := CheckActivitiesWithoutRouteId(ActivityType);
  if (RouteId <> EmptyStr) then
    CheckActivitiesWithRouteId(ActivityType, RouteId);
end;

procedure TTestActivitiesSamples.GetDestinationOutOfSequenceActivities;
var
  RouteId: String;
  ActivityType: TActivityType;
begin
  ActivityType := TActivityType.atDestinationOutSequence;

  CheckActivitiesWithInvalidRouteId(ActivityType);
  RouteId := CheckActivitiesWithoutRouteId(ActivityType, True);
  if (RouteId <> EmptyStr) then
    CheckActivitiesWithRouteId(ActivityType, RouteId);
end;

procedure TTestActivitiesSamples.GetDestinationUpdatedActivities;
var
  RouteId: String;
  ActivityType: TActivityType;
begin
  ActivityType := TActivityType.atUpdateDestinations;

  CheckActivitiesWithInvalidRouteId(ActivityType);
  RouteId := CheckActivitiesWithoutRouteId(ActivityType);
  if (RouteId <> EmptyStr) then
    CheckActivitiesWithRouteId(ActivityType, RouteId);
end;

procedure TTestActivitiesSamples.GetDriverArrivedEarlyActivities;
var
  RouteId: String;
  ActivityType: TActivityType;
begin
  ActivityType := TActivityType.atDriverArrivedEarly;

  CheckActivitiesWithInvalidRouteId(ActivityType);
  RouteId := CheckActivitiesWithoutRouteId(ActivityType, True);
  if (RouteId <> EmptyStr) then
    CheckActivitiesWithRouteId(ActivityType, RouteId);
end;

procedure TTestActivitiesSamples.GetDriverArrivedLateActivities;
var
  RouteId: String;
  ActivityType: TActivityType;
begin
  ActivityType := TActivityType.atDriverArrivedLate;

  CheckActivitiesWithInvalidRouteId(ActivityType);
  RouteId := CheckActivitiesWithoutRouteId(ActivityType);
  if (RouteId <> EmptyStr) then
    CheckActivitiesWithRouteId(ActivityType, RouteId);
end;

procedure TTestActivitiesSamples.GetDriverArrivedOnTimeActivities;
var
  RouteId: String;
  ActivityType: TActivityType;
begin
  ActivityType := TActivityType.atDriverArrivedOnTime;

  CheckActivitiesWithInvalidRouteId(ActivityType);
  RouteId := CheckActivitiesWithoutRouteId(ActivityType, False);
  if (RouteId <> EmptyStr) then
    CheckActivitiesWithRouteId(ActivityType, RouteId);
end;

procedure TTestActivitiesSamples.GetGeofenceEnteredActivities;
var
  RouteId: String;
  ActivityType: TActivityType;
begin
  ActivityType := TActivityType.atGeofenceEntered;

  CheckActivitiesWithInvalidRouteId(ActivityType);
  RouteId := CheckActivitiesWithoutRouteId(ActivityType, True);
  if (RouteId <> EmptyStr) then
    CheckActivitiesWithRouteId(ActivityType, RouteId);
end;

procedure TTestActivitiesSamples.GetGeofenceLeftActivities;
var
  RouteId: String;
  ActivityType: TActivityType;
begin
  ActivityType := TActivityType.atGeofenceLeft;

  CheckActivitiesWithInvalidRouteId(ActivityType);
  RouteId := CheckActivitiesWithoutRouteId(ActivityType, True);
  if (RouteId <> EmptyStr) then
    CheckActivitiesWithRouteId(ActivityType, RouteId);
end;

procedure TTestActivitiesSamples.GetAllActivities;
var
  ErrorString: String;
  Activities: TActivityList;
  ActivityIds: TStringArray;
  i: integer;
  Limit, Offset: integer;
  Total: integer;
  Expected: String;
begin
  Limit := 5;
  Offset := 0;
  Activities := FRoute4MeManager.ActivityFeed.GetAllActivities(
    Limit, Offset, Total, ErrorString);
  try
    CheckNotNull(Activities, '1. Activities=null');
    CheckEquals(EmptyStr, ErrorString, '1. ErrorString <> ""');
    CheckEquals(5, Activities.Count, '1. Activities.Count');
    CheckTrue(Total > 0, '1. Total');

    SetLength(ActivityIds, Activities.Count);
    Expected := EmptyStr;
    for i := 0 to Activities.Count - 1 do
    begin
      ActivityIds[i] := Activities[i].Id;
      Expected := Expected + Activities[i].Id + '; ';
    end;
  finally
    FreeAndNil(Activities);
  end;

  Limit := 2;
  Offset := 0;
  Activities := FRoute4MeManager.ActivityFeed.GetAllActivities(
    Limit, Offset, Total, ErrorString);
  try
    CheckNotNull(Activities, '2. Activities=null');
    CheckEquals(EmptyStr, ErrorString, '2. ErrorString <> ""');
    CheckEquals(2, Activities.Count, '2. Activities.Count');
    CheckTrue(Total > 0, '2. Total');
    CheckTrue((ActivityIds[0] = Activities[0].Id) or (ActivityIds[1] = Activities[0].Id), '2. ActivityId0. Actual: ' + Activities[0].Id + '; Expected: ' + Expected);
    CheckTrue((ActivityIds[1] = Activities[1].Id) or (ActivityIds[0] = Activities[1].Id), '2. ActivityId1. Actual: ' + Activities[1].Id + '; Expected: ' + Expected);
  finally
    FreeAndNil(Activities);
  end;

  Limit := 2;
  Offset := 2;
  Activities := FRoute4MeManager.ActivityFeed.GetAllActivities(
    Limit, Offset, Total, ErrorString);
  try
    CheckNotNull(Activities, '3. Activities=null');
    CheckEquals(EmptyStr, ErrorString, '3. ErrorString <> ""');
    CheckEquals(2, Activities.Count, '3. Activities.Count');
    CheckTrue(Total > 0, '3. Total');
    CheckTrue((ActivityIds[2] = Activities[0].Id) or (ActivityIds[3] = Activities[0].Id),  '3. ActivityId0. Actual: ' + Activities[0].Id + '; Expected: ' + Expected);
    CheckTrue((ActivityIds[3] = Activities[1].Id) or (ActivityIds[2] = Activities[1].Id), '3. ActivityId1. Actual: ' + Activities[1].Id + '; Expected: ' + Expected);
  finally
    FreeAndNil(Activities);
  end;

  Limit := 2;
  Offset := Total;
  Activities := FRoute4MeManager.ActivityFeed.GetAllActivities(
    Limit, Offset, Total, ErrorString);
  try
    CheckNotNull(Activities, '4. Activities=null');
    CheckEquals(EmptyStr, ErrorString, '4. ErrorString <> ""');
    CheckEquals(0, Activities.Count, '4. Activities.Count');
    CheckTrue(Total > 0, '4. Total');
  finally
    FreeAndNil(Activities);
  end;
end;

function TTestActivitiesSamples.GetRouteId: String;
var
  Routes: TDataObjectRouteList;
  ErrorString: String;
begin
  // по этому Id 13 activities есть в базе
  Result := 'B15C0ED469425DBD5FC3B04DAFF2A54D';
  Result := '4A840163D74804552EEAFC75EC1E50BD'; //4
{
  Routes := FRoute4MeManager.Route.GetList(20, 2, ErrorString);
  try
    CheckTrue(Routes.Count > 0);
    Result := Routes[0].RouteId;
  finally
    FreeAndNil(Routes);
  end;
}
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

procedure TTestActivitiesSamples.LogSpecificMessage;
var
  RouteId: String;
  Message: String;
  ErrorString: String;
begin
  // An empty message is an error
  RouteId := 'qwe';
  Message := EmptyStr;
  CheckFalse(
    FRoute4MeManager.ActivityFeed.LogSpecificMessage(RouteId, Message, ErrorString));
  CheckNotEquals(EmptyStr, ErrorString);

  // An empty message is an error
  RouteId := GetRouteId();
  CheckFalse(
    FRoute4MeManager.ActivityFeed.LogSpecificMessage(RouteId, Message, ErrorString));
  CheckNotEquals(EmptyStr, ErrorString);

  Message := 'T 5est';
  // Non-existent RouteId is error
  RouteId := 'qwe2321sadas';
  // todo 1: при несуществующем RouteId все равно сервер возвращает True. Спросил у Олега
  CheckTrue(
    FRoute4MeManager.ActivityFeed.LogSpecificMessage(RouteId, Message, ErrorString));
  CheckEquals(EmptyStr, ErrorString);

  RouteId := GetRouteId();
  CheckTrue(
    FRoute4MeManager.ActivityFeed.LogSpecificMessage(RouteId, Message, ErrorString));
  CheckEquals(EmptyStr, ErrorString);
end;

procedure TTestActivitiesSamples.GetMemberCreatedActivities;
var
  RouteId: String;
  ActivityType: TActivityType;
begin
  ActivityType := TActivityType.atMemberCreated;

  CheckActivitiesWithInvalidRouteId(ActivityType);
  RouteId := CheckActivitiesWithoutRouteId(ActivityType);
  if (RouteId <> EmptyStr) then
    CheckActivitiesWithRouteId(ActivityType, RouteId);
end;

procedure TTestActivitiesSamples.GetMemberDeletedActivities;
var
  RouteId: String;
  ActivityType: TActivityType;
begin
  ActivityType := TActivityType.atMemberDeleted;

  CheckActivitiesWithInvalidRouteId(ActivityType);
  RouteId := CheckActivitiesWithoutRouteId(ActivityType);
  if (RouteId <> EmptyStr) then
    CheckActivitiesWithRouteId(ActivityType, RouteId);
end;

procedure TTestActivitiesSamples.GetMemberModifiedActivities;
var
  RouteId: String;
  ActivityType: TActivityType;
begin
  ActivityType := TActivityType.atMemberModified;

  CheckActivitiesWithInvalidRouteId(ActivityType);
  RouteId := CheckActivitiesWithoutRouteId(ActivityType, True);
  if (RouteId <> EmptyStr) then
    CheckActivitiesWithRouteId(ActivityType, RouteId);
end;

procedure TTestActivitiesSamples.GetNoteInsertedActivities;
var
  RouteId: String;
  ActivityType: TActivityType;
begin
  ActivityType := TActivityType.atNoteInsert;

  CheckActivitiesWithInvalidRouteId(ActivityType);
  RouteId := CheckActivitiesWithoutRouteId(ActivityType);
  if (RouteId <> EmptyStr) then
    CheckActivitiesWithRouteId(ActivityType, RouteId);
end;

procedure TTestActivitiesSamples.GetRouteDeletedActivities;
var
  RouteId: String;
  ActivityType: TActivityType;
begin
  ActivityType := TActivityType.atRouteDelete;

  CheckActivitiesWithInvalidRouteId(ActivityType);
  RouteId := CheckActivitiesWithoutRouteId(ActivityType);
  if (RouteId <> EmptyStr) then
    CheckActivitiesWithRouteId(ActivityType, RouteId);
end;

procedure TTestActivitiesSamples.GetRouteOptimizedActivities;
var
  RouteId: String;
  ActivityType: TActivityType;
begin
  ActivityType := TActivityType.atRouteOptimized;

  CheckActivitiesWithInvalidRouteId(ActivityType);
  RouteId := CheckActivitiesWithoutRouteId(ActivityType);
  if (RouteId <> EmptyStr) then
    CheckActivitiesWithRouteId(ActivityType, RouteId);
end;

procedure TTestActivitiesSamples.GetRouteOwnerChangedActivities;
var
  RouteId: String;
  ActivityType: TActivityType;
begin
  ActivityType := TActivityType.atRouteOwnerChanged;

  CheckActivitiesWithInvalidRouteId(ActivityType);
  RouteId := CheckActivitiesWithoutRouteId(ActivityType, True);
  if (RouteId <> EmptyStr) then
    CheckActivitiesWithRouteId(ActivityType, RouteId);
end;

initialization
  RegisterTest('Examples\Online\Activities\', TTestActivitiesSamples.Suite);
end.
