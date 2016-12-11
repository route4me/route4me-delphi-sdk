unit ActivityActionsUnit;

interface

uses
  SysUtils, BaseActionUnit,
  ActivityRequestUnit, ActivityParametersUnit, ActivityUnit, EnumsUnit;

type
  TActivityActions = class(TBaseAction)
  public
    /// <summary>
    /// Create User Activity. Send custom message to Activity Stream.
    /// </summary>
    /// <returns> True/False </returns>
    function LogSpecificMessage(RouteId: String; Message: String;
      out ErrorString: String): boolean;

    /// <summary>
    /// Get Activity Feed
    /// </summary>
    /// <returns> List of Activity objects </returns>
    function GetAllActivities(Limit, Offset: integer; out Total: integer;
      out ErrorString: String): TActivityList;

    /// <summary>
    /// Get all recorded activities associated not only with a specific Route4Me account,
    /// but also with other users of a member’s team.
    /// </summary>
    /// <returns> List of Activity objects </returns>
    function GetTeamActivities(RouteId: String; Limit, Offset: integer;
      out Total: integer; out ErrorString: String): TActivityList;

    function GetActivities(ActivityType: TActivityType;
      Limit, Offset: integer; out Total: integer;
      out ErrorString: String): TActivityList; overload;
    function GetActivities(RouteId: String; ActivityType: TActivityType;
      Limit, Offset: integer; out Total: integer;
      out ErrorString: String): TActivityList; overload;
  end;

implementation

uses
  SettingsUnit, GetActivitiesResponseUnit, StatusResponseUnit,
  GenericParametersUnit, GetActivitiesQueryUnit;

function TActivityActions.GetActivities(RouteId: String; ActivityType: TActivityType;
  Limit, Offset: integer; out Total: integer; out ErrorString: String): TActivityList;
var
  Response: TGetActivitiesResponse;
  Request: TGetActivitiesQuery;
  i: integer;
begin
  Result := TActivityList.Create();

  Request := TGetActivitiesQuery.Create(ActivityType, RouteId, Limit, Offset);
  try
    Response := FConnection.Get(TSettings.EndPoints.GetActivities, Request,
      TGetActivitiesResponse, ErrorString) as TGetActivitiesResponse;
    try
      if (Response <> nil) then
      begin
        for i := 0 to Length(Response.Results) - 1 do
          Result.Add(Response.Results[i]);
        Total := Response.Total;
      end;

      if (Response = nil) and (ErrorString = EmptyStr) then
        ErrorString := TActivityTypeDescription[ActivityType] +  ' fault';
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

function TActivityActions.GetActivities(ActivityType: TActivityType; Limit,
  Offset: integer; out Total: integer; out ErrorString: String): TActivityList;
var
  Response: TGetActivitiesResponse;
  Request: TGetActivitiesQuery;
  i: integer;
begin
  Result := TActivityList.Create();

  Request := TGetActivitiesQuery.Create(ActivityType, Limit, Offset);
  try
    Response := FConnection.Get(TSettings.EndPoints.GetActivities, Request,
      TGetActivitiesResponse, ErrorString) as TGetActivitiesResponse;
    try
      if (Response <> nil) then
      begin
        for i := 0 to Length(Response.Results) - 1 do
          Result.Add(Response.Results[i]);
        Total := Response.Total;
      end;

      if (Response = nil) and (ErrorString = EmptyStr) then
        ErrorString := TActivityTypeDescription[ActivityType] +  ' fault';
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

function TActivityActions.GetAllActivities(Limit, Offset: integer;
  out Total: integer; out ErrorString: String): TActivityList;
var
  Response: TGetActivitiesResponse;
  Parameters: TActivityParameters;
  i: integer;
begin
  Result := TActivityList.Create;
  Total := 0;

  Parameters := TActivityParameters.Create(Limit, Offset);
  try
    Response := FConnection.Get(TSettings.EndPoints.ActivityFeed, Parameters,
      TGetActivitiesResponse, ErrorString) as TGetActivitiesResponse;
    try
      if (Response <> nil) then
      begin
        for i := 0 to Length(Response.Results) - 1 do
          Result.Add(Response.Results[i]);
        Total := Response.Total;
      end;
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

function TActivityActions.GetTeamActivities(RouteId: String; Limit, Offset: integer;
  out Total: integer; out ErrorString: String): TActivityList;
var
  Response: TGetActivitiesResponse;
  Request: TActivityParameters;
  i: integer;
begin
  Result := TActivityList.Create;

  Request := TActivityParameters.Create(Limit, Offset);
  try
    Request.AddParameter('route_id', RouteId);
    Request.AddParameter('team', 'true');

    Response := FConnection.Get(TSettings.EndPoints.ActivityFeed, Request,
      TGetActivitiesResponse, ErrorString) as TGetActivitiesResponse;
    try
      if (Response <> nil) then
      begin
        for i := 0 to Length(Response.Results) - 1 do
          Result.Add(Response.Results[i]);
        Total := Response.Total;
      end;
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

function TActivityActions.LogSpecificMessage(RouteId: String; Message: String;
  out ErrorString: String): boolean;
var
  Response: TStatusResponse;
  Activity: TActivity;
begin
  Activity := TActivity.Create;
  try
    Activity.RouteId := RouteId;
    Activity.ActivityMessage := Message;
    Activity.ActivityType := TActivityType.atUserMessage;

    Response := FConnection.Post(TSettings.EndPoints.ActivityFeed, Activity,
      TStatusResponse, ErrorString) as TStatusResponse;
    try
      Result := (Response <> nil) and (Response.Status);
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Activity);
  end;
end;

end.
