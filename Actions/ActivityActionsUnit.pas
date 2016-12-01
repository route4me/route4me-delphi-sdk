unit ActivityActionsUnit;

interface

uses
  SysUtils, BaseActionUnit,
  ActivityRequestUnit, ActivityParametersUnit, ActivityUnit;

type
  TActivityActions = class(TBaseAction)
  public
    /// <summary>
    /// Get Activity Feed
    /// </summary>
    /// <returns> List of Activity objects </returns>
    function GetAllActivities(Limit, Offset: integer; out Total: integer;
      out ErrorString: String): TActivityList;

    /// <summary>
    /// Get all recorded activities associated not only with a specific Route4Me account,
    /// but also with other users of a memberТs team.
    /// </summary>
    /// <returns> List of Activity objects </returns>
    function GetTeamActivities(RouteId: String; Limit, Offset: integer;
      out Total: integer; out ErrorString: String): TActivityList;

    /// <summary>
    /// Create User Activity. Send custom message to Activity Stream.
    /// </summary>
    /// <returns> True/False </returns>
    function LogCustomActivity(RouteId: String; Message: String;
      out ErrorString: String): boolean;

    procedure AreaAdded(out ErrorString: String);
    procedure AreaUpdated(out ErrorString: String);
    procedure AreaRemoved(out ErrorString: String);
  end;

implementation

{ TActivityActions }

uses
  SettingsUnit, GetActivitiesResponseUnit, StatusResponseUnit,
  GenericParametersUnit, EnumsUnit;

procedure TActivityActions.AreaAdded(out ErrorString: String);
var
  Response: TGetActivitiesResponse;
  Request: TGenericParameters;
begin
  Request := TGenericParameters.Create;
  try
    Request.AddParameter('activity_type', TActivityTypeDescription[TActivityType.atAreaAdded]);

    Response := FConnection.Get(TSettings.GetActivitiesHost, Request,
      TGetActivitiesResponse, ErrorString) as TGetActivitiesResponse;
    try
      if (Response = nil) and (ErrorString = EmptyStr) then
        ErrorString := 'Area Added fault';
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

procedure TActivityActions.AreaRemoved(out ErrorString: String);
var
  Response: TGetActivitiesResponse;
  Request: TGenericParameters;
begin
  Request := TGenericParameters.Create;
  try
    Request.AddParameter('activity_type', TActivityTypeDescription[TActivityType.atAreaRemoved]);

    Response := FConnection.Get(TSettings.GetActivitiesHost, Request,
      TGetActivitiesResponse, ErrorString) as TGetActivitiesResponse;
    try
      if (Response = nil) and (ErrorString = EmptyStr) then
        ErrorString := 'Area Removed fault';
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

procedure TActivityActions.AreaUpdated(out ErrorString: String);
var
  Response: TGetActivitiesResponse;
  Request: TGenericParameters;
begin
  Request := TGenericParameters.Create;
  try
    Request.AddParameter('activity_type', TActivityTypeDescription[TActivityType.atAreaUpdated]);

    Response := FConnection.Get(TSettings.GetActivitiesHost, Request,
      TGetActivitiesResponse, ErrorString) as TGetActivitiesResponse;
    try
      if (Response = nil) and (ErrorString = EmptyStr) then
        ErrorString := 'Area Updated fault';
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
    Response := FConnection.Get(TSettings.ActivityFeedHost, Parameters,
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

  // todo: проверить limit и offset применимы ли
  Request := TActivityParameters.Create(Limit, Offset);
  try
    Request.AddParameter('route_id', RouteId);
    Request.AddParameter('team', 'true');

    Response := FConnection.Get(TSettings.ActivityFeedHost, Request,
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

function TActivityActions.LogCustomActivity(RouteId: String; Message: String;
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

    Response := FConnection.Post(TSettings.ActivityFeedHost, Activity,
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
