unit ActivityActionsUnit;

interface

uses
  SysUtils, BaseActionUnit,
  ActivityUnit, ActivityParametersUnit;

type
  TActivityActions = class(TBaseAction)
  public
    /// <summary>
    /// Get Activity Feed
    /// </summary>
    /// <param name="activityParameters"> Input parameters </param>
    /// <param name="errorString"> Error string </param>
    /// <returns> List of Activity objects </returns>
    function GetActivityFeed(Parameters: TActivityParameters;
      out ErrorString: String): TActivityArray;

    /// <summary>
    /// Create User Activity. Send custom message to Activity Stream.
    /// </summary>
    /// <param name="activity"> Input Activity object to add </param>
    /// <param name="errorString"> Error string </param>
    /// <returns> True/False </returns>
    function LogCustomActivity(Activity: TActivity; out ErrorString: String): boolean;
  end;

implementation

{ TActivityActions }

uses
  SettingsUnit, GetActivitiesResponseUnit, LogCustomActivityResponseUnit;

function TActivityActions.GetActivityFeed(Parameters: TActivityParameters;
  out ErrorString: String): TActivityArray;
var
  Response: TGetActivitiesResponse;
begin
  SetLength(Result, 0);

  Response := FConnection.Get(TSettings.ActivityFeedHost, Parameters,
    TGetActivitiesResponse, ErrorString) as TGetActivitiesResponse;
  try
    if (Response <> nil) then
      Result := Response.Results;
  finally
    FreeAndNil(Response);
  end;
end;

function TActivityActions.LogCustomActivity(Activity: TActivity; out ErrorString: String): boolean;
var
  Response: TLogCustomActivityResponse;
begin
  Response := FConnection.Post(TSettings.ActivityFeedHost, Activity,
    TLogCustomActivityResponse, ErrorString) as TLogCustomActivityResponse;
  try
    Result := (Response <> nil) and (Response.Status);
  finally
    FreeAndNil(Response);
  end;
end;

end.
