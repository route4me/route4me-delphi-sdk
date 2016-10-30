unit TrackingActionsUnit;

interface

uses
  SysUtils, BaseActionUnit, DataObjectUnit, GPSParametersUnit,
  GenericParametersUnit;

type
  TTrackingActions = class(TBaseAction)
  public
    function GetLastLocation(Parameters: TGenericParameters;
      out ErrorString: String): TDataObject;
    function SetGPS(Parameters: TGPSParameters;
      out ErrorString: String): String;
  end;

implementation

{ TTrackingActions }

uses
  SettingsUnit;

{ TTrackingActions }

function TTrackingActions.GetLastLocation(Parameters: TGenericParameters;
  out ErrorString: String): TDataObject;
begin
  Result := FConnection.Get(TSettings.RouteHost, Parameters,
    TDataObject, ErrorString) as TDataObject;
end;

function TTrackingActions.SetGPS(Parameters: TGPSParameters;
  out ErrorString: String): String;
begin
  raise Exception.Create('Здесь должен вернуться String, проверит');
{  Result := FConnection.Get(TSettings.SetGpsHost, Parameters,
    TDataObject, ErrorString) as TDataObject;}
end;

end.
