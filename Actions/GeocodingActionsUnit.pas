unit GeocodingActionsUnit;

interface

uses
  SysUtils, BaseActionUnit,
  GeocodingUnit, EnumsUnit, DirectionPathPointUnit;

type
  TGeocodingActions = class(TBaseAction)
  public
    /// <summary>
    /// Forward geocoding is the process of converting place name information into latitude and longitude values
    /// </summary>
    function ForwardGeocodeAddress(Address: String;
      out ErrorString: String): TGeocoding;

    /// <summary>
    /// With the reverse geocoding you can retrieve an address name from a geographical location point (latitude, longitude).
    /// Using this method, you can get the nearest locations to a specific address name.
    /// You can also get the larger scale objects (such as street addresses, places,
    /// neighbourhoods, county, state or country) which include a specified address.
    /// </summary>
    function ReverseGeocodeAddress(Locations: TDirectionPathPointList;
      Format: TFormatEnum; out ErrorString: String): boolean;
  end;

implementation

{ TGeocodingActions }

uses
  SettingsUnit, GenericParametersUnit;


{ TGeocodingActions }

function TGeocodingActions.ForwardGeocodeAddress(Address: String;
  out ErrorString: String): TGeocoding;
var
  Request: TGenericParameters;
{  Response: TGenericParameters;
  Format: TFormatEnum;}
begin
  Request := TGenericParameters.Create;
  try
    Request.AddParameter('addresses', Address);
    Request.AddParameter('format', TFormatDescription[TFormatEnum.Xml]);

{    Response := FConnection.Get(TSettings.GeocodingHost, Request,
      TStatusResponse, ErrorString) as TStatusResponse;
    try
      Result := (Response <> nil) and (Response.Status);
    finally
      FreeAndNil(Response);
    end;}
  finally
    FreeAndNil(Request);
  end;
end;

function TGeocodingActions.ReverseGeocodeAddress(
  Locations: TDirectionPathPointList; Format: TFormatEnum;
  out ErrorString: String): boolean;
begin

end;

end.
