unit GeocodingActionsUnit;

interface

uses
  SysUtils, BaseActionUnit,
  GeocodingUnit, EnumsUnit, DirectionPathPointUnit, GeocodingAddressUnit;

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
    function ReverseGeocodeAddress(Location: TDirectionPathPoint;
      out ErrorString: String): TGeocoding;

    /// <summary>
    /// Single address geocoding refers to the process of getting a geographic address by address name sent with HTTP GET data.
    /// </summary>
    function GetSingleAddress(Pk: integer; out ErrorString: String): TGeocodingAddress;

    /// <summary>
    /// This example refers to the process of getting all addresses.
    /// </summary>
    function GetAddresses(out ErrorString: String): TGeocodingAddressList;

    /// <summary>
    /// This example refers to the process of getting a limited number of the addresses. The limitation parameters are: offset and limit.
    /// </summary>
    function GetLimitedAddresses(Limit, Offset: integer;
      out ErrorString: String): TGeocodingAddressList;
  end;

implementation

{ TGeocodingActions }

uses
  SettingsUnit, GenericParametersUnit, UtilsUnit;


{ TGeocodingActions }

function TGeocodingActions.ForwardGeocodeAddress(Address: String;
  out ErrorString: String): TGeocoding;
var
  Request: TGenericParameters;
  Format: TFormatEnum;
begin
  Request := TGenericParameters.Create;
  try
    Request.AddParameter('addresses', Address);
    Request.AddParameter('format', TFormatDescription[TFormatEnum.Xml]);

    Result := FConnection.Post(TSettings.EndPoints.Geocoding, Request,
      TGeocoding, ErrorString) as TGeocoding;

    if (Result = nil) and (ErrorString = EmptyStr) then
      ErrorString := 'Forward Geocode Address fault';
  finally
    FreeAndNil(Request);
  end;

// todo: в ответ XML
{
<?xml version="1.0" encoding="UTF-8" ?><destinations>
	<destination  destination="Los%20Angeles%20International%20Airport,%20CA" lat="0" lng="0" type="invalid" confidence="0" original="Los%20Angeles%20International%20Airport,%20CA"/></destinations>
}
end;

function TGeocodingActions.GetAddresses(
  out ErrorString: String): TGeocodingAddressList;
var
  Request: TGenericParameters;
begin
  Request := TGenericParameters.Create;
  try
    Result := FConnection.Get(TSettings.EndPoints.RapidAddressSearch, Request,
      TGeocodingAddressList, ErrorString) as TGeocodingAddressList;

    if (Result = nil) and (ErrorString = EmptyStr) then
      ErrorString := 'Get Addresses fault';
  finally
    FreeAndNil(Request);
  end;
end;

function TGeocodingActions.GetLimitedAddresses(Limit, Offset: integer;
  out ErrorString: String): TGeocodingAddressList;
var
  Request: TGenericParameters;
  Url: String;
begin
  Request := TGenericParameters.Create;
  try
    Url := TSettings.EndPoints.RapidAddressSearch +
      IntToStr(Offset) + '/' + IntToStr(Limit) + '/';

    Result := FConnection.Get(Url, Request,
      TGeocodingAddressList, ErrorString) as TGeocodingAddressList;

    if (Result = nil) and (ErrorString = EmptyStr) then
      ErrorString := 'Get Limited Addresses fault';
  finally
    FreeAndNil(Request);
  end;
end;

function TGeocodingActions.GetSingleAddress(Pk: integer;
  out ErrorString: String): TGeocodingAddress;
var
  Request: TGenericParameters;
  Url: String;
begin
  Request := TGenericParameters.Create;
  try
    Url := TSettings.EndPoints.RapidAddressSearch + IntToStr(Pk) + '/';

    Result := FConnection.Get(Url, Request,
      TGeocodingAddress, ErrorString) as TGeocodingAddress;

    if (Result = nil) and (ErrorString = EmptyStr) then
      ErrorString := 'Get Single Address fault';
  finally
    FreeAndNil(Request);
  end;
end;

function TGeocodingActions.ReverseGeocodeAddress(
  Location: TDirectionPathPoint; out ErrorString: String): TGeocoding;
var
  Request: TGenericParameters;
  Format: TFormatEnum;
  LocationParam: String;
begin
  Request := TGenericParameters.Create;
  try
    LocationParam := TUtils.FloatToStrDot(Location.Latitude.Value) + ',' +
      TUtils.FloatToStrDot(Location.Longitude.Value);
    Request.AddParameter('addresses', LocationParam);
    Request.AddParameter('format', TFormatDescription[TFormatEnum.Xml]);

    Result := FConnection.Post(TSettings.EndPoints.Geocoding, Request,
      TGeocoding, ErrorString) as TGeocoding;

    if (Result = nil) and (ErrorString = EmptyStr) then
      ErrorString := 'Reverse Geocode Address fault';
  finally
    FreeAndNil(Request);
  end;
// todo: в ответ XML
{
<?xml version="1.0" encoding="UTF-8" ?><destinations>
	<destination  destination="Unnamed Road, Stepasdabali, Georgia" lat="42.3543396" lng="42.3567284" type="route" confidence="medium" original="42.35863,42.35863"/>	<destination  destination="Samegrelo-Zemo Svaneti, Georgia" lat="42.7352247" lng="42.1689362" type="administrative_area_level_1, political" confidence="medium" original="42.35863,42.35863"/>	<destination  destination="Georgia" lat="42.315407" lng="43.356892" type="country, political" confidence="medium" original="42.35863,42.35863"/></destinations>
}
end;

end.
