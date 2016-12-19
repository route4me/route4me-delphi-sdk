unit ForwardGeocodeAddressUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TForwardGeocodeAddress = class(TBaseExample)
  public
    procedure Execute(Address: String);
  end;

implementation

uses GeocodingUnit;

procedure TForwardGeocodeAddress.Execute(Address: String);
var
  ErrorString: String;
  Geocoding: TGeocoding;
begin
  Geocoding := Route4MeManager.Geocoding.ForwardGeocodeAddress(Address, ErrorString);
  try
    WriteLn('');

    if (Geocoding <> nil) and
      (Geocoding.Latitude.IsNotNull) and(Geocoding.Longitude.IsNotNull) then
    begin
      WriteLn('ForwardGeocodeAddress executed successfully');
      WriteLn(Format('Latitude: %d, Longitude: %d',
        [Geocoding.Latitude.Value, Geocoding.Longitude.Value]));
    end
    else
      WriteLn(Format('ForwardGeocodeAddress error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Geocoding);
  end;
end;

end.
