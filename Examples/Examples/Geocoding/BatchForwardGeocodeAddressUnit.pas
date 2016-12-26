unit BatchForwardGeocodeAddressUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TBatchForwardGeocodeAddress = class(TBaseExample)
  public
    procedure Execute(Address: String);
  end;

implementation

uses GeocodingUnit;

procedure TBatchForwardGeocodeAddress.Execute(Address: String);
var
  ErrorString: String;
  Geocoding: TGeocoding;
begin
  Geocoding := Route4MeManager.Geocoding.ForwardGeocodeAddress(Address, ErrorString);
  try
    WriteLn('');

    if (Geocoding <> nil) and
      (Geocoding.Latitude.IsNotNull) and (Geocoding.Longitude.IsNotNull) then
    begin
      WriteLn('BatchForwardGeocodeAddress executed successfully');
      WriteLn(Format('Latitude: %f, Longitude: %f',
        [Geocoding.Latitude.Value, Geocoding.Longitude.Value]));
    end
    else
      WriteLn(Format('BatchForwardGeocodeAddress error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Geocoding);
  end;
end;

end.
