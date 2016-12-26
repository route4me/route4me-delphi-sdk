unit BulkForwardGeocodeAddressesUnit;

interface

uses SysUtils, BaseExampleUnit, CommonTypesUnit, BulkGeocodingRequestUnit;

type
  TBulkForwardGeocodeAddresses = class(TBaseExample)
  public
    procedure Execute(Addresses: TAddressInfoArray);
  end;

implementation

uses GeocodingUnit;

procedure TBulkForwardGeocodeAddresses.Execute(Addresses: TAddressInfoArray);
var
  ErrorString: String;
  Geocoding: TGeocodingList;
  i: integer;
begin
  Geocoding := Route4MeManager.Geocoding.ForwardGeocodeAddresses(
    Addresses, ErrorString);
  try
    WriteLn('');

    if (Geocoding.Count > 0) then
    begin
      WriteLn('BulkForwardGeocodeAddresses executed successfully');
      for i := 0 to Geocoding.Count - 1 do
        if Geocoding[i].Destination.IsNotNull then
          WriteLn(Format('Destination: %s', [Geocoding[i].Destination.Value]));
    end
    else
      WriteLn(Format('BulkForwardGeocodeAddresses error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Geocoding);
  end;
end;

end.
