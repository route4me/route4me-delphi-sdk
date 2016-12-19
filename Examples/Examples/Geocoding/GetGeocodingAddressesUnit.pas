unit GetGeocodingAddressesUnit;

interface

uses SysUtils, BaseExampleUnit, DirectionPathPointUnit;

type
  TGetGeocodingAddresses = class(TBaseExample)
  public
    procedure Execute;
  end;

implementation

uses GeocodingAddressUnit;

procedure TGetGeocodingAddresses.Execute;
var
  ErrorString: String;
  Addresses: TGeocodingAddressList;
begin
  Addresses := Route4MeManager.Geocoding.GetAddresses(ErrorString);
  try
    WriteLn('');

    if (Addresses <> nil) then
    begin
      WriteLn('GetGeocodingAddresses executed successfully');
      WriteLn(Format('Address count: %d', [Addresses.Count]));
    end
    else
      WriteLn(Format('GetGeocodingAddresses error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Addresses);
  end;
end;

end.
