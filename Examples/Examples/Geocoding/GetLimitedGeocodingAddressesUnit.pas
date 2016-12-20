unit GetLimitedGeocodingAddressesUnit;

interface

uses SysUtils, BaseExampleUnit, DirectionPathPointUnit;

type
  TGetLimitedGeocodingAddresses = class(TBaseExample)
  public
    procedure Execute(Limit, Offset: integer);
  end;

implementation

uses GeocodingAddressUnit;

procedure TGetLimitedGeocodingAddresses.Execute(Limit, Offset: integer);
var
  ErrorString: String;
  Addresses: TGeocodingAddressList;
begin
  Addresses := Route4MeManager.Geocoding.GetAddresses(
    Limit, Offset, ErrorString);
  try
    WriteLn('');

    if (Addresses <> nil) then
    begin
      WriteLn('GetLimitedGeocodingAddresses executed successfully');
      WriteLn(Format('Address count: %d', [Addresses.Count]));
    end
    else
      WriteLn(Format('GetLimitedGeocodingAddresses error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Addresses);
  end;
end;

end.
