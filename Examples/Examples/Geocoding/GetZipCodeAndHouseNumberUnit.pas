unit GetZipCodeAndHouseNumberUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetZipCodeAndHouseNumber = class(TBaseExample)
  public
    procedure Execute(ZipCode, HouseNumber: String);
  end;

implementation

uses GeocodingAddressUnit;

procedure TGetZipCodeAndHouseNumber.Execute(ZipCode, HouseNumber: String);
var
  ErrorString: String;
  Addresses: TGeocodingAddressList;
begin
  Addresses := Route4MeManager.Geocoding.GetZipCodeAndHouseNumber(
    ZipCode, HouseNumber, ErrorString);
  try
    WriteLn('');

    if (Addresses <> nil) then
    begin
      WriteLn('GetZipCodeAndHouseNumber executed successfully');
      WriteLn(Format('Address count: %d', [Addresses.Count]));
    end
    else
      WriteLn(Format('GetZipCodeAndHouseNumber error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Addresses);
  end;
end;

end.
