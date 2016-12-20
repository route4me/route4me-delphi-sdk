unit GetLimitedZipCodeAndHouseNumberUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetLimitedZipCodeAndHouseNumber = class(TBaseExample)
  public
    procedure Execute(ZipCode, HouseNumber: String; Limit, Offset: integer);
  end;

implementation

uses GeocodingAddressUnit;

procedure TGetLimitedZipCodeAndHouseNumber.Execute(
  ZipCode, HouseNumber: String; Limit, Offset: integer);
var
  ErrorString: String;
  Addresses: TGeocodingAddressList;
begin
  Addresses := Route4MeManager.Geocoding.GetZipCodeAndHouseNumber(
    ZipCode, HouseNumber, Limit, Offset, ErrorString);
  try
    WriteLn('');

    if (Addresses <> nil) then
    begin
      WriteLn('GetLimitedZipCodeAndHouseNumber executed successfully');
      WriteLn(Format('Address count: %d', [Addresses.Count]));
    end
    else
      WriteLn(Format('GetLimitedZipCodeAndHouseNumber error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Addresses);
  end;
end;

end.
