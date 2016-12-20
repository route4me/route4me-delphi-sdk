unit GetZipCodesUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetZipCodes = class(TBaseExample)
  public
    procedure Execute(ZipCode: String);
  end;

implementation

uses GeocodingAddressUnit;

procedure TGetZipCodes.Execute(ZipCode: String);
var
  ErrorString: String;
  Addresses: TGeocodingAddressList;
begin
  Addresses := Route4MeManager.Geocoding.GetZipCodes(ZipCode, ErrorString);
  try
    WriteLn('');

    if (Addresses <> nil) then
    begin
      WriteLn('GetZipCodes executed successfully');
      WriteLn(Format('Address count: %d', [Addresses.Count]));
    end
    else
      WriteLn(Format('GetZipCodes error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Addresses);
  end;
end;

end.
