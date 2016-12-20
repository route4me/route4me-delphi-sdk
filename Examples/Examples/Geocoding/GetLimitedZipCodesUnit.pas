unit GetLimitedZipCodesUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetLimitedZipCodes = class(TBaseExample)
  public
    procedure Execute(ZipCode: String; Limit, Offset: integer);
  end;

implementation

uses GeocodingAddressUnit;

procedure TGetLimitedZipCodes.Execute(ZipCode: String; Limit, Offset: integer);
var
  ErrorString: String;
  Addresses: TGeocodingAddressList;
begin
  Addresses := Route4MeManager.Geocoding.GetZipCodes(
    ZipCode, Limit, Offset, ErrorString);
  try
    WriteLn('');

    if (Addresses <> nil) then
    begin
      WriteLn('GetLimitedZipCodes executed successfully');
      WriteLn(Format('Address count: %d', [Addresses.Count]));
    end
    else
      WriteLn(Format('GetLimitedZipCodes error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Addresses);
  end;
end;

end.
