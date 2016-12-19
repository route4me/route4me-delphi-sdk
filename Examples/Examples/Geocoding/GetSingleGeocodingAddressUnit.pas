unit GetSingleGeocodingAddressUnit;

interface

uses SysUtils, BaseExampleUnit, DirectionPathPointUnit;

type
  TGetSingleGeocodingAddress = class(TBaseExample)
  public
    procedure Execute(Pk: integer);
  end;

implementation

uses GeocodingAddressUnit;

procedure TGetSingleGeocodingAddress.Execute(Pk: integer);
var
  ErrorString: String;
  Address: TGeocodingAddress;
begin
  Address := Route4MeManager.Geocoding.GetSingleAddress(Pk, ErrorString);
  try
    WriteLn('');

    if (Address <> nil) then
    begin
      WriteLn('GetSingleGeocodingAddress executed successfully');
      WriteLn(Format('StreeName: %s, Zip: %s',
        [Address.StreetName.Value, Address.ZipCode.Value]));
    end
    else
      WriteLn(Format('GetSingleGeocodingAddress error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Address);
  end;
end;

end.
