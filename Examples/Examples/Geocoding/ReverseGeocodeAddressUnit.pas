unit ReverseGeocodeAddressUnit;

interface

uses SysUtils, BaseExampleUnit, DirectionPathPointUnit;

type
  TReverseGeocodeAddress = class(TBaseExample)
  public
    procedure Execute(Location: TDirectionPathPoint);
  end;

implementation

uses GeocodingUnit;

procedure TReverseGeocodeAddress.Execute(Location: TDirectionPathPoint);
var
  ErrorString: String;
  Geocoding: TGeocoding;
begin
  Geocoding := Route4MeManager.Geocoding.ReverseGeocodeAddress(Location, ErrorString);
  try
    WriteLn('');

    if (Geocoding <> nil) and (Geocoding.Destination.IsNotNull) then
    begin
      WriteLn('ReverseGeocodeAddress executed successfully');
      WriteLn(Format('Destination: %s', [Geocoding.Destination.Value]));
    end
    else
      WriteLn(Format('ReverseGeocodeAddress error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Geocoding);
  end;
end;

end.
