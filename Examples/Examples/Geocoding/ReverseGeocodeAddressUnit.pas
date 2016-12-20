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
  Geocoding: TGeocodingList;
  i: integer;
begin
  Geocoding := Route4MeManager.Geocoding.ReverseGeocodeAddress(Location, ErrorString);
  try
    WriteLn('');

    if (Geocoding.Count > 0) then
    begin
      WriteLn('ReverseGeocodeAddress executed successfully');
      for i := 0 to Geocoding.Count - 1 do
        if Geocoding[i].Destination.IsNotNull then
          WriteLn(Format('Destination: %s', [Geocoding[i].Destination.Value]));
    end
    else
      WriteLn(Format('ReverseGeocodeAddress error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Geocoding);
  end;
end;

end.
