unit ForwardGeocodeAddressUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TForwardGeocodeAddress = class(TBaseExample)
  public
    procedure Execute(Address: String);
  end;

implementation

uses EnumsUnit;

procedure TForwardGeocodeAddress.Execute(Address: String);
var
  ErrorString: String;
begin
  Route4MeManager.Geocoding.ForwardGeocodeAddress(Address, ErrorString);
  try
    WriteLn('');

{    if (AvoidanceZone <> nil) then
    begin
      WriteLn('AddAvoidanceZone executed successfully');
      WriteLn(Format('Territory ID: %s', [AvoidanceZone.TerritoryId.Value]));

      Result := AvoidanceZone.TerritoryId;
    end
    else
      WriteLn(Format('AddAvoidanceZone error: "%s"', [ErrorString]));}
  finally
//    FreeAndNil(AvoidanceZone);
  end;
end;

end.
