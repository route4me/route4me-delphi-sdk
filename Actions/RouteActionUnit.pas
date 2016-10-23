unit RouteActionUnit;

interface

uses BaseActionUnit, DataObjectUnit, RouteParametersUnit, GenericParametersUnit,
  AddressesOrderInfoUnit;

type
  TRouteActions = class(TBaseAction)
  public
    function Resequence(AddressesOrderInfo: TAddressesOrderInfo;
      out ErrorString: String): TDataObjectRoute;
  end;

implementation

{ TRouteActions }

uses SettingsUnit;

function TRouteActions.Resequence(
  AddressesOrderInfo: TAddressesOrderInfo;
  out ErrorString: String): TDataObjectRoute;
begin
  Result := FConnection.Put(TSettings.RouteHost,
    AddressesOrderInfo, TDataObjectRoute, errorString) as TDataObjectRoute;
end;

end.
