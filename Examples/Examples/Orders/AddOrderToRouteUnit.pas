unit AddOrderToRouteUnit;

interface

uses SysUtils, BaseExampleUnit, RouteParametersUnit, AddressUnit;

type
  TAddOrderToRoute = class(TBaseExample)
  public
    procedure Execute(RouteId: String; RouteParameters: TRouteParameters;
      OrderedAddresses: TOrderedAddressArray);
  end;

implementation

uses
  AddOrderToRouteRequestUnit;

procedure TAddOrderToRoute.Execute(RouteId: String;
  RouteParameters: TRouteParameters; OrderedAddresses: TOrderedAddressArray);
var
  ErrorString: String;
begin
  Route4MeManager.Route.AddOrder(RouteId, RouteParameters, OrderedAddresses, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
    WriteLn('AddOrderToRoute executed successfully')
  else
    WriteLn(Format('AddOrderToRoute error: "%s"', [ErrorString]));
end;

end.
