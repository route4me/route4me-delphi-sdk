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
  Parameters: TAddOrderToRouteRequest;
  Address: TOrderedAddress;
begin
  Parameters := TAddOrderToRouteRequest.Create;
  try
    Parameters.RouteId := RouteId;
    Parameters.Redirect := False;
    Parameters.Parameters := RouteParameters;
    for Address in OrderedAddresses do
      Parameters.AddAddress(Address);

    Route4MeManager.Route.AddOrder(Parameters, ErrorString);

    WriteLn('');

    if (ErrorString = EmptyStr) then
      WriteLn('AddOrderToRoute executed successfully')
    else
      WriteLn(Format('AddOrderToRoute error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
