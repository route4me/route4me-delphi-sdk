unit AddOrderUnit;

interface

uses SysUtils, BaseExampleUnit, OrderUnit;

type
  TAddOrder = class(TBaseExample)
  public
    function Execute: TOrder;
  end;

implementation

function TAddOrder.Execute: TOrder;
var
  ErrorString: String;
  Order: TOrder;
begin
  Order := TOrder.Create();
  try
    Order.Address1 := 'Test Address1';
    Order.AddressAlias := 'Test AddressAlias';
    Order.FirstName := 'John';
    Order.CachedLatitude := 37.773972;
    Order.CachedLongitude := -122.431297;

    Result := Route4MeManager.Order.Add(Order, ErrorString);

    WriteLn('');

    if (Result <> nil) then
    begin
      WriteLn('AddOrder executed successfully');
      WriteLn(Format('Order ID: %s', [Result.OrderId.Value]));
    end
    else
      WriteLn(Format('AddOrder error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Order);
  end;
end;

end.
