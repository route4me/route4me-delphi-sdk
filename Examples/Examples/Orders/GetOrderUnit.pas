unit GetOrderUnit;

interface

uses SysUtils, BaseExampleUnit, OrderUnit;

type
  TGetOrder = class(TBaseExample)
  public
    procedure Execute(OrderId: String);
  end;

implementation

procedure TGetOrder.Execute(OrderId: String);
var
  ErrorString: String;
  Order: TOrder;
begin
  Order := Route4MeManager.Order.Get(OrderId, ErrorString);
  try
    WriteLn('');

    if (ErrorString = EmptyStr) then
      WriteLn('GetOrder executed successfully')
    else
      WriteLn(Format('GetOrder error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Order);
  end;
end;

end.
