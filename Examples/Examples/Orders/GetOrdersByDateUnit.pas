unit GetOrdersByDateUnit;

interface

uses SysUtils, BaseExampleUnit, OrderUnit;

type
  TGetOrdersByDate = class(TBaseExample)
  public
    procedure Execute(Date: TDate);
  end;

implementation

procedure TGetOrdersByDate.Execute(Date: TDate);
var
  ErrorString: String;
  Orders: TOrderList;
begin
  Orders := Route4MeManager.Order.Get(Date, ErrorString);
  try
    WriteLn('');

    if (Orders.Count > 0) then
      WriteLn(Format(
        'GetOrderByDate executed successfully, %d orders returned', [Orders.Count]))
    else
      WriteLn(Format('GetOrderByDate error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Orders);
  end;
end;

end.
