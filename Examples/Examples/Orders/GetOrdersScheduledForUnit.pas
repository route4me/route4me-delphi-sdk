unit GetOrdersScheduledForUnit;

interface

uses SysUtils, BaseExampleUnit, OrderUnit;

type
  TGetOrdersScheduledFor = class(TBaseExample)
  public
    procedure Execute(Date: TDate);
  end;

implementation

procedure TGetOrdersScheduledFor.Execute(Date: TDate);
var
  ErrorString: String;
  Orders: TOrderList;
begin
  Orders := Route4MeManager.Order.GetOrdersScheduledFor(Date, ErrorString);
  try
    WriteLn('');

    if (Orders.Count > 0) then
      WriteLn(Format(
        'GetOrdersScheduledFor executed successfully, %d orders returned', [Orders.Count]))
    else
      WriteLn(Format('GetOrdersScheduledFor error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Orders);
  end;
end;

end.
