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
  Orders: TOrderArray;
  i: integer;
begin
  Orders := Route4MeManager.Order.Get(Date, ErrorString);
  try
    WriteLn('');

    if (Length(Orders) > 0) then
      WriteLn(Format(
        'GetOrderByDate executed successfully, %d orders returned', [Length(Orders)]))
    else
      WriteLn(Format('GetOrderByDate error: "%s"', [ErrorString]));
  finally
    for i := Length(Orders) - 1 downto 0 do
      FreeAndNil(Orders[i]);
  end;
end;

end.
