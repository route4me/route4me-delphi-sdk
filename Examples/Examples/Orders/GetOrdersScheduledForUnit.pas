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
  Orders: TOrderArray;
  i: integer;
begin
  Orders := Route4MeManager.Order.GetOrdersScheduledFor(Date, ErrorString);
  try
    WriteLn('');

    if (Length(Orders) > 0) then
      WriteLn(Format(
        'GetOrdersScheduledFor executed successfully, %d orders returned', [Length(Orders)]))
    else
      WriteLn(Format('GetOrdersScheduledFor error: "%s"', [ErrorString]));
  finally
    for i := Length(Orders) - 1 downto 0 do
      FreeAndNil(Orders[i]);
  end;
end;

end.
