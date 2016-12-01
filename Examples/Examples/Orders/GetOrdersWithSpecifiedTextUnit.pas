unit GetOrdersWithSpecifiedTextUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetOrdersWithSpecifiedText = class(TBaseExample)
  public
    procedure Execute(SpecifiedText: String);
  end;

implementation

uses CommonTypesUnit, OrderUnit;

procedure TGetOrdersWithSpecifiedText.Execute(SpecifiedText: String);
var
  ErrorString: String;
  Orders: TOrderList;
  Offset, Limit: integer;
  Total: integer;
begin
  Limit := 10;
  Offset := 0;
  Orders := Route4MeManager.Order.GetOrdersWithSpecifiedText(
    SpecifiedText, Limit, Offset, Total, ErrorString);
  try
    WriteLn('');

    if (Orders.Count > 0) then
      WriteLn(Format(
        'GetOrdersWithSpecifiedText executed successfully, %d orders returned, %d total',
          [Orders.Count, Total]))
    else
      WriteLn(Format('GetOrdersWithSpecifiedText error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Orders);
  end;
end;

end.
