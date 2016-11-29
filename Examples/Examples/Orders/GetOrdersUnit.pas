unit GetOrdersUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetOrders = class(TBaseExample)
  public
    procedure Execute;
  end;

implementation

uses OrderParametersUnit, OrderUnit;

procedure TGetOrders.Execute;
var
  ErrorString: String;
  Parameters: TOrderParameters;
  Total: integer;
  Orders: TOrderList;
begin
  Parameters := TOrderParameters.Create();
  try
    Parameters.Limit := 10;

    Orders := Route4MeManager.Order.Get(Parameters, Total, ErrorString);
    try
      WriteLn('');

      if (Orders.Count > 0) then
        WriteLn(Format(
          'GetOrders executed successfully, %d orders returned, total = %d',
          [Orders.Count, Total]))
      else
        WriteLn(Format('GetOrders error: "%s"', [ErrorString]));
    finally
      FreeAndNil(Orders);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
