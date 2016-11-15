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
  Orders: TOrderArray;
  i: integer;
begin
  Parameters := TOrderParameters.Create();
  try
    Parameters.Limit := 10;

    Orders := Route4MeManager.Order.Get(Parameters, Total, ErrorString);
    try
      WriteLn('');

      if (Length(Orders) > 0) then
        WriteLn(Format(
          'GetOrders executed successfully, %d orders returned, total = %d',
          [Length(Orders), Total]))
      else
        WriteLn(Format('GetOrders error: "%s"', [ErrorString]));
    finally
      for i := Length(Orders) - 1 downto 0 do
        FreeAndNil(Orders[i]);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
