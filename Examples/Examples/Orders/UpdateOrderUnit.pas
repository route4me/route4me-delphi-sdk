unit UpdateOrderUnit;

interface

uses SysUtils, BaseExampleUnit, OrderUnit;

type
  TUpdateOrder = class(TBaseExample)
  public
    procedure Execute(Order: TOrder);
  end;

implementation

procedure TUpdateOrder.Execute(Order: TOrder);
var
  ErrorString: String;
  UpdatedOrder: TOrder;
begin
  UpdatedOrder := Route4MeManager.Order.Update(Order, ErrorString);
  try
    WriteLn('');

    if (UpdatedOrder <> nil) then
      WriteLn('UpdateOrder executed successfully')
    else
      WriteLn(Format('UpdateOrder error: "%s"', [ErrorString]));
  finally
    FreeAndNil(UpdatedOrder);
  end;
end;

end.
