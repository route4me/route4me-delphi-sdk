unit RemoveOrdersUnit;

interface

uses SysUtils, BaseExampleUnit, CommonTypesUnit, OrderUnit;

type
  TRemoveOrders = class(TBaseExample)
  public
    procedure Execute(OrderIds: TStringArray);
  end;

implementation

uses OrderParametersUnit;

procedure TRemoveOrders.Execute(OrderIds: TStringArray);
var
  ErrorString: String;
  Removed: boolean;
begin
  Removed := Route4MeManager.Order.Remove(OrderIds, ErrorString);

  WriteLn('');

  if (Removed) then
    WriteLn(Format('RemoveOrders executed successfully, %d orders removed',
      [Length(OrderIds)]))
  else
    WriteLn(Format('RemoveOrders error: "%s"', [ErrorString]));
end;

end.
