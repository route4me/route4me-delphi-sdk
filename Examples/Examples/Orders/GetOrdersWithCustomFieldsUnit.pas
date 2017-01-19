unit GetOrdersWithCustomFieldsUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetOrdersWithCustomFields = class(TBaseExample)
  public
    procedure Execute(Fields: TArray<String>);
  end;

implementation

uses CommonTypesUnit, OrderActionsUnit;

procedure TGetOrdersWithCustomFields.Execute(Fields: TArray<String>);
var
  ErrorString: String;
  Orders: TOrdersCustomFields;
  Offset, Limit: integer;
  Total: integer;
begin
  Limit := 10;
  Offset := 0;
  Orders := Route4MeManager.Order.GetOrdersWithCustomFields(
    Fields, Limit, Offset, Total, ErrorString);
  try
    WriteLn('');

    if (Orders.Count > 0) then
      WriteLn(Format(
        'GetOrdersWithCustomFields executed successfully, %d orders returned, %d total',
          [Orders.Count, Total]))
    else
      WriteLn(Format('GetOrdersWithCustomFields error: "%s"', [ErrorString]));
  finally
      FreeAndNil(Orders);
  end;
end;

end.
