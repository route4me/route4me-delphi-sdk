unit GetOrdersWithCustomFieldsUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetOrdersWithCustomFields = class(TBaseExample)
  public
    procedure Execute(Fields: String);
  end;

implementation

uses CommonTypesUnit;

procedure TGetOrdersWithCustomFields.Execute(Fields: String);
var
  ErrorString: String;
  Orders: TIntegerArray;
  i: integer;
  Offset, Limit: integer;
  Total: integer;
begin
  Offset := 0;
  Limit := 10;
  Orders := Route4MeManager.Order.GetOrdersWithCustomFields(
    Fields, Offset, Limit, Total, ErrorString);
  try
    WriteLn('');

    if (Length(Orders) > 0) then
      WriteLn(Format(
        'GetOrdersWithCustomFields executed successfully, %d orders returned, %d total',
          [Length(Orders), Total]))
    else
      WriteLn(Format('GetOrdersWithCustomFields error: "%s"', [ErrorString]));
  finally
    for i := Length(Orders) - 1 downto 0 do
      FreeAndNil(Orders[i]);
  end;
end;

end.
