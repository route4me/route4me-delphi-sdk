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
  Orders: TOrderArray;
  i: integer;
  Offset, Limit: integer;
  Total: integer;
begin
  Offset := 0;
  Limit := 10;
  Orders := Route4MeManager.Order.GetOrdersWithSpecifiedText(
    SpecifiedText, Offset, Limit, Total, ErrorString);
  try
    WriteLn('');

    if (Length(Orders) > 0) then
      WriteLn(Format(
        'GetOrdersWithSpecifiedText executed successfully, %d orders returned, %d total',
          [Length(Orders), Total]))
    else
      WriteLn(Format('GetOrdersWithSpecifiedText error: "%s"', [ErrorString]));
  finally
    for i := Length(Orders) - 1 downto 0 do
      FreeAndNil(Orders[i]);
  end;
end;

end.
