unit TestUnmarshalMultipleArrayUnit;

interface

uses
  TestFramework, System.JSON, SysUtils;

type
  TTestUnmarshalMultipleArray = class(TTestCase)
  private
    function Etalon: String;
  published
    procedure TestUnmarshal();
  end;

implementation

{ TTestUnmarshalMultipleArray }

uses GetOrdersWithCustomFieldsResponseUnit, MarshalUnMarshalUnit;

function TTestUnmarshalMultipleArray.Etalon: String;
begin
  Result := '{"results": [[7205711, 1],[7205710, 1],[7205709, 1],[7205708, 1]],"total": 148,"fields": ["order_id","member_id"]}';
end;

procedure TTestUnmarshalMultipleArray.TestUnmarshal;
var
  Actual: TGetOrdersWithCustomFieldsResponse;
  JsonValue: TJSONValue;
begin
  JsonValue := TJSONObject.ParseJSONValue(Etalon);
  try
    Actual := TMarshalUnMarshal.FromJson(
      TGetOrdersWithCustomFieldsResponse, JsonValue) as TGetOrdersWithCustomFieldsResponse;

    CheckEquals(148, Actual.Total);
    CheckEquals(2, Length(Actual.Fields));
    CheckEquals('order_id', Actual.Fields[0]);
    CheckEquals('member_id', Actual.Fields[1]);
    CheckEquals(4, Actual.OrdersCount);
    CheckEquals(7205711, Actual.OrderId[0]);
    CheckEquals(7205710, Actual.OrderId[1]);
    CheckEquals(7205709, Actual.OrderId[2]);
    CheckEquals(7205708, Actual.OrderId[3]);
  finally
    FreeAndNil(JsonValue);
  end;
end;

initialization
  RegisterTest('JSON\Unmarshal\', TTestUnmarshalMultipleArray.Suite);
end.
