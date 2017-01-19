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
  Result := '{"results": [[7205711, 1],[7205710, 2],[7205709, 3],[7205708, 4]],"total": 148,"fields": ["order_id","member_id"]}';
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
    CheckEquals(4, Actual.ResultCount);
    CheckEquals('7205711', Actual.GetResult(0, 'order_id'));
    CheckEquals('1', Actual.GetResult(0, 'member_id'));
    CheckEquals('7205710', Actual.GetResult(1, 'order_id'));
    CheckEquals('2', Actual.GetResult(1, 'member_id'));
    CheckEquals('7205709', Actual.GetResult(2, 'order_id'));
    CheckEquals('3', Actual.GetResult(2, 'member_id'));
    CheckEquals('7205708', Actual.GetResult(3, 'order_id'));
    CheckEquals('4', Actual.GetResult(3, 'member_id'));
  finally
    FreeAndNil(JsonValue);
  end;
end;

initialization
  RegisterTest('JSON\Unmarshal\', TTestUnmarshalMultipleArray.Suite);
end.
