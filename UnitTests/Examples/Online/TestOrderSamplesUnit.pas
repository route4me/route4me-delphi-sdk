unit TestOrderSamplesUnit;

interface

uses
  TestFramework, Classes, SysUtils, DateUtils,
  BaseTestOnlineExamplesUnit, NullableBasicTypesUnit, OrderUnit;

type
  TTestOrderSamples = class(TTestOnlineExamples)
  private
    function GetTestOrder: TOrder;
  published
    procedure AddNewOrder;
    procedure GetOrderById;
    procedure GetAllOrders;
    procedure ScheduleOrder;
    procedure GetOrdersByDate;
    procedure GetOrdersScheduledFor;
    procedure GetOrdersWithSpecifiedText;

    procedure GetOrdersWithCustomFields;
    procedure AddOrderToOptimization;

    procedure UpdateOrder;
    procedure RemoveOrder;
  end;


implementation

{ TTestMemberSamples }

uses UserParametersUnit, UserParameterProviderUnit, UserUnit, EnumsUnit,
  OrderParametersUnit, CommonTypesUnit, DataObjectUnit,
  AddOrderToRouteRequestUnit;

var
  FOrderId: NullableInteger;

{ TTestOrderSamples }

procedure TTestOrderSamples.AddNewOrder;
var
  Order: TOrder;
  AddedOrder: TOrder;
  ErrorString: String;
begin
  Order := GetTestOrder;
  try
    // Correct adding new order. Must be success.
    AddedOrder := FRoute4MeManager.Order.Add(Order, ErrorString);
    try
      CheckNotNull(AddedOrder);
      CheckEquals(EmptyStr, ErrorString);
      CheckTrue(AddedOrder.Id.IsNotNull);

      FOrderId := AddedOrder.Id;
    finally
      FreeAndNil(AddedOrder);
    end;
  finally
    FreeAndNil(Order);
  end;
end;

procedure TTestOrderSamples.AddOrderToOptimization;
begin
// todo 4: сделать unit-тест
end;

procedure TTestOrderSamples.GetAllOrders;
var
  Parameters: TOrderParameters;
  Total: integer;
  ErrorString: String;
  Orders: TOrderList;
  OrderIds: TIntegerArray;
  i: integer;
begin
  Parameters := TOrderParameters.Create;
  try
    Parameters.Limit := 5;
    Orders := FRoute4MeManager.Order.Get(Parameters, Total, ErrorString);
    try
      CheckNotNull(Orders);
      CheckEquals(EmptyStr, ErrorString);
      CheckEquals(5, Orders.Count);
      CheckTrue(Total > 0);

      SetLength(OrderIds, Orders.Count);
      for i := 0 to Orders.Count - 1 do
        OrderIds[i] := Orders[i].Id;
    finally
      FreeAndNil(Orders);
    end;

    try
      Parameters.Limit := 2;
      Orders := FRoute4MeManager.Order.Get(Parameters, Total, ErrorString);
      CheckNotNull(Orders);
      CheckEquals(EmptyStr, ErrorString);
      CheckEquals(2, Orders.Count);
      CheckTrue(Total > 0);
      CheckTrue(OrderIds[0] = Orders[0].Id);
      CheckTrue(OrderIds[1] = Orders[1].Id);
    finally
      FreeAndNil(Orders);
    end;

    try
      Parameters.Limit := 2;
      Parameters.Offset := 2;
      Orders := FRoute4MeManager.Order.Get(Parameters, Total, ErrorString);
      CheckNotNull(Orders);
      CheckEquals(EmptyStr, ErrorString);
      CheckEquals(2, Orders.Count);
      CheckTrue(Total > 0);
      CheckTrue(OrderIds[2] = Orders[0].Id);
      CheckTrue(OrderIds[3] = Orders[1].Id);
    finally
      FreeAndNil(Orders);
    end;

    try
      Parameters.Limit := 2;
      Parameters.Offset := Total;
      Orders := FRoute4MeManager.Order.Get(Parameters, Total, ErrorString);
      CheckNotNull(Orders);
      CheckEquals(EmptyStr, ErrorString);
      CheckEquals(0, Orders.Count);
      CheckTrue(Total > 0);
    finally
      FreeAndNil(Orders);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

procedure TTestOrderSamples.GetOrderById;
var
  Order: TOrder;
  ErrorString: String;
begin
  // Correct OrderId. Must be success.
  Order := FRoute4MeManager.Order.Get(FOrderId, ErrorString);
  try
    CheckNotNull(Order);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Order);
  end;

  // Invalid OrderId. Must be error.
  Order := FRoute4MeManager.Order.Get(-1, ErrorString);
  try
    CheckNull(Order);
    CheckNotEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Order);
  end;
end;

procedure TTestOrderSamples.GetOrdersByDate;
var
  ErrorString: String;
  Date: TDate;
  Orders: TOrderList;
  i: integer;
  IsFound: boolean;
begin
  // Correct date. Must be success.
  // todo 3: время надо перевести в серверное (по Штатам, похоже)
  Date := Now();
  Orders := FRoute4MeManager.Order.Get(Date, ErrorString);
  try
    CheckNotNull(Orders);
    CheckTrue(Orders.Count > 0);
    CheckEquals(EmptyStr, ErrorString);

    IsFound := False;
    for i := 0 to Orders.Count - 1 do
      if (Orders[i].Id = FOrderId) then
      begin
        IsFound := True;
        Break;
      end;
    CheckTrue(IsFound);
  finally
    FreeAndNil(Orders);
  end;

//  DONE 5: Олега спросил почему для дат из будущего и прошлого список заказов не пуст. Ответа нет.
{  Date := EncodeDateTime(2216, 06, 15, 0, 0, 0, 0);
  // Invalid date. Must be error.
  Orders := FRoute4MeManager.Order.Get(Date, ErrorString);
  try
    CheckNotNull(Orders);
    CheckEquals(0, Orders.Count);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Orders);
  end;}
end;

procedure TTestOrderSamples.GetOrdersScheduledFor;
var
  ErrorString: String;
  Orders: TOrderList;
  IsFound: boolean;
  i: integer;
begin
  // Correct date. Must be success.
  Orders := FRoute4MeManager.Order.GetOrdersScheduledFor(Tomorrow, ErrorString);
  try
    CheckNotNull(Orders);
    CheckTrue(Orders.Count > 0);
    CheckEquals(EmptyStr, ErrorString);

    IsFound := False;
    for i := 0 to Orders.Count - 1 do
      if (Orders[i].Id = FOrderId) then
      begin
        IsFound := True;
        Break;
      end;
    CheckTrue(IsFound);
  finally
    FreeAndNil(Orders);
  end;

// DONE 5: Олега спросил почему для дат из будущего и прошлого список заказов не пуст. Ответа нет.
{  // Invalid date. Must be error.
  Orders := FRoute4MeManager.Order.GetOrdersScheduledFor(IncDay(Now, -2), ErrorString);
  try
    CheckNotNull(Orders);
    CheckEquals(0, Orders.Count);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Orders);
  end;}
end;

procedure TTestOrderSamples.GetOrdersWithCustomFields;
begin
// todo 4: сделать unit-тест (нужен парсинг из многомерного массива)
end;

procedure TTestOrderSamples.GetOrdersWithSpecifiedText;
var
  ErrorString: String;
  Text: String;
  Orders: TOrderList;
  Offset, Limit, Total: integer;
  Order: TOrder;
begin
  Text := 'Some Unique Text S34Ds2';
  Limit := 5;
  Offset := 0;
  Orders := FRoute4MeManager.Order.GetOrdersWithSpecifiedText(
    Text, Limit, Offset, Total, ErrorString);
  try
    CheckNotNull(Orders);
    CheckEquals(0, Orders.Count);
    CheckEquals(EmptyStr, ErrorString);
    CheckEquals(0, Total);
  finally
    FreeAndNil(Orders);
  end;

  Order := FRoute4MeManager.Order.Get(FOrderId, ErrorString);
  try
    CheckEquals(EmptyStr, ErrorString);

    // By full LastName
    Text := Order.LastName;
    Limit := 5;
    Offset := 0;
    Orders := FRoute4MeManager.Order.GetOrdersWithSpecifiedText(
      Text, Limit, Offset, Total, ErrorString);
    try
      CheckNotNull(Orders);
      CheckEquals(1, Orders.Count);
      CheckEquals(EmptyStr, ErrorString);
      CheckEquals(1, Total);
    finally
      FreeAndNil(Orders);
    end;

    // The part of FirstName
    Text := Copy(Order.FirstName, 2, Length(Order.FirstName) - 2);
    Limit := 5;
    Offset := 0;
    Orders := FRoute4MeManager.Order.GetOrdersWithSpecifiedText(
      Text, Limit, Offset, Total, ErrorString);
    try
      CheckNotNull(Orders);
      CheckEquals(1, Orders.Count);
      CheckEquals(EmptyStr, ErrorString);
      CheckEquals(1, Total);
    finally
      FreeAndNil(Orders);
    end;

    // Text with spaces
    Text := Order.Address1;
    Limit := 5;
    Offset := 0;
    Orders := FRoute4MeManager.Order.GetOrdersWithSpecifiedText(
      Text, Limit, Offset, Total, ErrorString);
    try
      CheckNotNull(Orders);
      CheckEquals(1, Orders.Count);
      CheckEquals(EmptyStr, ErrorString);
      CheckEquals(1, Total);
    finally
      FreeAndNil(Orders);
    end;
  finally
    FreeAndNil(Order);
  end;
end;

function TTestOrderSamples.GetTestOrder: TOrder;
var
  Rnd: String;
begin
  Randomize;
  Rnd := IntToStr(Random(100000));

  Result := TOrder.Create;
  Result.Address1 := 'Test Address2' + Rnd;
  Result.AddressAlias := 'Test AddressAlias' + Rnd;
  Result.FirstName := 'Jefferson' + Rnd;
  Result.LastName := 'Cruse' + Rnd;
  Result.CachedLatitude := 37.773972;
  Result.CachedLongitude := -122.431297;
end;

procedure TTestOrderSamples.RemoveOrder;
var
  ErrorString: String;
begin
  // Deleting existing order. Must be success.
  CheckTrue(FRoute4MeManager.Order.Remove([FOrderId], ErrorString));
  CheckEquals(EmptyStr, ErrorString);

  // Deleting unexisting order. Must be success.
  CheckTrue(FRoute4MeManager.Order.Remove([-1], ErrorString));
  CheckEquals(EmptyStr, ErrorString);
end;

procedure TTestOrderSamples.ScheduleOrder;
var
  ErrorString: String;
begin
  // Set invalid date. Must be error.
  FRoute4MeManager.Order.ScheduleOrder(FOrderId, IncDay(Now, -10000), ErrorString);
  CheckNotEquals(EmptyStr, ErrorString);

  // Set correct date. Must be success.
  FRoute4MeManager.Order.ScheduleOrder(FOrderId, Tomorrow, ErrorString);
  CheckEquals(EmptyStr, ErrorString);
end;

procedure TTestOrderSamples.UpdateOrder;
var
  Order: TOrder;
  UpdatedOrder: TOrder;
  ErrorString: String;
begin
  Order := GetTestOrder();
  try
    Order.Id := FOrderId;
    Order.FirstName := 'Mary';

    // Correct updating order. Must be success.
    UpdatedOrder := FRoute4MeManager.Order.Update(Order, ErrorString);
    try
      CheckNotNull(UpdatedOrder);
      CheckEquals(EmptyStr, ErrorString);
      CheckEquals('Mary', UpdatedOrder.FirstName);
      CheckTrue(FOrderId.IsNotNull);
    finally
      FreeAndNil(UpdatedOrder);
    end;

    // Invalid OrderId. Must be error.
    Order.Id := -1;
    UpdatedOrder := FRoute4MeManager.Order.Update(Order, ErrorString);
    try
      CheckNull(UpdatedOrder);
      CheckNotEquals(EmptyStr, ErrorString);
    finally
      FreeAndNil(UpdatedOrder);
    end;
  finally
    FreeAndNil(Order);
  end;
end;

initialization
  RegisterTest('Examples\Online\Orders\', TTestOrderSamples.Suite);
end.
