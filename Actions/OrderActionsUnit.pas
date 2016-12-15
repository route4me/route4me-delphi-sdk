unit OrderActionsUnit;

interface

uses
  SysUtils, BaseActionUnit,
  OrderUnit, OrderParametersUnit, CommonTypesUnit;

type
  TOrderActions = class(TBaseAction)
  private
    function GetDateStr(Date: TDate): String;
  public
    function Get(OrderQuery: TOrderParameters;
      out Total: integer; out ErrorString: String): TOrderList; overload;

    function Get(OrderId: integer; out ErrorString: String): TOrder; overload;

    /// <summary>
    ///  Retrieve orders inserted on a specified date.
    /// </summary>
    function Get(AddedDate: TDate; out ErrorString: String): TOrderList; overload;

    /// <summary>
    ///  Retrieve orders scheduled for a specified date.
    /// </summary>
    function GetOrdersScheduledFor(ScheduledDate: TDate; out ErrorString: String): TOrderList;

    /// <summary>
    ///  Searching all Orders with specified custom fields
    /// </summary>
    function GetOrdersWithCustomFields(Fields: String; Limit, Offset: integer;
      out Total: integer; out ErrorString: String): TIntegerArray;

    /// <summary>
    ///  Search for all order records which contain specified text in any field
    /// </summary>
    function GetOrdersWithSpecifiedText(SpecifiedText: String; Limit, Offset: integer;
      out Total: integer; out ErrorString: String): TOrderList;

    function Add(Order: TOrder; out ErrorString: String): TOrder;

    function Update(Order: TOrder; out ErrorString: String): TOrder;

    function Remove(OrderIds: TIntegerArray; out ErrorString: String): boolean;

    procedure ScheduleOrder(OrderId: integer; ScheduleDate: TDate; out ErrorString: String);
  end;

implementation

{ TOrderActions }

uses
  System.NetEncoding, SettingsUnit, GetOrdersResponseUnit,
  RemoveOrdersRequestUnit, StatusResponseUnit, GenericParametersUnit,
  GetOrdersWithCustomFieldsResponseUnit;

function TOrderActions.Add(Order: TOrder; out ErrorString: String): TOrder;
begin
  Result := FConnection.Post(TSettings.EndPoints.Order, Order,
    TOrder, ErrorString) as TOrder;
end;

function TOrderActions.Get(OrderQuery: TOrderParameters;
  out Total: integer; out ErrorString: String): TOrderList;
var
  Response: TGetOrdersResponse;
  i: integer;
begin
  Result := TOrderList.Create;

  Total := 0;
  Response := FConnection.Get(TSettings.EndPoints.Order, OrderQuery,
    TGetOrdersResponse, ErrorString) as TGetOrdersResponse;
  try
    if (Response <> nil) then
    begin
      for i := 0 to Length(Response.Results) - 1 do
        Result.Add(Response.Results[i]);
      Total := Response.Total;
    end;
  finally
    FreeAndNil(Response);
  end;
end;

function TOrderActions.Get(OrderId: integer; out ErrorString: String): TOrder;
var
  Request: TGenericParameters;
begin
  Request := TGenericParameters.Create;
  try
    Request.AddParameter('order_id', IntToStr(OrderId));

    Result := FConnection.Get(TSettings.EndPoints.Order, Request, TOrder, ErrorString) as TOrder;

    if (Result = nil) and (ErrorString = EmptyStr) then
      ErrorString := 'Order details not got';
  finally
    FreeAndNil(Request);
  end;
end;

function TOrderActions.Remove(OrderIds: TIntegerArray;
  out ErrorString: String): boolean;
var
  Request: TRemoveOrdersRequest;
  Response: TStatusResponse;
begin
  Request := TRemoveOrdersRequest.Create();
  try
    Request.OrderIds := OrderIds;

    Response := FConnection.Delete(TSettings.EndPoints.Order, Request,
      TStatusResponse, ErrorString) as TStatusResponse;
    try
      Result := (Response <> nil) and (Response.Status);
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

procedure TOrderActions.ScheduleOrder(OrderId: integer; ScheduleDate: TDate;
  out ErrorString: String);
var
  Order: TOrder;
  UpdatedOrder: TOrder;
begin
  Order := Get(OrderId, ErrorString);
  try
    if (Order = nil) then
      Exit;

    Order.ScheduleDate := ScheduleDate;
    UpdatedOrder := Update(Order, ErrorString);
    try
      if (Order.ScheduleDate <> UpdatedOrder.ScheduleDate) then
        ErrorString := 'Set schedule order error';
    finally
      FreeAndNil(UpdatedOrder);
    end;
  finally
    FreeAndNil(Order);
  end;
end;

function TOrderActions.Update(Order: TOrder; out ErrorString: String): TOrder;
begin
  Result := FConnection.Put(TSettings.EndPoints.Order, Order, TOrder, ErrorString) as TOrder;
end;

function TOrderActions.Get(AddedDate: TDate; out ErrorString: String): TOrderList;
var
  Response: TGetOrdersResponse;
  Request: TGenericParameters;
  i: integer;
begin
  Result := TOrderList.Create;

  Request := TGenericParameters.Create;
  try
    Request.AddParameter('day_added_YYMMDD', GetDateStr(AddedDate));

    Response := FConnection.Get(TSettings.EndPoints.Order, Request,
      TGetOrdersResponse, ErrorString) as TGetOrdersResponse;
    try
      if (Response <> nil) then
        for i := 0 to Length(Response.Results) - 1 do
          Result.Add(Response.Results[i])
      else
      if (ErrorString = EmptyStr) then
        ErrorString := 'Order details not got';
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

function TOrderActions.GetDateStr(Date: TDate): String;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings := TFormatSettings.Create;
  FormatSettings.ShortDateFormat := 'yyyy-mm-dd';
  FormatSettings.DateSeparator := '-';

  Result := DateToStr(Date, FormatSettings);
end;

function TOrderActions.GetOrdersScheduledFor(ScheduledDate: TDate;
  out ErrorString: String): TOrderList;
var
  Response: TGetOrdersResponse;
  Request: TGenericParameters;
  i: integer;
begin
  Result := TOrderList.Create;

  Request := TGenericParameters.Create;
  try
    Request.AddParameter('scheduled_for_YYMMDD', GetDateStr(ScheduledDate));

    Response := FConnection.Get(TSettings.EndPoints.Order, Request,
      TGetOrdersResponse, ErrorString) as TGetOrdersResponse;
    try
      if (Response <> nil) then
        for i := 0 to Length(Response.Results) - 1 do
          Result.Add(Response.Results[i])
      else
      if (ErrorString = EmptyStr) then
        ErrorString := 'Order details not got';
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

function TOrderActions.GetOrdersWithCustomFields(Fields: String;
  Limit, Offset: integer; out Total: integer; out ErrorString: String): TIntegerArray;
var
  Response: TGetOrdersWithCustomFieldsResponse;
  Request: TGenericParameters;
  i: integer;
begin
  SetLength(Result, 0);
  Total := 0;

  Request := TGenericParameters.Create;
  try
    Request.AddParameter('fields', Fields);
    Request.AddParameter('offset', IntToStr(Offset));
    Request.AddParameter('limit', IntToStr(Limit));

    Response := FConnection.Get(TSettings.EndPoints.Order, Request,
      TGetOrdersWithCustomFieldsResponse, ErrorString) as TGetOrdersWithCustomFieldsResponse;
    try
      if (Response <> nil) then
      begin
        SetLength(Result, Response.OrdersCount);
        for i := 0 to Response.OrdersCount - 1 do
          Result[i] := Response.OrderId[i];
        Total := Response.Total;
      end
      else
      if (ErrorString = EmptyStr) then
        ErrorString := 'Order details not got';
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

function TOrderActions.GetOrdersWithSpecifiedText(SpecifiedText: String; Limit,
  Offset: integer; out Total: integer; out ErrorString: String): TOrderList;
var
  Response: TGetOrdersResponse;
  Request: TGenericParameters;
  i: integer;
begin
  Result := TOrderList.Create;
  Total := 0;

  Request := TGenericParameters.Create;
  try
    Request.AddParameter('query', TNetEncoding.URL.Encode(SpecifiedText));
    Request.AddParameter('offset', IntToStr(Offset));
    Request.AddParameter('limit', IntToStr(Limit));

    Response := FConnection.Get(TSettings.EndPoints.Order, Request,
      TGetOrdersResponse, ErrorString) as TGetOrdersResponse;
    try
      if (Response <> nil) then
      begin
        for i := 0 to Length(Response.Results) - 1 do
          Result.Add(Response.Results[i]);

        Total := Response.Total;
      end
      else
      if (ErrorString = EmptyStr) then
        ErrorString := 'Order details not got';
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

end.
