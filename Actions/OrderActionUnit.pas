unit OrderActionUnit;

interface

uses
  SysUtils, BaseActionUnit,
  OrderUnit, OrderParametersUnit, CommonTypesUnit;

type
  TOrderActions = class(TBaseAction)
  public
    function Get(OrderQuery: TOrderParameters;
      out Total: integer; out ErrorString: String): TOrderArray;

    function Add(Order: TOrder; out ErrorString: String): TOrder;

    function Update(Order: TOrder; out ErrorString: String): TOrder;

    function Remove(OrderIds: TStringArray; out ErrorString: String): boolean;
  end;

implementation

{ TOrderActions }

uses
  SettingsUnit, GetOrdersResponseUnit, RemoveOrdersRequestUnit,
  StatusResponseUnit;

function TOrderActions.Add(Order: TOrder; out ErrorString: String): TOrder;
begin
  Result := FConnection.Post(TSettings.Order, Order,
    TOrder, ErrorString) as TOrder;
end;

function TOrderActions.Get(OrderQuery: TOrderParameters;
  out Total: integer; out ErrorString: String): TOrderArray;
var
  Response: TGetOrdersResponse;
begin
  SetLength(Result, 0);

  Total := 0;
  Response := FConnection.Get(TSettings.Order, OrderQuery,
    TGetOrdersResponse, ErrorString) as TGetOrdersResponse;
  try
    if (Response <> nil) then
    begin
      Result := Response.Results;
      Total := Response.Total;
    end;
  finally
    FreeAndNil(Response);
  end;
end;

function TOrderActions.Remove(OrderIds: TStringArray;
  out ErrorString: String): boolean;
var
  Request: TRemoveOrdersRequest;
  Response: TStatusResponse;
begin
  Request := TRemoveOrdersRequest.Create();
  try
    Request.OrderIds := OrderIds;

    Response := FConnection.Delete(TSettings.Order, Request,
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

function TOrderActions.Update(Order: TOrder; out ErrorString: String): TOrder;
begin
  Result := FConnection.Put(TSettings.Order, Order, TOrder, ErrorString) as TOrder;
end;

end.
