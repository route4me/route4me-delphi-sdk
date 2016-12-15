unit GetOrdersWithCustomFieldsResponseUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit, OrderUnit, CommonTypesUnit;

type
  TGetOrdersWithCustomFieldsResponse = class(TGenericParameters)
  private
    [JSONName('results')]
    FResults: TArray<TArray<integer>>;

    [JSONName('total')]
    FTotal: integer;

    [JSONName('fields')]
    FFields: TStringArray;

    function GetMemberId(index: integer): integer;
    function GetOrderId(index: integer): integer;
    function GetOrdersCount: integer;
  public
    constructor Create; override;

    property Total: integer read FTotal write FTotal;
    property Fields: TStringArray read FFields write FFields;
    property OrdersCount: integer read GetOrdersCount;
    property OrderId[index: integer]: integer read GetOrderId;
    property MemberId[index: integer]: integer read GetMemberId;
  end;

implementation

{ TGetOrdersWithCustomFieldsResponse }

constructor TGetOrdersWithCustomFieldsResponse.Create;
begin
  inherited;

  SetLength(FResults, 0);
  SetLength(FFields, 0);
end;

function TGetOrdersWithCustomFieldsResponse.GetMemberId(index: integer): integer;
begin
  Result := FResults[index][1];
end;

function TGetOrdersWithCustomFieldsResponse.GetOrderId(index: integer): integer;
begin
  Result := FResults[index][0];
end;

function TGetOrdersWithCustomFieldsResponse.GetOrdersCount: integer;
begin
  Result := Length(FResults);
end;

end.
