unit GetOrdersWithCustomFieldsResponseUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit, OrderUnit, CommonTypesUnit;

type
  TOrderPair = class(TGenericParameters)
  private
    FValues: TIntegerArray;

    function GetId: integer;
  public
    property Id: integer read GetId;
  end;
  TOrderPairArray = TArray<TOrderPair>;

  TGetOrdersWithCustomFieldsResponse = class(TGenericParameters)
  private
    [JSONName('results')]
    FResults: TOrderPairArray;

    [JSONName('total')]
    FTotal: integer;

    [JSONName('fields')]
    FFields: TStringArray;
  public
    constructor Create; override;

    property Results: TOrderPairArray read FResults write FResults;
    property Total: integer read FTotal write FTotal;
    property Fields: TStringArray read FFields write FFields;
  end;

implementation

{ TGetOrdersWithCustomFieldsResponse }

constructor TGetOrdersWithCustomFieldsResponse.Create;
begin
  inherited;

  SetLength(FResults, 0);
  SetLength(FFields, 0);
end;

{ TOrderPair }

function TOrderPair.GetId: integer;
begin
  Result := FValues[0];
end;

end.
