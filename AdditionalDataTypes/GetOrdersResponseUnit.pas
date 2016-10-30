unit GetOrdersResponseUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit, OrderUnit;

type
  TGetOrdersResponse = class(TGenericParameters)
  private
    [JSONName('results')]
    FResults: TOrderArray;

    [JSONName('total')]
    FTotal: integer;
  public
    property Results: TOrderArray read FResults write FResults;
    property Total: integer read FTotal write FTotal;
  end;

implementation

end.
