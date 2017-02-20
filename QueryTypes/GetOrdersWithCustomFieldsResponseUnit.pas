unit GetOrdersWithCustomFieldsResponseUnit;

interface

uses
  REST.Json.Types, SysUtils,
  GenericParametersUnit, OrderUnit, CommonTypesUnit;

type
  TGetOrdersWithCustomFieldsResponse = class(TGenericParameters)
  private
    [JSONName('results')]
    FResults: TArray<TArray<string>>;

    [JSONName('total')]
    FTotal: integer;

    [JSONName('fields')]
    FFields: TStringArray;

    function GetResults(index: integer): TStringArray;
    function GetResultCount: Integer;
  public
    constructor Create; override;

    function GetResult(index: integer; Field: String): String;

    property Total: integer read FTotal;
    property Fields: TStringArray read FFields;
    property Results[index: integer]: TStringArray read GetResults;
    property ResultCount: Integer read GetResultCount;
  end;

implementation

{ TGetOrdersWithCustomFieldsResponse }

constructor TGetOrdersWithCustomFieldsResponse.Create;
begin
  inherited;

  SetLength(FResults, 0);
  SetLength(FFields, 0);
end;

function TGetOrdersWithCustomFieldsResponse.GetResult(index: integer;
  Field: String): String;
var
  Results: TStringArray;
  i: Integer;
begin
  Result := EmptyStr;

  Results := FResults[index];
  for i := 0 to High(FFields) do
    if (FFields[i] = Field) then
    begin
      Result := Results[i];
      Break
    end;
end;

function TGetOrdersWithCustomFieldsResponse.GetResultCount: Integer;
begin
  Result := Length(FResults);
end;

function TGetOrdersWithCustomFieldsResponse.GetResults(index: integer): TStringArray;
begin
  Result := FResults[index];
end;

end.
