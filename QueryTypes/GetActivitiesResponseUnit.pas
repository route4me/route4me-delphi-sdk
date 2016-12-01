unit GetActivitiesResponseUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit, NullableBasicTypesUnit, ActivityUnit,
  JSONNullableAttributeUnit;

type
  TGetActivitiesResponse = class(TGenericParameters)
  private
    [JSONName('results')]
    FResults: TActivityArray;

    [JSONName('total')]
    [Nullable]
    FTotal: NullableInteger;
  public
    constructor Create; override;

    property Results: TActivityArray read FResults write FResults;
    property Total: NullableInteger read FTotal write FTotal;
  end;

implementation

constructor TGetActivitiesResponse.Create;
begin
  inherited;
  SetLength(FResults, 0);
end;

end.
