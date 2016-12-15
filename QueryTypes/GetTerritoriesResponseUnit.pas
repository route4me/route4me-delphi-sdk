unit GetTerritoriesResponseUnit;

interface

uses
  REST.Json.Types, JSONNullableAttributeUnit,
  GenericParametersUnit, TerritoryUnit;

type
  TGetTerritoriesResponse = class(TGenericParameters)
  private
    // Array of objects must named "FItems" for correct unmarshaling.
    FItems: TArray<TTerritory>;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Territories: TArray<TTerritory> read FItems;
  end;

implementation

constructor TGetTerritoriesResponse.Create;
begin
  inherited;

  SetLength(FItems, 0);
end;

destructor TGetTerritoriesResponse.Destroy;
begin
  Finalize(FItems);

  inherited;
end;

end.
