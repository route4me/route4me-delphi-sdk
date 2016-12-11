unit GetTerritoriesResponseUnit;

interface

uses
  REST.Json.Types, JSONNullableAttributeUnit,
  GenericParametersUnit, TerritoryUnit;

type
  TGetTerritoriesResponse = class(TGenericParameters)
  private
    FTerritories: TArray<TTerritory>;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Territories: TArray<TTerritory> read FTerritories;
  end;

implementation

constructor TGetTerritoriesResponse.Create;
begin
  inherited;

  SetLength(FTerritories, 0);
end;

destructor TGetTerritoriesResponse.Destroy;
begin
  Finalize(FTerritories);

  inherited;
end;

end.
