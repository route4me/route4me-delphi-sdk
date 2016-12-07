unit AvoidanceZoneRequestUnit;

interface

uses
  REST.Json.Types, JSONNullableAttributeUnit,
  NullableBasicTypesUnit, GenericParametersUnit, TerritoryUnit;

type
  TAvoidanceZoneRequest = class(TGenericParameters)
  private
    [JSONNameAttribute('territory_id')]
    [Nullable]
    FTerritoryId: NullableString;

    [JSONNameAttribute('territory_name')]
    [Nullable]
    FTerritoryName: NullableString;

    [JSONNameAttribute('territory_color')]
    [Nullable]
    FTerritoryColor: NullableString;

    [JSONNameAttribute('member_id')]
    [Nullable]
    FMemberId: NullableString;

    [JSONNameAttribute('territory')]
    [NullableObject(TTerritory)]
    FTerritory: NullableObject;
  public
    constructor Create; overload; override;
    constructor Create(TerritoryName, TerritoryColor: String; Territory: TTerritory); reintroduce; overload;
    destructor Destroy; override;

    property TerritoryId: NullableString read FTerritoryId write FTerritoryId;
    property TerritoryName: NullableString read FTerritoryName write FTerritoryName;
    property TerritoryColor: NullableString read FTerritoryColor write FTerritoryColor;
    property MemberId: NullableString read FMemberId write FMemberId;
    property Territory: NullableObject read FTerritory write FTerritory;
  end;

implementation

constructor TAvoidanceZoneRequest.Create;
begin
  Inherited Create;

  TerritoryId := NullableString.Null;
  TerritoryName := NullableString.Null;
  TerritoryColor := NullableString.Null;
  MemberId := NullableString.Null;
  Territory := NullableObject.Null;
end;

constructor TAvoidanceZoneRequest.Create(TerritoryName, TerritoryColor: String;
  Territory: TTerritory);
begin
  Create;

  FTerritoryName := TerritoryName;
  FTerritoryColor := TerritoryColor;
  FTerritory := Territory;
end;

destructor TAvoidanceZoneRequest.Destroy;
begin
  FTerritory.Free;

  inherited;
end;

end.
