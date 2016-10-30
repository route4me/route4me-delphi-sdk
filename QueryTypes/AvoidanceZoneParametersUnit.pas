unit AvoidanceZoneParametersUnit;

interface

uses
  REST.Json.Types, NullableBasicTypesUnit, JSONNullableAttributeUnit,
  GenericParametersUnit, HttpQueryMemberAttributeUnit, TerritoryUnit;

type
  TAvoidanceZoneParameters = class(TGenericParameters)
  private
    [JSONMarshalled(False)]
    [HttpQueryMember('device_id')]
    [Nullable]
    FDeviceId: NullableString;

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
    constructor Create; override;
    destructor Destroy; override;

    property DeviceId: NullableString read FDeviceId write FDeviceId;
    property TerritoryId: NullableString read FTerritoryId write FTerritoryId;
    property TerritoryName: NullableString read FTerritoryName write FTerritoryName;
    property TerritoryColor: NullableString read FTerritoryColor write FTerritoryColor;
    property MemberId: NullableString read FMemberId write FMemberId;
    property Territory: NullableObject read FTerritory write FTerritory;
  end;

implementation

{ TAvoidanceZoneParameters }

constructor TAvoidanceZoneParameters.Create;
begin
  Inherited Create;

  DeviceId := NullableString.Null;
  TerritoryId := NullableString.Null;
  TerritoryName := NullableString.Null;
  TerritoryColor := NullableString.Null;
  MemberId := NullableString.Null;
  Territory := NullableObject.Null;
end;

destructor TAvoidanceZoneParameters.Destroy;
begin
  FTerritory.Free;

  inherited;
end;

end.
