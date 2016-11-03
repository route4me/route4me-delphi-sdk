unit AvoidanceZoneQueryUnit;

interface

uses
  HttpQueryMemberAttributeUnit, JSONNullableAttributeUnit,
  NullableBasicTypesUnit, GenericParametersUnit;

type
  TAvoidanceZoneQuery = class(TGenericParameters)
  private
    [HttpQueryMember('device_id')]
    [Nullable]
    FDeviceId: NullableString;

    [HttpQueryMember('territory_id')]
    [Nullable]
    FTerritoryId: NullableString;
  public
    constructor Create; override;

    property DeviceId: NullableString read FDeviceId write FDeviceId;
    property TerritoryId: NullableString read FTerritoryId write FTerritoryId;
  end;

implementation

{ TAddressParameters }

constructor TAvoidanceZoneQuery.Create;
begin
  Inherited Create;

  DeviceId := NullableString.Null;
  TerritoryId := NullableString.Null;
end;

end.
