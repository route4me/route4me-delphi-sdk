unit AvoidanceZoneUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections, Generics.Defaults,
  JSONNullableAttributeUnit, NullableBasicTypesUnit,
  GenericParametersUnit, HttpQueryMemberAttributeUnit, TerritoryUnit;

type
  /// <summary>
  ///  Json schema for an Avoidance Zone clas, which is used for defining different type avoidance zones.
  /// </summary>
  /// <remarks>
  ///  https://github.com/route4me/json-schemas/blob/master/Avoidance_zone.dtd
  /// </remarks>
  TAvoidanceZone = class(TGenericParameters)
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

    function GetTerritory: TTerritory;
    procedure SetTerritory(const Value: TTerritory);
  public
    constructor Create; override;
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; override;

    /// <summary>
    ///  32 character unique identifier
    /// </summary>
    property TerritoryId: NullableString read FTerritoryId write FTerritoryId;

    /// <summary>
    ///  Territory name
    /// </summary>
    property TerritoryName: NullableString read FTerritoryName write FTerritoryName;

    /// <summary>
    ///  Territory color
    /// </summary>
    property TerritoryColor: NullableString read FTerritoryColor write FTerritoryColor;

    /// <summary>
    ///  Member ID
    /// </summary>
    property MemberId: NullableString read FMemberId write FMemberId;

    /// <summary>
    ///  Territory
    /// </summary>
    property Territory: TTerritory read GetTerritory write SetTerritory;
  end;

  TAvoidanceZoneArray = TArray<TAvoidanceZone>;
  TAvoidanceZoneList = TList<TAvoidanceZone>;

  function SortAvoidanceZones(AvoidanceZones: TAvoidanceZoneArray): TAvoidanceZoneArray;

implementation

function SortAvoidanceZones(AvoidanceZones: TAvoidanceZoneArray): TAvoidanceZoneArray;
begin
  SetLength(Result, Length(AvoidanceZones));
  if Length(AvoidanceZones) = 0 then
    Exit;

  TArray.Copy<TAvoidanceZone>(AvoidanceZones, Result, Length(AvoidanceZones));
  TArray.Sort<TAvoidanceZone>(Result, TComparer<TAvoidanceZone>.Construct(
    function (const AvoidanceZone1, AvoidanceZone2: TAvoidanceZone): Integer
    begin
      Result := AvoidanceZone1.FTerritoryId.Compare(AvoidanceZone2.FTerritoryId);
    end));
end;

{ TAvoidanceZoneParameters }

constructor TAvoidanceZone.Create;
begin
  Inherited Create;

  FTerritoryId := NullableString.Null;
  FTerritoryName := NullableString.Null;
  FTerritoryColor := NullableString.Null;
  FMemberId := NullableString.Null;
  FTerritory := NullableObject.Null;
end;

destructor TAvoidanceZone.Destroy;
begin
  FTerritory.Free;

  inherited;
end;

function TAvoidanceZone.Equals(Obj: TObject): Boolean;
var
  Other: TAvoidanceZone;
begin
  Result := False;

  if not (Obj is TAvoidanceZone) then
    Exit;

  Other := TAvoidanceZone(Obj);

  Result :=
    (FTerritoryId = Other.FTerritoryId) and
    (FTerritoryName = Other.FTerritoryName) and
    (FTerritoryColor = Other.FTerritoryColor) and
    (FMemberId = Other.FMemberId) and
    (FTerritory = Other.FTerritory);
end;

function TAvoidanceZone.GetTerritory: TTerritory;
begin
  if (FTerritory.IsNull) then
    Result := nil
  else
    Result := FTerritory.Value as TTerritory;
end;

procedure TAvoidanceZone.SetTerritory(const Value: TTerritory);
begin
  FTerritory := Value;
end;

end.