unit AvoidanceZoneUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections, Generics.Defaults,
  JSONNullableAttributeUnit, NullableBasicTypesUnit,
  GenericParametersUnit, TerritoryContourUnit;

type
  /// <summary>
  ///  Json schema for an Avoidance Zone class, which is used for defining different type avoidance zones.
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
    [NullableObject(TTerritoryContour)]
    FTerritory: NullableObject;

    function GetTerritory: TTerritoryContour;
    procedure SetTerritory(const Value: TTerritoryContour);
  public
    constructor Create; overload; override;
    constructor Create(TerritoryName, TerritoryColor: String; Territory: TTerritoryContour); reintroduce; overload;
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
    property Territory: TTerritoryContour read GetTerritory write SetTerritory;
  end;

  TAvoidanceZoneArray = TArray<TAvoidanceZone>;
  TAvoidanceZoneList = TObjectList<TAvoidanceZone>;

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

constructor TAvoidanceZone.Create;
begin
  Inherited Create;

  FTerritoryId := NullableString.Null;
  FTerritoryName := NullableString.Null;
  FTerritoryColor := NullableString.Null;
  FMemberId := NullableString.Null;
  FTerritory := NullableObject.Null;
end;

constructor TAvoidanceZone.Create(TerritoryName, TerritoryColor: String;
  Territory: TTerritoryContour);
begin
  Create;

  FTerritoryName := TerritoryName;
  FTerritoryColor := TerritoryColor;
  FTerritory := Territory;
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

function TAvoidanceZone.GetTerritory: TTerritoryContour;
begin
  if (FTerritory.IsNull) then
    Result := nil
  else
    Result := FTerritory.Value as TTerritoryContour;
end;

procedure TAvoidanceZone.SetTerritory(const Value: TTerritoryContour);
begin
  FTerritory := Value;
end;

end.