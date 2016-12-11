unit TerritoryUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections, Generics.Defaults,
  JSONNullableAttributeUnit, NullableBasicTypesUnit,
  GenericParametersUnit, TerritoryContourUnit, CommonTypesUnit;

type
  /// <summary>
  ///  Json schema for an Territory class, which is used for defining different type avoidance zones.
  /// </summary>
  /// <remarks>
  ///  https://github.com/route4me/json-schemas/blob/master/Territory.dtd
  /// </remarks>
  TTerritory = class(TGenericParameters)
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

    [JSONNameAttribute('addresses')]
    FAddresses: TArray<integer>;

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

    /// <summary>
    ///  Territory
    /// </summary>
    property Addresses: TArray<integer> read FAddresses;
  end;

  TTerritoryArray = TArray<TTerritory>;
  TTerritoryList = TObjectList<TTerritory>;

  function SortTerritorys(Territorys: TTerritoryArray): TTerritoryArray;

implementation

uses UtilsUnit;

function SortTerritorys(Territorys: TTerritoryArray): TTerritoryArray;
begin
  SetLength(Result, Length(Territorys));
  if Length(Territorys) = 0 then
    Exit;

  TArray.Copy<TTerritory>(Territorys, Result, Length(Territorys));
  TArray.Sort<TTerritory>(Result, TComparer<TTerritory>.Construct(
    function (const Territory1, Territory2: TTerritory): Integer
    begin
      Result := Territory1.FTerritoryId.Compare(Territory2.FTerritoryId);
    end));
end;

constructor TTerritory.Create;
begin
  Inherited Create;

  FTerritoryId := NullableString.Null;
  FTerritoryName := NullableString.Null;
  FTerritoryColor := NullableString.Null;
  FMemberId := NullableString.Null;
  FTerritory := NullableObject.Null;
  SetLength(FAddresses, 0);
end;

constructor TTerritory.Create(TerritoryName, TerritoryColor: String;
  Territory: TTerritoryContour);
begin
  Create;

  FTerritoryName := TerritoryName;
  FTerritoryColor := TerritoryColor;
  FTerritory := Territory;
end;

destructor TTerritory.Destroy;
begin
  FTerritory.Free;

  inherited;
end;

function TTerritory.Equals(Obj: TObject): Boolean;
var
  Other: TTerritory;
  SortedData1, SortedData2: TArray<integer>;
  i: integer;
begin
  Result := False;

  if not (Obj is TTerritory) then
    Exit;

  Other := TTerritory(Obj);

  Result :=
    (FTerritoryId = Other.FTerritoryId) and
    (FTerritoryName = Other.FTerritoryName) and
    (FTerritoryColor = Other.FTerritoryColor) and
    (FMemberId = Other.FMemberId) and
    (FTerritory = Other.FTerritory) and
    (Length(FAddresses) = Length(Other.FAddresses));

  if not Result then
    Exit;

  Result := False;

  SortedData1 := TUtils.SortIntegerArray(FAddresses);
  SortedData2 := TUtils.SortIntegerArray(Other.FAddresses);
  for i := 0 to Length(SortedData1) - 1 do
    if (SortedData1[i] <> SortedData2[i]) then
      Exit;

  Result := True;
end;

function TTerritory.GetTerritory: TTerritoryContour;
begin
  if (FTerritory.IsNull) then
    Result := nil
  else
    Result := FTerritory.Value as TTerritoryContour;
end;

procedure TTerritory.SetTerritory(const Value: TTerritoryContour);
begin
  FTerritory := Value;
end;

end.
