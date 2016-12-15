unit TerritoryUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections, Generics.Defaults,
  HttpQueryMemberAttributeUnit, JSONNullableAttributeUnit, NullableBasicTypesUnit,
  GenericParametersUnit, TerritoryContourUnit, CommonTypesUnit, AddressUnit;

type
  /// <summary>
  ///  Json schema for an Territory class, which is used for defining different type avoidance zones.
  /// </summary>
  /// <remarks>
  ///  https://github.com/route4me/json-schemas/blob/master/Territory.dtd
  /// </remarks>
  TTerritory = class(TGenericParameters)
  private
//    [JSONMarshalled(False)]
//    [HttpQueryMember('territory_id')]
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
    [NullableArray(TSimpleInteger)]
    FAddressIds: TArray<TSimpleInteger>;
//    FAddressIds: TArray<integer>;

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
    property Id: NullableString read FTerritoryId write FTerritoryId;

    /// <summary>
    ///  Territory name
    /// </summary>
    property Name: NullableString read FTerritoryName write FTerritoryName;

    /// <summary>
    ///  Territory color
    /// </summary>
    property Color: NullableString read FTerritoryColor write FTerritoryColor;

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
//    property AddressIds: TArray<integer> read FAddressIds;
    property AddressIds: TArray<TSimpleInteger> read FAddressIds;
    procedure AddAddressId(AddressId: integer);
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
  SetLength(FAddressIds, 0);
end;

procedure TTerritory.AddAddressId(AddressId: integer);
begin
  SetLength(FAddressIds, Length(FAddressIds) + 1);
//  FAddressIds[High(FAddressIds)] := AddressId;
  FAddressIds[High(FAddressIds)] := TSimpleInteger.Create(AddressId);
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
    (Length(FAddressIds) = Length(Other.FAddressIds));

  if not Result then
    Exit;

  Result := False;

{!!!  SortedData1 := TUtils.SortIntegerArray(FAddressIds);
  SortedData2 := TUtils.SortIntegerArray(Other.FAddressIds);
  for i := 0 to Length(SortedData1) - 1 do
    if (SortedData1[i] <> SortedData2[i]) then
      Exit;}

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
