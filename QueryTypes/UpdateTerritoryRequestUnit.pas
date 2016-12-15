unit UpdateTerritoryRequestUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections, Generics.Defaults,
  HttpQueryMemberAttributeUnit, JSONNullableAttributeUnit, NullableBasicTypesUnit,
  GenericParametersUnit, TerritoryContourUnit, CommonTypesUnit, AddressUnit,
  TerritoryUnit;

type
  TUpdateTerritoryRequest = class(TGenericParameters)
  private
    [JSONMarshalled(False)]
    [HttpQueryMember('territory_id')]
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

    [JSONNameAttribute('territory')]
    [NullableObject(TTerritoryContour)]
    FTerritory: NullableObject;

    function GetTerritory: TTerritoryContour;
    procedure SetTerritory(const Value: TTerritoryContour);
  public
    constructor Create; overload; override;
    constructor Create(TerritoryName, TerritoryColor: String; Territory: TTerritoryContour); reintroduce; overload;
    constructor Create(Territory: TTerritory); reintroduce; overload;
    destructor Destroy; override;

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
    property AddressIds: TArray<TSimpleInteger> read FAddressIds;
    procedure AddAddressId(AddressId: integer);
  end;

implementation

uses UtilsUnit;

constructor TUpdateTerritoryRequest.Create;
begin
  Inherited Create;

  FTerritoryId := NullableString.Null;
  FTerritoryName := NullableString.Null;
  FTerritoryColor := NullableString.Null;
  FMemberId := NullableString.Null;
  FTerritory := NullableObject.Null;
  SetLength(FAddressIds, 0);
end;

procedure TUpdateTerritoryRequest.AddAddressId(AddressId: integer);
begin
  SetLength(FAddressIds, Length(FAddressIds) + 1);
  FAddressIds[High(FAddressIds)] := TSimpleInteger.Create(AddressId);
end;

constructor TUpdateTerritoryRequest.Create(TerritoryName, TerritoryColor: String;
  Territory: TTerritoryContour);
begin
  Create;

  FTerritoryName := TerritoryName;
  FTerritoryColor := TerritoryColor;
  FTerritory := Territory;
end;

destructor TUpdateTerritoryRequest.Destroy;
begin
  FTerritory.Free;

  inherited;
end;

function TUpdateTerritoryRequest.GetTerritory: TTerritoryContour;
begin
  if (FTerritory.IsNull) then
    Result := nil
  else
    Result := FTerritory.Value as TTerritoryContour;
end;

procedure TUpdateTerritoryRequest.SetTerritory(const Value: TTerritoryContour);
begin
  FTerritory := Value;
end;

constructor TUpdateTerritoryRequest.Create(Territory: TTerritory);
var
  i: integer;
begin
  Create;

  FTerritoryId := Territory.Id;
  FTerritoryName := Territory.Name;
  FTerritoryColor := Territory.Color;
  FMemberId := Territory.MemberId;
  FTerritory := Territory.Territory.Clone;

  for i := 0 to High(Territory.AddressIds) do
//    AddAddressId(Territory.AddressIds[i]);
    AddAddressId(Territory.AddressIds[i].Value);
end;

end.
