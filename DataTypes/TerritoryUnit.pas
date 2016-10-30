unit TerritoryUnit;

interface

uses
  REST.Json.Types, SysUtils,
  JSONNullableAttributeUnit, NullableBasicTypesUnit, CommonTypesUnit, EnumsUnit;

type
  /// <summary>
  /// Territory parameters
  /// </summary>
  /// <remarks>
  ///  https://github.com/route4me/json-schemas/blob/master/Avoidance_zone.dtd
  /// </remarks>
  TTerritory = class
  private
    [JSONName('type')]
    [Nullable]
    FType: NullableString;

    [JSONName('data')]
    FData: TStringArray;

    function GetType: TTerritoryType;
    procedure SetType(const Value: TTerritoryType);
  public
    /// <remarks>
    ///  Constructor with 0-arguments must be and be public.
    ///  For JSON-deserialization.
    /// </remarks>
    constructor Create;

    function Equals(Obj: TObject): Boolean; override;

    /// <summary>
    /// Territory type (circle, rectangle, polygon)
    /// </summary>
    property TerritoryType: TTerritoryType read GetType write SetType;

    /// <summary>
    ///
    /// </summary>
    property Data: TStringArray read FData;
    procedure AddDataItem(Item: String);
  end;

implementation

{ TTerritory }

procedure TTerritory.AddDataItem(Item: String);
begin
  SetLength(FData, Length(FData) + 1);
  FData[High(FData)] := Item;
end;

constructor TTerritory.Create;
begin
  Inherited;

  FType := NullableString.Null;
  SetLength(FData, 0);
end;

function TTerritory.Equals(Obj: TObject): Boolean;
var
  Other: TTerritory;
  SortedData1,  SortedData2: TStringArray;
  i: integer;
begin
  Result := False;

  if not (Obj is TTerritory) then
    Exit;

  Other := TTerritory(Obj);

  Result := (TerritoryType = Other.TerritoryType);

  if (not Result) then
    Exit;

  if (Length(Data) <> Length(Other.Data)) then
    Exit;

  SortedData1 := SortStringArray(Data);
  SortedData2 := SortStringArray(Other.Data);
  for i := 0 to Length(SortedData1) - 1 do
    if not SortedData1[i].Equals(SortedData2[i]) then
      Exit;

  Result := True;
end;

function TTerritory.GetType: TTerritoryType;
var
  TerritoryType: TTerritoryType;
begin
  if FType.IsNull then
    Exit(TTerritoryType.ttUndefined);

  for TerritoryType := Low(TTerritoryType) to High(TTerritoryType) do
    if (FType = TTerritoryTypeDescription[TerritoryType]) then
      Exit(TerritoryType);
end;

procedure TTerritory.SetType(const Value: TTerritoryType);
begin
  FType := TTerritoryTypeDescription[Value];
end;

end.
