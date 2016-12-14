unit TerritoryContourUnit;

interface

uses
  REST.Json.Types, SysUtils, System.Generics.Collections,
  JSONNullableAttributeUnit, NullableBasicTypesUnit, CommonTypesUnit, EnumsUnit,
  PositionUnit;

type
  /// <summary>
  /// Territory parameters
  /// </summary>
  /// <remarks>
  ///  https://github.com/route4me/json-schemas/blob/master/Avoidance_zone.dtd
  /// </remarks>
  TTerritoryContour = class
  private
    [JSONName('type')]
    [Nullable]
    FType: NullableString;

    [JSONName('data')]
    FData: TStringArray;

    function GetType: TTerritoryType;
    procedure SetType(const Value: TTerritoryType);
  public
    constructor Create; virtual;

    function Equals(Obj: TObject): Boolean; override;

    class function MakeCircleContour(
      Latitude, Longitude, Radius: double): TTerritoryContour; static;
    class function MakePolygonContour(Points: TArray<TPosition>): TTerritoryContour; static;
    class function MakeRectangularContour(
      Latitude1, Longitude1, Latitude2, Longitude2: double): TTerritoryContour; static;

    /// <summary>
    /// Territory type (circle, rectangle, polygon)
    /// </summary>
    property TerritoryType: TTerritoryType read GetType write SetType;

    /// <summary>
    ///
    /// </summary>
    property Data: TStringArray read FData;
    procedure AddDataItem(Item: String);

    function Clone: TTerritoryContour;
  end;

implementation

{ TTerritoryContour }

uses UtilsUnit;

procedure TTerritoryContour.AddDataItem(Item: String);
begin
  SetLength(FData, Length(FData) + 1);
  FData[High(FData)] := Item;
end;

function TTerritoryContour.Clone: TTerritoryContour;
var
  i: integer;
begin
  Result := TTerritoryContour.Create;
  Result.TerritoryType := TerritoryType;

  for i := 0 to High(Data) do
    Result.AddDataItem(Data[i]);
end;

constructor TTerritoryContour.Create;
begin
  Inherited;

  FType := NullableString.Null;
  SetLength(FData, 0);
end;

function TTerritoryContour.Equals(Obj: TObject): Boolean;
var
  Other: TTerritoryContour;
  SortedData1,  SortedData2: TStringArray;
  i: integer;
begin
  Result := False;

  if not (Obj is TTerritoryContour) then
    Exit;

  Other := TTerritoryContour(Obj);

  Result :=
    (FType = Other.FType);

  if (not Result) then
    Exit;

  Result := False;

  if (Length(Data) <> Length(Other.Data)) then
    Exit;

  SortedData1 := TUtils.SortStringArray(Data);
  SortedData2 := TUtils.SortStringArray(Other.Data);
  for i := 0 to Length(SortedData1) - 1 do
    if not SortedData1[i].Equals(SortedData2[i]) then
      Exit;

  Result := True;
end;

function TTerritoryContour.GetType: TTerritoryType;
var
  TerritoryType: TTerritoryType;
begin
  Result := TTerritoryType.ttUndefined;
  if FType.IsNotNull then
    for TerritoryType := Low(TTerritoryType) to High(TTerritoryType) do
      if (FType = TTerritoryTypeDescription[TerritoryType]) then
        Exit(TerritoryType);
end;

class function TTerritoryContour.MakeCircleContour(Latitude, Longitude,
  Radius: double): TTerritoryContour;
begin
  Result := TTerritoryContour.Create;

  Result.TerritoryType := TTerritoryType.ttCircle;
  Result.AddDataItem(TUtils.FloatToStrDot(Latitude) + ',' + TUtils.FloatToStrDot(Longitude));
  Result.AddDataItem(TUtils.FloatToStrDot(Radius));
end;

class function TTerritoryContour.MakePolygonContour(
  Points: TArray<TPosition>): TTerritoryContour;
var
  i: integer;
begin
  Result := TTerritoryContour.Create;

  Result.TerritoryType := TTerritoryType.ttPoly;

  for i := 0 to High(Points) do
    Result.AddDataItem(
      TUtils.FloatToStrDot(Points[i].Latitude) + ',' +
      TUtils.FloatToStrDot(Points[i].Longitude));
end;

class function TTerritoryContour.MakeRectangularContour(Latitude1, Longitude1,
  Latitude2, Longitude2: double): TTerritoryContour;
begin
  Result := TTerritoryContour.Create;

  Result.TerritoryType := TTerritoryType.ttRect;

  Result.AddDataItem(TUtils.FloatToStrDot(Latitude1) + ',' + TUtils.FloatToStrDot(Longitude1));
  Result.AddDataItem(TUtils.FloatToStrDot(Latitude2) + ',' + TUtils.FloatToStrDot(Longitude2));
end;

procedure TTerritoryContour.SetType(const Value: TTerritoryType);
begin
  FType := TTerritoryTypeDescription[Value];
end;

end.
