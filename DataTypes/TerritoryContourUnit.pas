unit TerritoryContourUnit;

interface

uses
  REST.Json.Types, SysUtils, System.Generics.Collections,
  JSONNullableAttributeUnit, NullableBasicTypesUnit, CommonTypesUnit, EnumsUnit;

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
  protected
    /// <summary>
    /// Territory type (circle, rectangle, polygon)
    /// </summary>
    property TerritoryType: TTerritoryType read GetType write SetType;

    /// <summary>
    ///
    /// </summary>
    property Data: TStringArray read FData;
    procedure AddDataItem(Item: String);
  public
    constructor Create; virtual;

    function Equals(Obj: TObject): Boolean; override;

{    procedure MakeCircleContour(Latitude, Longitude, Radius: double);
    procedure MakePolygonContour(Points: TArray<>);
    procedure MakeRectangularContour(Latitude1, Longitude1, Latitude2, Longitude2: double);}

    function Clone: TTerritoryContour; virtual; abstract;
  end;

  TCircleTerritory = class(TTerritoryContour)
  private
    FLatitude: double;
    FLongitude: double;
    FRadius: double;
  public
    constructor Create(Latitude, Longitude, Radius: double); reintroduce;

    function Clone: TTerritoryContour; override;
  end;

  TPolygonTerritory = class(TTerritoryContour)
  var
    FLatitudes: TArray<double>;
    FLongitudes: TArray<double>;
  public
    constructor Create(); override;

    procedure AddPoint(Latitude, Longitude: double);

    function Clone: TTerritoryContour; override;
  end;

  TRectangularTerritory = class(TTerritoryContour)
  private
    FLatitude1: double;
    FLongitude1: double;
    FLatitude2: double;
    FLongitude2: double;
  public
    constructor Create(Latitude1, Longitude1, Latitude2, Longitude2: double); reintroduce;

    function Clone: TTerritoryContour; override;
  end;

implementation

{ TTerritoryContour }

uses UtilsUnit;

procedure TTerritoryContour.AddDataItem(Item: String);
begin
  SetLength(FData, Length(FData) + 1);
  FData[High(FData)] := Item;
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

procedure TTerritoryContour.SetType(const Value: TTerritoryType);
begin
  FType := TTerritoryTypeDescription[Value];
end;

{ TCircleTerritory }

function TCircleTerritory.Clone: TTerritoryContour;
begin
  Result := TCircleTerritory.Create(FLatitude, FLongitude, FRadius);
end;

constructor TCircleTerritory.Create(Latitude, Longitude, Radius: double);
begin
  Inherited Create;

  FLatitude := Latitude;
  FLongitude := Longitude;
  FRadius := FRadius;

  TerritoryType := TTerritoryType.ttCircle;
  AddDataItem(TUtils.FloatToStrDot(Latitude) + ',' + TUtils.FloatToStrDot(Longitude));
  AddDataItem(TUtils.FloatToStrDot(Radius));
end;

{ TCircleTerritory }

{function TCircleTerritory.Clone: TTerritoryContour;
begin

end;

 constructor TCircleTerritory.Create(Latitude, Longitude, Radius: double);
begin

end;

TPolygonTerritory }

procedure TPolygonTerritory.AddPoint(Latitude, Longitude: double);
begin
  SetLength(FLatitudes, Length(FLatitudes) + 1);
  FLatitudes[High(FLatitudes)] := Latitude;

  SetLength(FLongitudes, Length(FLongitudes) + 1);
  FLongitudes[High(FLongitudes)] := Longitude;

  AddDataItem(TUtils.FloatToStrDot(Latitude) + ',' + TUtils.FloatToStrDot(Longitude));
end;

function TPolygonTerritory.Clone: TTerritoryContour;
var
  i: integer;
begin
  Result := TPolygonTerritory.Create;

  for i := 0 to High(FLatitudes) do
    TPolygonTerritory(Result).AddPoint(FLatitudes[i], FLongitudes[i]);
end;

constructor TPolygonTerritory.Create;
begin
  inherited Create;

  TerritoryType := TTerritoryType.ttPoly;

  SetLength(FLatitudes, 0);
  SetLength(FLongitudes,0);
end;

{ TRectangularTerritory }

function TRectangularTerritory.Clone: TTerritoryContour;
begin
  Result := TRectangularTerritory.Create(
    FLatitude1, FLongitude1, FLatitude2, FLongitude2);
end;

constructor TRectangularTerritory.Create(
  Latitude1, Longitude1, Latitude2, Longitude2: double);
begin
  inherited Create;

  FLatitude1 := Latitude1;
  FLongitude1 := Longitude1;
  FLatitude2 := Latitude2;
  FLongitude2 := Longitude2;

  TerritoryType := TTerritoryType.ttRect;

  AddDataItem(TUtils.FloatToStrDot(Latitude1) + ',' + TUtils.FloatToStrDot(Longitude1));
  AddDataItem(TUtils.FloatToStrDot(Latitude2) + ',' + TUtils.FloatToStrDot(Longitude2));
end;

end.
