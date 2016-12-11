unit TestTerritoriesSamplesUnit;

interface

uses
  TestFramework, Classes, SysUtils,
  BaseTestOnlineExamplesUnit;

type
  TTestTerritoriesSamples = class(TTestOnlineExamples)
    procedure AddCircleTerritories;
    procedure AddPolygonTerritories;
    procedure AddRectangularTerritories;
    procedure GetTerritoriess;
    procedure GetTerritories;
    procedure UpdateTerritories;
    procedure RemoveTerritories;
  published
  end;

implementation

uses NullableBasicTypesUnit, TerritoryContourUnit, TerritoryUnit;

var
  FTerritoryId: NullableString;
  FTerritoryIds: TArray<String>;

procedure AddTerritoriesId(Id: NullableString);
begin
  if (Id.IsNotNull) then
  begin
    SetLength(FTerritoryIds, Length(FTerritoryIds) + 1);
    FTerritoryIds[High(FTerritoryIds)] := Id;
  end;
end;

procedure TTestTerritoriesSamples.AddCircleTerritories;
var
  ErrorString: String;
  TerritoryId: NullableString;
  TerritoryName, TerritoryColor: String;
  TerritoryContour: TCircleTerritory;
begin
  TerritoryName := 'Circle Territory';
  TerritoryColor := 'ff0000';
  TerritoryContour := TCircleTerritory.Create(37.5697528227865, -77.4783325195313, 5000);
  FTerritoryId := FRoute4MeManager.Territory.Add(
    TerritoryName, TerritoryColor, TerritoryContour, ErrorString);

  CheckTrue(FTerritoryId.IsNotNull);
  CheckEquals(EmptyStr, ErrorString);

  TerritoryId := FRoute4MeManager.Territory.Add(
    TerritoryName, TerritoryColor, nil, ErrorString);
  CheckTrue(TerritoryId.IsNotNull);
  CheckNotEquals(EmptyStr, ErrorString);

  TerritoryName := '';
  TerritoryColor := 'ff0000';
  TerritoryContour := TCircleTerritory.Create(37.5697528227865, -77.4783325195313, 5000);
  TerritoryId := FRoute4MeManager.Territory.Add(
    TerritoryName, TerritoryColor, TerritoryContour, ErrorString);
  CheckTrue(TerritoryId.IsNotNull);
  CheckNotEquals(EmptyStr, ErrorString);
end;

procedure TTestTerritoriesSamples.AddPolygonTerritories;
var
  ErrorString: String;
  TerritoryId: NullableString;
  TerritoryName, TerritoryColor: String;
  TerritoryContour: TPolygonTerritory;
begin
  TerritoryName := 'Polygon Territory';
  TerritoryColor := 'ff0000';
  TerritoryContour := TPolygonTerritory.Create();
  TerritoryContour.AddPoint(37.76975282278,-77.67833251953);
  TerritoryContour.AddPoint(37.75886716305,-77.68974800109);
  TerritoryContour.AddPoint(37.74763966054,-77.69172210693);

  TerritoryId := FRoute4MeManager.Territory.Add(
    TerritoryName, TerritoryColor, TerritoryContour, ErrorString);

  AddTerritoriesId(TerritoryId);
  CheckTrue(TerritoryId.IsNotNull);
  CheckEquals(EmptyStr, ErrorString);

  TerritoryName := 'Polygon Territory';
  TerritoryColor := 'ff0000';
  TerritoryContour := TPolygonTerritory.Create();
  TerritoryContour.AddPoint(37.76975282278,-77.67833251953);
  TerritoryContour.AddPoint(37.74763966054,-77.69172210693);

  TerritoryId := FRoute4MeManager.Territory.Add(
    TerritoryName, TerritoryColor, TerritoryContour, ErrorString);
  AddTerritoriesId(TerritoryId);
  CheckTrue(TerritoryId.IsNotNull);
  CheckEquals(EmptyStr, ErrorString);

  TerritoryName := 'Polygon Territory';
  TerritoryColor := 'ff0000';
  TerritoryContour := TPolygonTerritory.Create();
  TerritoryContour.AddPoint(37.74763966054,-77.69172210693);

  TerritoryId := FRoute4MeManager.Territory.Add(
    TerritoryName, TerritoryColor, TerritoryContour, ErrorString);
  AddTerritoriesId(TerritoryId);
  CheckTrue(TerritoryId.IsNotNull);
  CheckEquals(EmptyStr, ErrorString);
end;

procedure TTestTerritoriesSamples.AddRectangularTerritories;
var
  ErrorString: String;
  Territories: TTerritory;
  Parameters: TTerritory;
  TerritoryName, TerritoryColor: String;
  Territory: TRectangularTerritory;
begin
{  TerritoryName := 'Rect Territory';
  TerritoryColor := 'ff0000';
  Territory := TRectangularTerritory.Create(
    43.51668853502909, -109.3798828125, 46.98025235521883, -101.865234375);
  Parameters := TTerritory.Create(TerritoryName, TerritoryColor, Territory);
  try
    Territories := FRoute4MeManager.Territory.Add(Parameters, ErrorString);
    try
      CheckNotNull(Territories);
      AddTerritoriesId(Territories.TerritoryId);
      CheckTrue(Territories.TerritoryId.IsNotNull);
      CheckEquals(EmptyStr, ErrorString);
    finally
      FreeAndNil(Territories);
    end;
  finally
    FreeAndNil(Parameters);
  end;
}
end;

procedure TTestTerritoriesSamples.GetTerritories;
var
  ErrorString: String;
  Territories: TTerritory;
begin
{  Territories := FRoute4MeManager.Territory.Get('-123', ErrorString);
  try
    CheckNull(Territories);
    CheckNotEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Territories);
  end;

  Territories := FRoute4MeManager.Territory.Get(
    FTerritory.TerritoryId, ErrorString);
  try
    CheckNotNull(Territories);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Territories);
  end;}
end;

procedure TTestTerritoriesSamples.GetTerritoriess;
var
  ErrorString: String;
  Territories: TTerritoryList;
begin
{  Territories := FRoute4MeManager.Territory.GetList(ErrorString);
  try
    CheckNotNull(Territories);
    CheckTrue(Territories.Count > 0);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Territories);
  end;}
end;

procedure TTestTerritoriesSamples.RemoveTerritories;
var
  ErrorString: String;
begin
{  CheckTrue(FRoute4MeManager.Territory.Remove('-123', ErrorString));
  CheckEquals(EmptyStr, ErrorString);

  CheckTrue(FRoute4MeManager.Territory.Remove(FTerritory.TerritoryId, ErrorString));
  CheckEquals(EmptyStr, ErrorString);

  CheckTrue(FRoute4MeManager.Territory.Remove(FTerritoryIds, ErrorString));
  CheckEquals(EmptyStr, ErrorString);

  FreeAndNil(FTerritory);}
end;

procedure TTestTerritoriesSamples.UpdateTerritories;
var
  ErrorString: String;
  Territories: TTerritory;
  Id: String;
begin
{  FTerritory.TerritoryName := 'New name';

  Territories := FRoute4MeManager.Territory.Update(FTerritory, ErrorString);
  try
    CheckNotNull(Territories);
    CheckEquals(EmptyStr, ErrorString);
    CheckEquals('New name', Territories.TerritoryName);
  finally
    FreeAndNil(Territories)
  end;

  Id := FTerritory.TerritoryId;
  FTerritory.TerritoryId := NullableString.Null;
  try
    FTerritory.TerritoryName := 'Error';
    Territories := FRoute4MeManager.Territory.Update(FTerritory, ErrorString);
    try
      CheckNull(Territories);
      CheckNotEquals(EmptyStr, ErrorString);
    finally
      FreeAndNil(Territories)
    end;
  finally
    FTerritory.TerritoryId := Id;
  end;}
end;

initialization
  RegisterTest('Examples\Online\Territories\', TTestTerritoriesSamples.Suite);
  SetLength(FTerritoryIds, 0);

end.
