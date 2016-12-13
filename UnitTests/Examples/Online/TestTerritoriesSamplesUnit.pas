unit TestTerritoriesSamplesUnit;

interface

uses
  TestFramework, Classes, SysUtils,
  BaseTestOnlineExamplesUnit;

type
  TTestTerritoriesSamples = class(TTestOnlineExamples)
  published
    procedure AddCircleTerritories;
    procedure AddPolygonTerritories;
    procedure AddRectangularTerritories;
    procedure GetTerritories;
    procedure GetTerritory;
    procedure UpdateTerritories;
    procedure RemoveTerritories;
  end;

implementation

uses NullableBasicTypesUnit, TerritoryContourUnit, TerritoryUnit, AddressUnit;

var
  FTerritoryId: NullableString;
  FTerritory: TTerritory;
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
  CheckTrue(TerritoryId.IsNull);
  CheckNotEquals(EmptyStr, ErrorString);

  TerritoryName := '';
  TerritoryColor := 'ff0000';
  TerritoryContour := TCircleTerritory.Create(37.5697528227865, -77.4783325195313, 5000);
  TerritoryId := FRoute4MeManager.Territory.Add(
    TerritoryName, TerritoryColor, TerritoryContour, ErrorString);
  CheckTrue(TerritoryId.IsNull);
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
  TerritoryId: NullableString;
  TerritoryName, TerritoryColor: String;
  TerritoryContour: TRectangularTerritory;
begin
  TerritoryName := 'Rect Territory';
  TerritoryColor := 'ff0000';
  TerritoryContour := TRectangularTerritory.Create(
    43.51668853502909, -109.3798828125, 46.98025235521883, -101.865234375);

  TerritoryId := FRoute4MeManager.Territory.Add(
    TerritoryName, TerritoryColor, TerritoryContour, ErrorString);
  AddTerritoriesId(TerritoryId);
  CheckTrue(TerritoryId.IsNotNull);
  CheckEquals(EmptyStr, ErrorString);
end;

procedure TTestTerritoriesSamples.GetTerritory;
var
  ErrorString: String;
  GetEnclosedAddresses: boolean;
  Territory: TTerritory;
begin
  CheckNull(FRoute4MeManager.Territory.Get('-123', GetEnclosedAddresses, ErrorString));
  CheckNotEquals(EmptyStr, ErrorString);

  GetEnclosedAddresses := False;
  FTerritory := FRoute4MeManager.Territory.Get(
    FTerritoryId, GetEnclosedAddresses, ErrorString);
  CheckNotNull(FTerritory);
  CheckEquals(EmptyStr, ErrorString);
  CheckEquals(0, Length(FTerritory.AddressIds));

  GetEnclosedAddresses := True;
  Territory := FRoute4MeManager.Territory.Get(
    FTerritoryId, GetEnclosedAddresses, ErrorString);
  try
    CheckNotNull(Territory);
    CheckEquals(EmptyStr, ErrorString);
    CheckTrue(Length(Territory.AddressIds) > 0);
  finally
    FreeAndNil(Territory);
  end;
end;

procedure TTestTerritoriesSamples.GetTerritories;
var
  ErrorString: String;
  Territories: TTerritoryList;
begin
  Territories := FRoute4MeManager.Territory.GetList(ErrorString);
  try
    CheckNotNull(Territories);
    CheckTrue(Territories.Count > 0);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Territories);
  end;
end;

procedure TTestTerritoriesSamples.RemoveTerritories;
var
  ErrorString: String;
begin
  CheckFalse(FRoute4MeManager.Territory.Remove('-123', ErrorString));
  CheckEquals(EmptyStr, ErrorString);

  CheckTrue(FRoute4MeManager.Territory.Remove(FTerritoryId, ErrorString));
  CheckEquals(EmptyStr, ErrorString);

  CheckTrue(FRoute4MeManager.Territory.Remove(FTerritoryIds, ErrorString));
  CheckEquals(EmptyStr, ErrorString);
end;

procedure TTestTerritoriesSamples.UpdateTerritories;
var
  ErrorString: String;
  Territory: TTerritory;
begin
  FTerritory.Name := 'New name';
//  FTerritory.AddAddressId(11553292);

  Territory := FRoute4MeManager.Territory.Update(FTerritory, ErrorString);
  try
    CheckNotNull(Territory);
    CheckEquals(EmptyStr, ErrorString);
    CheckEquals('New name', Territory.Name);
  finally
    FreeAndNil(Territory);
  end;

  FTerritory.Id := NullableString.Null;
  FTerritory.Name := 'Error';
  Territory := FRoute4MeManager.Territory.Update(FTerritory, ErrorString);
  try
    CheckNull(Territory);
    CheckNotEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Territory);
  end;

  FreeAndNil(FTerritory);
end;

initialization
  RegisterTest('Examples\Online\Territories\', TTestTerritoriesSamples.Suite);
  SetLength(FTerritoryIds, 0);
end.
