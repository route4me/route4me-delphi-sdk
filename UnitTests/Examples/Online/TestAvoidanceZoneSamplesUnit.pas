unit TestAvoidanceZoneSamplesUnit;

interface

uses
  TestFramework, Classes, SysUtils,
  BaseTestOnlineExamplesUnit;

type
  TTestAvoidanceZoneSamples = class(TTestOnlineExamples)
  published
    procedure AddCircleAvoidanceZone;
    procedure AddPolygonAvoidanceZone;
    procedure AddRectangularAvoidanceZone;
    procedure GetAvoidanceZones;
    procedure GetAvoidanceZone;
    procedure UpdateAvoidanceZone;
    procedure RemoveAvoidanceZone;
  end;

implementation

uses NullableBasicTypesUnit, TerritoryContourUnit, AvoidanceZoneUnit,
  PositionUnit;

var
  FAvoidanceZone: TAvoidanceZone;
  FAvoidanceZoneIds: TArray<String>;

procedure AddAvoidanceZoneId(Id: NullableString);
begin
  if (Id.IsNotNull) then
  begin
    SetLength(FAvoidanceZoneIds, Length(FAvoidanceZoneIds) + 1);
    FAvoidanceZoneIds[High(FAvoidanceZoneIds)] := Id;
  end;
end;

procedure TTestAvoidanceZoneSamples.AddCircleAvoidanceZone;
var
  ErrorString: String;
  AvoidanceZone: TAvoidanceZone;
  Parameters: TAvoidanceZone;
  TerritoryName, TerritoryColor: String;
  Territory: TTerritoryContour;
begin
  TerritoryName := 'Circle Territory';
  TerritoryColor := 'ff0000';
  Territory := TTerritoryContour.MakeCircleContour(
    37.5697528227865, -77.4783325195313, 5000);
  Parameters := TAvoidanceZone.Create(TerritoryName, TerritoryColor, Territory);
  try
    FAvoidanceZone := FRoute4MeManager.AvoidanceZone.Add(Parameters, ErrorString);

    CheckNotNull(AvoidanceZone);
    CheckTrue(FAvoidanceZone.TerritoryId.IsNotNull);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Parameters);
  end;

  Parameters := TAvoidanceZone.Create(TerritoryName, TerritoryColor, nil);
  try
    AvoidanceZone := FRoute4MeManager.AvoidanceZone.Add(Parameters, ErrorString);
    try
      CheckNull(AvoidanceZone);
      CheckNotEquals(EmptyStr, ErrorString);
    finally
      FreeAndNil(AvoidanceZone);
    end;
  finally
    FreeAndNil(Parameters);
  end;

  TerritoryName := '';
  TerritoryColor := 'ff0000';
  Territory := TTerritoryContour.MakeCircleContour(
    37.5697528227865, -77.4783325195313, 5000);
  Parameters := TAvoidanceZone.Create(TerritoryName, TerritoryColor, Territory);
  try
    AvoidanceZone := FRoute4MeManager.AvoidanceZone.Add(Parameters, ErrorString);
    try
      CheckNull(AvoidanceZone);
      CheckNotEquals(EmptyStr, ErrorString);
    finally
      FreeAndNil(AvoidanceZone);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

procedure TTestAvoidanceZoneSamples.AddPolygonAvoidanceZone;
var
  ErrorString: String;
  AvoidanceZone: TAvoidanceZone;
  Parameters: TAvoidanceZone;
  TerritoryName, TerritoryColor: String;
  Territory: TTerritoryContour;
begin
  TerritoryName := 'Polygon Territory';
  TerritoryColor := 'ff0000';
  Territory := TTerritoryContour.MakePolygonContour([
    TPosition.Create(37.76975282278,-77.67833251953),
    TPosition.Create(37.75886716305,-77.68974800109),
    TPosition.Create(37.74763966054,-77.69172210693)
  ]);
  Parameters := TAvoidanceZone.Create(TerritoryName, TerritoryColor, Territory);
  try
    AvoidanceZone := FRoute4MeManager.AvoidanceZone.Add(Parameters, ErrorString);
    try
      CheckNotNull(AvoidanceZone);
      AddAvoidanceZoneId(AvoidanceZone.TerritoryId);
      CheckTrue(AvoidanceZone.TerritoryId.IsNotNull);
      CheckEquals(EmptyStr, ErrorString);
    finally
      FreeAndNil(AvoidanceZone);
    end;
  finally
    FreeAndNil(Parameters);
  end;

  TerritoryName := 'Polygon Territory';
  TerritoryColor := 'ff0000';
  Territory := TTerritoryContour.MakePolygonContour([
    TPosition.Create(37.76975282278,-77.67833251953),
    TPosition.Create(37.75886716305,-77.68974800109)
  ]);
  Parameters := TAvoidanceZone.Create(TerritoryName, TerritoryColor, Territory);
  try
    AvoidanceZone := FRoute4MeManager.AvoidanceZone.Add(Parameters, ErrorString);
    try
      CheckNotNull(AvoidanceZone);
      AddAvoidanceZoneId(AvoidanceZone.TerritoryId);
      CheckTrue(AvoidanceZone.TerritoryId.IsNotNull);
      CheckEquals(EmptyStr, ErrorString);
    finally
      FreeAndNil(AvoidanceZone);
    end;
  finally
    FreeAndNil(Parameters);
  end;

  TerritoryName := 'Polygon Territory';
  TerritoryColor := 'ff0000';
  Territory := TTerritoryContour.MakePolygonContour([
    TPosition.Create(37.76975282278,-77.67833251953)
  ]);
  Parameters := TAvoidanceZone.Create(TerritoryName, TerritoryColor, Territory);
  try
    AvoidanceZone := FRoute4MeManager.AvoidanceZone.Add(Parameters, ErrorString);
    try
      CheckNotNull(AvoidanceZone);
      AddAvoidanceZoneId(AvoidanceZone.TerritoryId);
      CheckTrue(AvoidanceZone.TerritoryId.IsNotNull);
      CheckEquals(EmptyStr, ErrorString);
    finally
      FreeAndNil(AvoidanceZone);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

procedure TTestAvoidanceZoneSamples.AddRectangularAvoidanceZone;
var
  ErrorString: String;
  AvoidanceZone: TAvoidanceZone;
  Parameters: TAvoidanceZone;
  TerritoryName, TerritoryColor: String;
  Territory: TTerritoryContour;
begin
  TerritoryName := 'Rect Territory';
  TerritoryColor := 'ff0000';
  Territory := TTerritoryContour.MakeRectangularContour(
    43.51668853502909, -109.3798828125, 46.98025235521883, -101.865234375);
  Parameters := TAvoidanceZone.Create(TerritoryName, TerritoryColor, Territory);
  try
    AvoidanceZone := FRoute4MeManager.AvoidanceZone.Add(Parameters, ErrorString);
    try
      CheckNotNull(AvoidanceZone);
      AddAvoidanceZoneId(AvoidanceZone.TerritoryId);
      CheckTrue(AvoidanceZone.TerritoryId.IsNotNull);
      CheckEquals(EmptyStr, ErrorString);
    finally
      FreeAndNil(AvoidanceZone);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

procedure TTestAvoidanceZoneSamples.GetAvoidanceZone;
var
  ErrorString: String;
  AvoidanceZone: TAvoidanceZone;
begin
  AvoidanceZone := FRoute4MeManager.AvoidanceZone.Get('-123', ErrorString);
  try
    CheckNull(AvoidanceZone);
    CheckNotEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(AvoidanceZone);
  end;

  AvoidanceZone := FRoute4MeManager.AvoidanceZone.Get(
    FAvoidanceZone.TerritoryId, ErrorString);
  try
    CheckNotNull(AvoidanceZone);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(AvoidanceZone);
  end;
end;

procedure TTestAvoidanceZoneSamples.GetAvoidanceZones;
var
  ErrorString: String;
  AvoidanceZones: TAvoidanceZoneList;
begin
  AvoidanceZones := FRoute4MeManager.AvoidanceZone.GetList(ErrorString);
  try
    CheckNotNull(AvoidanceZones);
    CheckTrue(AvoidanceZones.Count > 0);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(AvoidanceZones);
  end;
end;

procedure TTestAvoidanceZoneSamples.RemoveAvoidanceZone;
var
  ErrorString: String;
begin
  CheckTrue(FRoute4MeManager.AvoidanceZone.Remove('-123', ErrorString));
  CheckEquals(EmptyStr, ErrorString);

  CheckTrue(FRoute4MeManager.AvoidanceZone.Remove(FAvoidanceZone.TerritoryId, ErrorString));
  CheckEquals(EmptyStr, ErrorString);

  CheckTrue(FRoute4MeManager.AvoidanceZone.Remove(FAvoidanceZoneIds, ErrorString));
  CheckEquals(EmptyStr, ErrorString);

  FreeAndNil(FAvoidanceZone);
end;

procedure TTestAvoidanceZoneSamples.UpdateAvoidanceZone;
var
  ErrorString: String;
  AvoidanceZone: TAvoidanceZone;
  Id: String;
begin
  FAvoidanceZone.TerritoryName := 'New name';

  AvoidanceZone := FRoute4MeManager.AvoidanceZone.Update(FAvoidanceZone, ErrorString);
  try
    CheckNotNull(AvoidanceZone);
    CheckEquals(EmptyStr, ErrorString);
    CheckEquals('New name', AvoidanceZone.TerritoryName);
  finally
    FreeAndNil(AvoidanceZone)
  end;

  Id := FAvoidanceZone.TerritoryId;
  FAvoidanceZone.TerritoryId := NullableString.Null;
  try
    FAvoidanceZone.TerritoryName := 'Error';
    AvoidanceZone := FRoute4MeManager.AvoidanceZone.Update(FAvoidanceZone, ErrorString);
    try
      CheckNull(AvoidanceZone);
      CheckNotEquals(EmptyStr, ErrorString);
    finally
      FreeAndNil(AvoidanceZone)
    end;
  finally
    FAvoidanceZone.TerritoryId := Id;
  end;
end;

initialization
  RegisterTest('Examples\Online\AvoidanceZone\', TTestAvoidanceZoneSamples.Suite);
  SetLength(FAvoidanceZoneIds, 0);
end.
