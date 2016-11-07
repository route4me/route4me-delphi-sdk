unit TestUnmarshalAvoidanceZoneUnit;

interface

uses
  TestFramework, SysUtils,
  TestBaseJsonUnmarshalUnit,
  System.Generics.Collections,
  IAvoidanceZoneProviderUnit;

type
  TTestUnmarshalAvoidanceZone = class(TTestBaseJsonUnmarshal)
  private
    procedure CheckEquals(Etalon: IAvoidanceZoneProvider; TestName: String); overload;
  published
    procedure RealAvoidanceZone();
  end;

implementation

uses
  Classes, System.JSON,
  AvoidanceZoneUnit, MarshalUnMarshalUnit,
  RealAvoidanceZoneProviderUnit;

procedure TTestUnmarshalAvoidanceZone.CheckEquals(
  Etalon: IAvoidanceZoneProvider; TestName: String);
var
  ActualList: TStringList;
  Actual: TAvoidanceZoneList;
  JsonFilename: String;
  AvoidanceZones: TAvoidanceZoneList;
  JsonValue: TJSONValue;
  SortedAvoidanceZone1, SortedAvoidanceZone2: TAvoidanceZoneArray;
  i: integer;
begin
  JsonFilename := EtalonFilename(TestName);
  ActualList := TStringList.Create;
  try
    ActualList.LoadFromFile(JsonFilename);

    JsonValue := TJSONObject.ParseJSONValue(ActualList.Text);
    try
      AvoidanceZones := Etalon.AvoidanceZones;
      Actual := TMarshalUnMarshal.FromJson(
        AvoidanceZones.ClassType, JsonValue) as TAvoidanceZoneList;
      try
        CheckEquals(AvoidanceZones.Count, Actual.Count);
        SortedAvoidanceZone1 := AvoidanceZoneUnit.SortAvoidanceZones(AvoidanceZones.ToArray);
        SortedAvoidanceZone2 := AvoidanceZoneUnit.SortAvoidanceZones(Actual.ToArray);
        for i := 0 to Length(SortedAvoidanceZone1) - 1 do
          CheckTrue(SortedAvoidanceZone1[i].Equals(SortedAvoidanceZone2[i]));
      finally
        FreeAndNil(Actual);
        FreeAndNil(AvoidanceZones);
      end;
    finally
      FreeAndNil(JsonValue);
    end;
  finally
    FreeAndNil(ActualList);
  end;
  Etalon := nil;
end;

procedure TTestUnmarshalAvoidanceZone.RealAvoidanceZone;
begin
  CheckEquals(
    TRealAvoidanceZoneProvider.Create,
    'AvoidanceZoneFromJson');
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('JSON\Unmarshal\', TTestUnmarshalAvoidanceZone.Suite);
end.
