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
  Actual: TAvoidanceZone;
  JsonFilename: String;
  AvoidanceZone: TAvoidanceZone;
  JsonValue: TJSONValue;
begin
  JsonFilename := EtalonFilename(TestName);
  ActualList := TStringList.Create;
  try
    ActualList.LoadFromFile(JsonFilename);

    JsonValue := TJSONObject.ParseJSONValue(ActualList.Text);
    try
      AvoidanceZone := Etalon.AvoidanceZone;
      Actual := TMarshalUnMarshal.FromJson(
        AvoidanceZone.ClassType, JsonValue) as TAvoidanceZone;
      try
        CheckTrue(AvoidanceZone.Equals(Actual));
      finally
        FreeAndNil(Actual);
        FreeAndNil(AvoidanceZone);
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
