unit TestUnmarshalDataObjectUnit;

interface

uses
  TestFramework, SysUtils,
  TestBaseJsonUnmarshalUnit, IDataObjectProviderUnit;

type
  TTestUnmarshalDataObject = class(TTestBaseJsonUnmarshal)
  private
    procedure CheckEquals(Etalon: IDataObjectProvider; TestName: String);
  published
    procedure SingleDriverRoute10Stops();
  end;

implementation

{ TTestOptimizationParametersToJson }

uses
  Classes, System.JSON,
  SampleDataObjectRouteProviderUnit,
  DataObjectUnit, MarshalUnMarshalUnit,
  SingleDriverRoute10StopsResponseDataProviderUnit;

procedure TTestUnmarshalDataObject.CheckEquals(
  Etalon: IDataObjectProvider; TestName: String);
var
  ActualList: TStringList;
  Actual: TDataObject;
  JsonFilename: String;
  DataObject: TDataObject;
  JsonValue: TJSONValue;
begin
  JsonFilename := EtalonFilename(TestName);
  ActualList := TStringList.Create;
  try
    ActualList.LoadFromFile(JsonFilename);

    JsonValue := TJSONObject.ParseJSONValue(ActualList.Text);
    try
      DataObject := Etalon.DataObject;
      Actual := TMarshalUnMarshal.FromJson(DataObject.ClassType, JsonValue) as TDataObject;
    finally
      FreeAndNil(JsonValue);
    end;

    CheckTrue(DataObject.Equals(Actual));
  finally
    FreeAndNil(ActualList);
  end;
  Etalon := nil;
end;

procedure TTestUnmarshalDataObject.SingleDriverRoute10Stops;
begin
  CheckEquals(
    TSingleDriverRoute10StopsResponseDataProvider.Create,
    'Route4MeExamples\Responses\SingleDriverRoute10Stops');
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('JSON\Unmarshal\', TTestUnmarshalDataObject.Suite);
end.
