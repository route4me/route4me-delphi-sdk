unit TestBaseJsonUnmarshalUnit;

interface

uses
  TestFramework, Classes, IOptimizationParametersProviderUnit,
  OptimizationParametersUnit;

type
  TTestBaseJsonUnmarshal = class abstract(TTestCase)
  protected
    procedure SaveTestDataToFile(s: String);
    procedure CheckEquals(Etalon: IOptimizationParametersProvider; TestName: String);
    function EtalonFilename(TestName: String): String;
  end;

implementation

{ TTestOptimizationParametersToJson }

uses MarshalUnMarshalUnit, GenericParametersUnit;

procedure TTestBaseJsonUnmarshal.CheckEquals(Etalon: IOptimizationParametersProvider;
  TestName: String);
var
  ActualList: TStringList;
  Actual: TGenericParameters;
  JsonFilename: String;
begin
  JsonFilename := EtalonFilename(TestName);
  ActualList := TStringList.Create;
  try
    ActualList.LoadFromFile(JsonFilename);

    Actual := TMarshalUnMarshal.FromJson(ActualList.Text);
    CheckTrue(Etalon.OptimizationParameters.Equals(Actual));
  finally
    ActualList.Free;
  end;
  Etalon := nil;
end;

function TTestBaseJsonUnmarshal.EtalonFilename(TestName: String): String;
begin
  Result := '..\..\Etalons\' + TestName + '.json';
end;

procedure TTestBaseJsonUnmarshal.SaveTestDataToFile(s: String);
var
  st: TStringList;
begin
  st := TStringList.Create;
  st.Text := s;
  st.SaveToFile('TestData.txt');
  st.Free;
end;

end.