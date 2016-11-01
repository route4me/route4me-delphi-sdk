unit TestBaseJsonMarshalUnit;

interface

uses
  TestFramework, Classes, SysUtils;

type
  TTestBaseJsonMarshal = class abstract(TTestCase)
  protected
    procedure SaveTestDataToFile(s: String);
    procedure CheckEquals(EtalonFilename: String; Actual: String);
    function EtalonFilename(TestName: String): String;
  end;

implementation

{ TTestOptimizationParametersToJson }

procedure TTestBaseJsonMarshal.CheckEquals(EtalonFilename,
  Actual: String);
var
  EtalonList: TStringList;
  ActualList: TStringList;
begin
  EtalonList := TStringList.Create;
  ActualList := TStringList.Create;
  try
    EtalonList.LoadFromFile(EtalonFilename);
    ActualList.Text := Actual;
//    SaveTestDataToFile(Actual);
    CheckTrue(EtalonList.Equals(ActualList));
  finally
    FreeAndNil(ActualList);
    FreeAndNil(EtalonList);
  end;
end;

function TTestBaseJsonMarshal.EtalonFilename(TestName: String): String;
begin
  Result := '..\..\Etalons\' + TestName + '.json';
end;

procedure TTestBaseJsonMarshal.SaveTestDataToFile(s: String);
var
  st: TStringList;
begin
  st := TStringList.Create;
  try
    st.Text := s;
    st.SaveToFile('TestData.txt');
  finally
    FreeAndNil(st);
  end;
end;

end.
