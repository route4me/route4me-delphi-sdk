unit TestBaseJsonUnmarshalUnit;

interface

uses
  TestFramework, Classes, SysUtils;

type
  TTestBaseJsonUnmarshal = class abstract(TTestCase)
  protected
    procedure SaveTestDataToFile(s: String);
    function EtalonFilename(TestName: String): String;
  end;

implementation

{ TTestOptimizationParametersToJson }

uses MarshalUnMarshalUnit, GenericParametersUnit;

function TTestBaseJsonUnmarshal.EtalonFilename(TestName: String): String;
begin
  Result := '..\..\Etalons\' + TestName + '.json';
end;

procedure TTestBaseJsonUnmarshal.SaveTestDataToFile(s: String);
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