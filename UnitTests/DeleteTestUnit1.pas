unit DeleteTestUnit1;

interface

uses
  Classes, SysUtils,
  TestFramework, IRoute4MeManagerUnit,
  DataObjectUnit;

type
  TDeleteTestUnit1 = class(TTestCase)
  private
    function GetEtalon: TDataObject;
  published
    procedure Test();
  end;

implementation

{ TBaseRoute4MeTest }

uses
  System.JSON,
  Route4MeManagerUnit, MarshalUnMarshalUnit, EnumsUnit, AddressUnit;

{ TDeleteTestUnit1 }

function TDeleteTestUnit1.GetEtalon: TDataObject;
var
  UserErrors: TArray<String>;
  Addresses: TAddressesArray;
begin
  Result := TDataObject.Create;
  Result.State := Integer(TOptimizationState.Optimized);
  SetLength(UserErrors, 0);
  Result.UserErrors := UserErrors;
  Result.IsSentToBackground := False;
//  Result.Addresses := Addresses;
end;

procedure TDeleteTestUnit1.Test;
var
  st: TStringList;
  obj: TObject;
  Etalon: TDataObject;
  JsonValue: TJSONValue;
begin
  st := TStringList.Create;
  try
    st.LoadFromFile('..\..\testdata.json');

    JsonValue := TJSONObject.ParseJSONValue(st.Text);
    try
      obj := TMarshalUnMarshal.FromJson(TDataObject, JsonValue);

      CheckIs(obj, TDataObject);
      Etalon := GetEtalon;
    finally
      FreeAndNil(JsonValue);
    end;
    try
      CheckTrue(obj.Equals(Etalon));
    finally
      FreeAndNil(Etalon);
    end;
  finally
    st.Free()
  end;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('Tests', TDeleteTestUnit1.Suite);
end.
