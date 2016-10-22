unit DeleteTestUnit1;

interface

uses
  Classes,
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

uses Route4MeManagerUnit, MarshalUnMarshalUnit, EnumsUnit, AddressUnit;

{ TDeleteTestUnit1 }

function TDeleteTestUnit1.GetEtalon: TDataObject;
var
  UserErrors: TArray<String>;
  TAddresses: TArray<TAddress>;
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
begin
  st := TStringList.Create;
  try
    st.LoadFromFile('..\..\testdata.json');
    obj := TMarshalUnMarshal.FromJson(TDataObject, st.Text);

    CheckIs(obj, TDataObject);
    Etalon := GetEtalon;
    try
      CheckTrue(obj.Equals(Etalon));
    finally
      Etalon.Free;
    end;
  finally
    st.Free()
  end;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('Tests', TDeleteTestUnit1.Suite);
end.
