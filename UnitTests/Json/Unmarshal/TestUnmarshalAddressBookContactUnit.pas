unit TestUnmarshalAddressBookContactUnit;

interface

uses
  TestFramework,
  TestBaseJsonUnmarshalUnit,
  IAddressBookContactProviderUnit;

type
  TTestUnmarshalAddressBookContact = class(TTestBaseJsonUnmarshal)
  private
    procedure CheckEquals(Etalon: IAddressBookContactProvider; TestName: String);
  published
    procedure DefaultAddressBookContact();
    procedure FullAddressBookContact();
  end;

implementation

{ TTestOptimizationParametersToJson }

uses
  Classes, System.JSON,
  DefaultAddressBookContactProviderUnit, FullAddressBookContactProviderUnit,
  AddressBookContactUnit, MarshalUnMarshalUnit;

procedure TTestUnmarshalAddressBookContact.CheckEquals(
  Etalon: IAddressBookContactProvider; TestName: String);
var
  ActualList: TStringList;
  Actual: TAddressBookContact;
  JsonFilename: String;
  AddressBookContact: TAddressBookContact;
  JsonValue: TJSONValue;
begin
  JsonFilename := EtalonFilename(TestName);
  ActualList := TStringList.Create;
  try
    ActualList.LoadFromFile(JsonFilename);

    JsonValue := TJSONObject.ParseJSONValue(ActualList.Text);
    try
      AddressBookContact := Etalon.AddressBookContact;
      Actual := TMarshalUnMarshal.FromJson(
        AddressBookContact.ClassType, JsonValue) as TAddressBookContact;
    finally
      JsonValue.Free;
    end;

    CheckTrue(AddressBookContact.Equals(Actual));
  finally
    ActualList.Free;
  end;
  Etalon := nil;
end;

procedure TTestUnmarshalAddressBookContact.DefaultAddressBookContact;
begin
  CheckEquals(
    TDefaultAddressBookContactProvider.Create,
    'AddressBookContactToJson\DefaultAddressBookContact');
end;

procedure TTestUnmarshalAddressBookContact.FullAddressBookContact;
begin
  CheckEquals(
    TFullAddressBookContactProvider.Create,
    'AddressBookContactToJson\FullAddressBookContact');
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('JSON\Unmarshal\', TTestUnmarshalAddressBookContact.Suite);
end.
