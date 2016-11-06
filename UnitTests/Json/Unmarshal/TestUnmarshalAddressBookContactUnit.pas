unit TestUnmarshalAddressBookContactUnit;

interface

uses
  TestFramework, SysUtils,
  TestBaseJsonUnmarshalUnit,
  System.Generics.Collections,
  IAddressBookContactProviderUnit;

type
  TTestUnmarshalAddressBookContact = class(TTestBaseJsonUnmarshal)
  private
    procedure CheckEquals(Etalon: IAddressBookContactProvider; TestName: String); overload;
  published
    procedure DefaultAddressBookContact();
    procedure FullAddressBookContact();
    procedure RealAddressBookContact();
  end;

implementation

{ TTestOptimizationParametersToJson }

uses
  Classes, System.JSON,
  DefaultAddressBookContactProviderUnit, FullAddressBookContactProviderUnit,
  AddressBookContactUnit, MarshalUnMarshalUnit,
  RealAddressBookContactProviderUnit;

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
      try
        CheckTrue(AddressBookContact.Equals(Actual));
      finally
        FreeAndNil(Actual);
        FreeAndNil(AddressBookContact);
      end;
    finally
      FreeAndNil(JsonValue);
    end;
  finally
    FreeAndNil(ActualList);
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

procedure TTestUnmarshalAddressBookContact.RealAddressBookContact;
begin
  CheckEquals(
    TRealAddressBookContactProvider.Create,
    'AddressBookContactToJson\RealAddressBookContact');
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('JSON\Unmarshal\', TTestUnmarshalAddressBookContact.Suite);
end.
