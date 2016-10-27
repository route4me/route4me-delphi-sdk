unit TestUnmarshalAddressBookContactUnit;

interface

uses
  TestFramework, SysUtils,
  TestBaseJsonUnmarshalUnit,
  IAddressBookContactProviderUnit;

type
  TTestUnmarshalAddressBookContact = class(TTestBaseJsonUnmarshal)
  private
    procedure CheckEquals(Etalon: IAddressBookContactProvider; TestName: String); overload;
  published
    procedure DefaultAddressBookContact();
    procedure FullAddressBookContact();
    procedure AddressBookContactList();
  end;

implementation

{ TTestOptimizationParametersToJson }

uses
  Classes, System.JSON,
  DefaultAddressBookContactProviderUnit, FullAddressBookContactProviderUnit,
  AddressBookContactUnit, MarshalUnMarshalUnit;

procedure TTestUnmarshalAddressBookContact.AddressBookContactList;
var
  JsonString: String;
  JsonValue: TJSONValue;
  ActualList: TAddressBookContactList;
begin
  JsonString := '[{"address_1":"First Address","cached_lat":40.7803123,"cached_lng":-73.9793079},{"address_1":"Second Address","cached_lat":40.7803123,"cached_lng":-73.9793079}]';

  JsonValue := TJSONObject.ParseJSONValue(JsonString);
  try
    ActualList := TMarshalUnMarshal.FromJson(
      TAddressBookContactList, JsonValue) as TAddressBookContactList;
    try
      CheckEquals(2, ActualList.Count);
      CheckEquals('First Address', ActualList[0].Address);
      CheckEquals('Second Address', ActualList[1].Address);
    finally
      FreeAndNil(ActualList);
    end;
  finally
    FreeAndNil(JsonValue);
  end;
end;

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

initialization
  // Register any test cases with the test runner
  RegisterTest('JSON\Unmarshal\', TTestUnmarshalAddressBookContact.Suite);
end.
