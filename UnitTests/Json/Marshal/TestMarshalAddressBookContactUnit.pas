unit TestMarshalAddressBookContactUnit;

interface

uses
  TestFramework,
  SysUtils, TestBaseJsonMarshalUnit;

type
  TTestMarshalAddressBookContact = class(TTestBaseJsonMarshal)
  published
    procedure DefaultAddressBookContact();
    procedure FullAddressBookContact();
  end;

implementation

{ TTestAddressBookContactToJson }

uses AddressBookContactUnit, IAddressBookContactProviderUnit,
  FullAddressBookContactProviderUnit, DefaultAddressBookContactProviderUnit;

procedure TTestMarshalAddressBookContact.DefaultAddressBookContact;
var
  Provider: IAddressBookContactProvider;
begin
  Provider := TDefaultAddressBookContactProvider.Create;
  try
    CheckEquals(
      EtalonFilename('AddressBookContactToJson\DefaultAddressBookContact'),
      Provider.AddressBookContact.ToJsonString);
  finally
    Provider := nil;
  end;
end;

procedure TTestMarshalAddressBookContact.FullAddressBookContact;
var
  Provider: IAddressBookContactProvider;
begin
  Provider := TFullAddressBookContactProvider.Create;
  try
    CheckEquals(
      EtalonFilename('AddressBookContactToJson\FullAddressBookContact'),
      Provider.AddressBookContact.ToJsonString);
  finally
    Provider := nil;
  end;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('JSON\Marshal\', TTestMarshalAddressBookContact.Suite);
end.
