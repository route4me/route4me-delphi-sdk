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
  AddressBookContact: TAddressBookContact;
begin
  Provider := TDefaultAddressBookContactProvider.Create;
  try
    AddressBookContact := Provider.AddressBookContact;
    try
      CheckEquals(
        EtalonFilename('AddressBookContactToJson\DefaultAddressBookContact'),
        AddressBookContact.ToJsonString);
    finally
      FreeAndNil(AddressBookContact);
    end;
  finally
    Provider := nil;
  end;
end;

procedure TTestMarshalAddressBookContact.FullAddressBookContact;
var
  Provider: IAddressBookContactProvider;
  AddressBookContact: TAddressBookContact;
begin
  Provider := TFullAddressBookContactProvider.Create;
  try
    AddressBookContact := Provider.AddressBookContact;
    try
      CheckEquals(
        EtalonFilename('AddressBookContactToJson\FullAddressBookContact'),
        AddressBookContact.ToJsonString);
    finally
      FreeAndNil(AddressBookContact);
    end;
  finally
    Provider := nil;
  end;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('JSON\Marshal\', TTestMarshalAddressBookContact.Suite);
end.
