unit TestAddressBookContactActionsUnit;

interface

uses
  TestFramework, BaseRoute4MeTestUnit,
  AddressBookContactUnit,
  IAddressBookContactProviderUnit;

type
  TTestAddressBookContactActions = class(TBaseRoute4MeTest)
  private
    function GetAddressBookContact(Provider: IAddressBookContactProvider): TAddressBookContact;

    // todo: сделать тест
    procedure TestAddGetUpdateRemove;
  published
  end;

implementation

{ TTestAddressBookContactActions }

uses
  SysUtils, System.Generics.Collections,
  DefaultAddressBookContactProviderUnit, FullAddressBookContactProviderUnit,
  AddressBookParametersUnit;

function TTestAddressBookContactActions.GetAddressBookContact(
  Provider: IAddressBookContactProvider): TAddressBookContact;
begin
  Result := Provider.AddressBookContact;
  Provider := nil;
end;

procedure TTestAddressBookContactActions.TestAddGetUpdateRemove;
{var
  AddressBookContact1: TAddressBookContact;
  AddressBookContact2: TAddressBookContact;
  Id1, Id2: String;
  AddressBookContacts: TObjectList<TAddressBookContact>;
  Parameters: TAddressBookParameters;
  Total: integer;
  ErrorString: String;         }
begin
{  AddressBookContact1 := GetAddressBookContact(TDefaultAddressBookContactProvider.Create);
  AddressBookContact2 := GetAddressBookContact(TFullAddressBookContactProvider.Create);

  try
    raise Exception.Create('Закрепить эталон');

    CheckTrue(AddressBookContact1.Id.IsNull);
    FRoute4MeManager.AddressBookContact.Add(AddressBookContact1, ErrorString);
    CheckEquals(EmptyStr, ErrorString);
    CheckFalse(AddressBookContact1.Id.IsNull);
    Id1 := AddressBookContact1.Id;

    CheckTrue(AddressBookContact2.Id.IsNull);
    FRoute4MeManager.AddressBookContact.Add(AddressBookContact2, ErrorString);
    CheckEquals(EmptyStr, ErrorString);
    CheckFalse(AddressBookContact2.Id.IsNull);
    Id2 := AddressBookContact2.Id;

    Parameters := TAddressBookParameters.Create(10, 0);
    try
      AddressBookContacts := FRoute4MeManager.AddressBookContact.Get(Parameters, Total, ErrorString);
      try
        CheckEquals(EmptyStr, ErrorString);
        CheckEquals(2, Total);
        CheckEquals(2, AddressBookContacts.Count);
        CheckNotEquals(AddressBookContacts[0].Id, AddressBookContacts[1].Id);
        CheckTrue((Id1 = AddressBookContacts[0].Id) or (Id1 = AddressBookContacts[0].Id));
        CheckTrue((Id2 = AddressBookContacts[0].Id) or (Id2 = AddressBookContacts[0].Id));
      finally
        FreeAndNil(AddressBookContacts);
      end;

      AddressBookContact2.Id := Id1;
      FRoute4MeManager.AddressBookContact.Update(AddressBookContact2, ErrorString);

      FRoute4MeManager.AddressBookContact.Remove([Id1, Id2]);
      CheckEquals(EmptyStr, ErrorString);

      AddressBookContacts := FRoute4MeManager.AddressBookContact.Get(Parameters, Total, ErrorString);
      try
        CheckEquals(EmptyStr, ErrorString);
        CheckEquals(0, Total);
        CheckEquals(0, AddressBookContacts.Count);
      finally
        FreeAndNil(AddressBookContacts);
      end;
    finally
      FreeAndNil(Parameters);
    end;
  finally
    FreeAndNil(AddressBookContact1);
    FreeAndNil(AddressBookContact2);
  end;}
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('Actions\', TTestAddressBookContactActions.Suite);

end.
