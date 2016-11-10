unit RealAddressBookContactProviderUnit;

interface

uses IAddressBookContactProviderUnit, AddressBookContactUnit;

type
  TRealAddressBookContactProvider = class(TInterfacedObject, IAddressBookContactProvider)
  public
    function AddressBookContact: TAddressBookContact;
  end;

implementation

{ TRealAddressBookContactProvider }

function TRealAddressBookContactProvider.AddressBookContact: TAddressBookContact;
begin
  Result := TAddressBookContact.Create('Test Address 1', 38.024654, -77.338814);
  Result.Id := 10702048;
  Result.FirstName := 'Test FirstName 1';
  Result.LastName := '';
  Result.Email := '';
  Result.PhoneNumber := '';
  Result.AddressGroup := '';
  Result.Address2 := '';
  Result.Alias := '';
  Result.City := '';
  Result.Zip := '';
  Result.CountryId := '0';
  Result.CurbsideLatitude := 38.024654;
  Result.CurbsideLongitude := -77.338814;
end;

end.
