unit DefaultAddressBookContactProviderUnit;

interface

uses IAddressBookContactProviderUnit, AddressBookContactUnit;

type
  TDefaultAddressBookContactProvider = class(TInterfacedObject, IAddressBookContactProvider)
  public
    function AddressBookContact: TAddressBookContact;
  end;

implementation

{ TDefaultAddressBookContactProvider }

function TDefaultAddressBookContactProvider.AddressBookContact: TAddressBookContact;
begin
  Result := TAddressBookContact.Create(
    '324 Columbus Ave #1 New York, NY 10023', 40.7803123, -73.9793079);
end;

end.
