unit IAddressBookContactProviderUnit;

interface

uses
  AddressBookContactUnit;

type
  IAddressBookContactProvider = interface
    ['{E03EDA15-6146-4119-80BF-C18B1FE8E3BE}']

    function AddressBookContact: TAddressBookContact;
  end;

implementation

end.
