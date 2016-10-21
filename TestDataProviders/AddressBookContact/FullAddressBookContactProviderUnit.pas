unit FullAddressBookContactProviderUnit;

interface

uses IAddressBookContactProviderUnit, AddressBookContactUnit;

type
  TFullAddressBookContactProvider = class(TInterfacedObject, IAddressBookContactProvider)
  public
    function AddressBookContact: TAddressBookContact;
  end;

implementation

{ TFullAddressBookContactProvider }

function TFullAddressBookContactProvider.AddressBookContact: TAddressBookContact;
begin
  Result := TAddressBookContact.Create('17205 RICHMOND TNPK, MILFORD, VA, 22514', 38.024654, -77.338814);
  Result.AddressGroup := 'AddressGroup';
  Result.Alias := '301 MARKET SHELL';
  Result.Address2 := 'Address2';
  Result.FirstName := 'Gela';
  Result.LastName := 'Gorason';
  Result.Email := 'ggora@gmail.com';
  Result.PhoneNumber := '8046335852';
  Result.City := 'Tbilisi';
  Result.StateId := 'TB';
  Result.CountryId := 'GEO';
  Result.Zip := '00167';
  Result.Color := 'Red';

  Result.AddCustomData('sales rep id', '545');
  Result.AddCustomData('sales rep name', 'Kellye Foster');
  Result.AddCustomData('retailer id', '173907');
end;

end.
