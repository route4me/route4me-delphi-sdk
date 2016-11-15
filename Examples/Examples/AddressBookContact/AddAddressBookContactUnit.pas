unit AddAddressBookContactUnit;

interface

uses SysUtils, BaseExampleUnit, AddressBookContactUnit;

type
  TAddAddressBookContact = class(TBaseExample)
  public
    function Execute(FirstName, Address: String): TAddressBookContact;
  end;

implementation

function TAddAddressBookContact.Execute(
  FirstName, Address: String): TAddressBookContact;
var
  ErrorString: String;
  Contact: TAddressBookContact;
begin
  Contact := TAddressBookContact.Create();
  try
    Contact.FirstName := FirstName;
    Contact.Address := Address;
    Contact.Latitude := 38.024654;
    Contact.Longitude := -77.338814;

    Result := Route4MeManager.AddressBookContact.Add(Contact, ErrorString);

    WriteLn('');

    if (Result <> nil) then
    begin
      WriteLn('AddAddressBookContact executed successfully');
      WriteLn(Format('AddressId: %d', [Result.Id.Value]));
    end
    else
      WriteLn(Format('AddAddressBookContact error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Contact);
  end;
end;

end.
