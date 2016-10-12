unit AddressBookContactUnit;

interface

type

  TAddressBookContact = class

  public
    function Add(AddressId: String): boolean; overload;
    function Remove(AddressId: String): boolean; overload;
    function Remove(AddressId: TArray<String>): boolean; overload;
  end;

implementation

{ TAddressBookContact }

function TAddressBookContact.Remove(AddressId: String): boolean;
begin

end;

function TAddressBookContact.Remove(AddressId: TArray<String>): boolean;
begin

end;

end.
