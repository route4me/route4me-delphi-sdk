unit AddressBookContactActionsUnit;

interface

uses
  System.Generics.Collections,
  AddressBookContactUnit, AddressBookParametersUnit;

type

  TAddressBookContactActions = class

  public
    function Add(AddressBookContact: TAddressBookContact;
      out errorString: String): boolean;
    function Update(AddressBookContact: TAddressBookContact;
      out errorString: String): boolean;
    function Remove(AddressId: String): boolean; overload;
    function Remove(AddressId: TArray<String>): boolean; overload;
    function Get(AddressBookParameters: TAddressBookParameters;
      out Total: integer; out ErrorString: String): TList<TAddressBookContact>;
  end;

implementation

{ TAddressBookContact }

function TAddressBookContactActions.Remove(AddressId: String): boolean;
begin

end;

function TAddressBookContactActions.Add(AddressBookContact: TAddressBookContact;
  out errorString: String): boolean;
begin

end;

function TAddressBookContactActions.Get(
  AddressBookParameters: TAddressBookParameters; out Total: integer;
  out ErrorString: String): TList<TAddressBookContact>;
begin

end;

function TAddressBookContactActions.Remove(AddressId: TArray<String>): boolean;
begin

end;

function TAddressBookContactActions.Update(
  AddressBookContact: TAddressBookContact; out errorString: String): boolean;
begin

end;

end.
