unit UpdateAddressBookContactUnit;

interface

uses SysUtils, BaseExampleUnit, AddressBookContactUnit;

type
  TUpdateAddressBookContact = class(TBaseExample)
  public
    procedure Execute(Contact: TAddressBookContact);
  end;

implementation

procedure TUpdateAddressBookContact.Execute(Contact: TAddressBookContact);
var
  ErrorString: String;
  UpdatedContact: TAddressBookContact;
begin
  UpdatedContact := Route4MeManager.AddressBookContact.Update(Contact, ErrorString);
  try
    WriteLn('');

    if (UpdatedContact <> nil) then
      WriteLn('UpdateAddressBookContact executed successfully')
    else
      WriteLn(Format('UpdateAddressBookContact error: "%s"', [ErrorString]));
  finally
    FreeAndNil(UpdatedContact);
  end;
end;

end.
