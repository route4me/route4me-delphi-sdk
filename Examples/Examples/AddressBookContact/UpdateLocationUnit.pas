unit UpdateLocationUnit;

interface

uses SysUtils, BaseExampleUnit, AddressBookContactUnit;

type
  TUpdateLocation = class(TBaseExample)
  public
    procedure Execute(Contact: TAddressBookContact);
  end;

implementation

procedure TUpdateLocation.Execute(Contact: TAddressBookContact);
var
  ErrorString: String;
  UpdatedContact: TAddressBookContact;
begin
  UpdatedContact := Route4MeManager.AddressBookContact.Update(Contact, ErrorString);
  try
    WriteLn('');

    if (UpdatedContact <> nil) then
      WriteLn('UpdateLocation executed successfully')
    else
      WriteLn(Format('UpdateLocation error: "%s"', [ErrorString]));
  finally
    FreeAndNil(UpdatedContact);
  end;
end;

end.
