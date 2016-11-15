unit RemoveAddressBookContactsUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TRemoveAddressBookContacts = class(TBaseExample)
  public
    procedure Execute(AddressIds: TArray<integer>);
  end;

implementation

procedure TRemoveAddressBookContacts.Execute(AddressIds: TArray<integer>);
var
  ErrorString: String;
  Removed: boolean;
begin
  Removed := Route4MeManager.AddressBookContact.Remove(AddressIds, ErrorString);

  WriteLn('');

  if (Removed) then
    WriteLn(Format('RemoveAddressBookContacts executed successfully, %d contacts deleted',
      [Length(AddressIds)]))
  else
    WriteLn(Format('RemoveAddressBookContacts error: "%s"', [ErrorString]));
end;

end.
