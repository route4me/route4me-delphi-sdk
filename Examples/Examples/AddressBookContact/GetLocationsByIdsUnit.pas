unit GetLocationsByIdsUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetLocationsByIds = class(TBaseExample)
  public
    procedure Execute(AddressesIds: TArray<integer>);
  end;

implementation

uses AddressBookContactUnit;

procedure TGetLocationsByIds.Execute(AddressesIds: TArray<integer>);
var
  ErrorString: String;
  Contacts: TAddressBookContactList;
begin
  Contacts := Route4MeManager.AddressBookContact.Get(AddressesIds, ErrorString);
  try
    WriteLn('');

    if (Contacts.Count > 0) then
    begin
      WriteLn(Format('GetLocationsByIds executed successfully, ' +
        '%d contacts returned', [Contacts.Count]));
      WriteLn('');
    end
    else
      WriteLn(Format('GetLocationsByIds error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Contacts);
  end;
end;

end.
