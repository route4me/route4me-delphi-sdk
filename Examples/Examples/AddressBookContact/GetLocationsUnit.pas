unit GetLocationsUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetLocations = class(TBaseExample)
  public
    procedure Execute;
  end;

implementation

uses AddressBookContactUnit;

procedure TGetLocations.Execute;
var
  ErrorString: String;
  Total: integer;
  Contacts: TAddressBookContactList;
  Limit, Offset: integer;
begin
  Limit := 10;
  Offset := 0;

  Contacts := Route4MeManager.AddressBookContact.Get(Limit, Offset, Total, ErrorString);
  try
    WriteLn('');

    if (Contacts.Count > 0) then
    begin
      WriteLn(Format('GetLocations executed successfully, ' +
        '%d contacts returned, total = %d', [Contacts.Count, Total]));
      WriteLn('');
    end
    else
      WriteLn(Format('GetLocations error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Contacts);
  end;
end;

end.
