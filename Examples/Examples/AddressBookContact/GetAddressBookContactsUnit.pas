unit GetAddressBookContactsUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetAddressBookContacts = class(TBaseExample)
  public
    procedure Execute;
  end;

implementation

uses AddressBookParametersUnit, AddressBookContactUnit;

procedure TGetAddressBookContacts.Execute;
var
  ErrorString: String;
  Parameters: TAddressBookParameters;
  Total: integer;
  Contacts: TAddressBookContactArray;
  i: integer;
begin
  Parameters := TAddressBookParameters.Create();
  try
    Parameters.Limit := 10;
    Parameters.Offset := 0;

    Contacts := Route4MeManager.AddressBookContact.Get(Parameters, Total, ErrorString);
    try
      WriteLn('');

      if (Length(Contacts) > 0) then
      begin
        WriteLn(Format('GetAddressBookContacts executed successfully, ' +
          '%d contacts returned, total = %d', [Length(Contacts), Total]));
        WriteLn('');
      end
      else
        WriteLn(Format('GetAddressBookContacts error: "%s"', [ErrorString]));
    finally
      for i := Length(Contacts) - 1 downto 0 do
        FreeAndNil(Contacts[i]);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
