unit DisplayRoutedUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TDisplayRouted = class(TBaseExample)
  public
    procedure Execute;
  end;

implementation

uses AddressBookContactUnit, EnumsUnit;

procedure TDisplayRouted.Execute;
var
  ErrorString: String;
  Total: integer;
  Contacts: TAddressBookContactList;
  Limit, Offset: integer;
begin
  Limit := 10;
  Offset := 0;

  Contacts := Route4MeManager.AddressBookContact.Find(
    TDisplayLocations.dlRouted, Limit, Offset, Total, ErrorString);
  try
    WriteLn('');

    if (Contacts.Count > 0) then
    begin
      WriteLn(Format('DisplayRouted executed successfully, ' +
        '%d contacts returned, total = %d', [Contacts.Count, Total]));
      WriteLn('');
    end
    else
      WriteLn(Format('DisplayRouted error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Contacts);
  end;
end;

end.
