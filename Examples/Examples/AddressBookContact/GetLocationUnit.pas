unit GetLocationUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetLocation = class(TBaseExample)
  public
    procedure Execute(Query: String);
  end;

implementation

uses AddressBookContactUnit;

procedure TGetLocation.Execute(Query: String);
var
  ErrorString: String;
  Total: integer;
  Contacts: TAddressBookContactList;
  Limit, Offset: integer;
begin
  Limit := 10;
  Offset := 0;

  Contacts := Route4MeManager.AddressBookContact.Find(
    Query, Limit, Offset, Total, ErrorString);
  try
    WriteLn('');

    if (Contacts.Count > 0) then
    begin
      WriteLn(Format('GetLocation executed successfully, ' +
        '%d contacts returned, total = %d', [Contacts.Count, Total]));
      WriteLn('');
    end
    else
      WriteLn(Format('GetLocation error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Contacts);
  end;
end;

end.
