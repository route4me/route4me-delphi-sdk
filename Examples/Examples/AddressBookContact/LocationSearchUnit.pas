unit LocationSearchUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TLocationSearch = class(TBaseExample)
  public
    procedure Execute(Query: String; Fields: TArray<String>);
  end;

implementation

uses AddressBookContactUnit;

procedure TLocationSearch.Execute(Query: String; Fields: TArray<String>);

  ErrorString: String;
  Total: integer;
  Contacts: TAddressBookContactList;
  Limit, Offset: integer;
begin
  Limit := 10;
  Offset := 0;

  Contacts := Route4MeManager.AddressBookContact.Find(
    Query, Fields, Limit, Offset, Total, ErrorString);
  try
    WriteLn('');

    if (Contacts.Count > 0) then
    begin
      WriteLn(Format('LocationSearch executed successfully, ' +
        '%d contacts returned, total = %d', [Contacts.Count, Total]));
      WriteLn('');
    end
    else
      WriteLn(Format('LocationSearch error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Contacts);
  end;
end;

end.