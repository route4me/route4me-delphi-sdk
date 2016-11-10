unit RemoveAddressBookContactsResponseUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit, AddressNoteUnit;

type
  TRemoveAddressBookContactsResponse = class(TGenericParameters)
  private
    [JSONName('status')]
    FStatus: boolean;
  public
    property Status: boolean read FStatus write FStatus;
  end;

implementation

end.
