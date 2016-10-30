unit GetAddressBookContactsResponseUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit, AddressBookContactUnit;

type
  TGetAddressBookContactsResponse = class(TGenericParameters)
  private
    [JSONName('results')]
    FResults: TAddressBookContactArray;

    [JSONName('total')]
    FTotal: integer;
  public
    property Results: TAddressBookContactArray read FResults write FResults;
    property Total: integer read FTotal write FTotal;
  end;

implementation

end.
