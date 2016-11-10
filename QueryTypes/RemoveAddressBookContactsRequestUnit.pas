unit RemoveAddressBookContactsRequestUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections,
  GenericParametersUnit;

type
  TRemoveAddressBookContactsRequest = class(TGenericParameters)
  private
    [JSONName('address_ids')]
    FAddressIds: TArray<integer>;
  public
    property AddressIds: TArray<integer> read FAddressIds write FAddressIds;
  end;

implementation

end.
