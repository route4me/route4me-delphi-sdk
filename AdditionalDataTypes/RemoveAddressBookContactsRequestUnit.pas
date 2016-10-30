unit RemoveAddressBookContactsRequestUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit, CommonTypesUnit;

type
  TRemoveAddressBookContactsRequest = class(TGenericParameters)
  private
    [JSONName('address_ids')]
    FAddressIds: TStringArray;
  public
    property AddressIds: TStringArray read FAddressIds write FAddressIds;
  end;

implementation

end.
