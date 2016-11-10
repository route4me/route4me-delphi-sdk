unit RemoveOrdersRequestUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit, AddressNoteUnit, CommonTypesUnit;

type
  TRemoveOrdersRequest = class(TGenericParameters)
  private
    [JSONName('order_ids')]
    FOrderIds: TStringArray;
  public
    property OrderIds: TStringArray read FOrderIds write FOrderIds;
  end;

implementation

end.
