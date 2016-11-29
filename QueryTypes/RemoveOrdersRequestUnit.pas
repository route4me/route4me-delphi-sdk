unit RemoveOrdersRequestUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit, AddressNoteUnit, CommonTypesUnit;

type
  TRemoveOrdersRequest = class(TGenericParameters)
  private
    [JSONName('order_ids')]
    FOrderIds: TIntegerArray;
  public
    property OrderIds: TIntegerArray read FOrderIds write FOrderIds;
  end;

implementation

end.
