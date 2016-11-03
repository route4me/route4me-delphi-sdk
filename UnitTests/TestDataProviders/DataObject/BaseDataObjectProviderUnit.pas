unit BaseDataObjectProviderUnit;

interface

uses
  IDataObjectProviderUnit, DataObjectUnit;

type
  // todo: удалить, если не будем делать unit-тест
  TBaseDataObjectProvider = class abstract (TInterfacedObject, IDataObjectProvider)
  protected
    function MakeDataObject: TDataObject; virtual; abstract;
  public
    function DataObject: TDataObject;
  end;

implementation

{ TBaseDataObjectProvider }

function TBaseDataObjectProvider.DataObject: TDataObject;
begin
  Result := MakeDataObject;
end;

end.
