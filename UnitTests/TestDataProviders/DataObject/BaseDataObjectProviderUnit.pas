unit BaseDataObjectProviderUnit;

interface

uses
  IDataObjectProviderUnit, DataObjectUnit;

type
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
