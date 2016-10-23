unit BaseDataObjectRouteProviderUnit;

interface

uses
  IDataObjectRouteProviderUnit, DataObjectUnit;

type
  TBaseDataObjectRouteProvider = class abstract (TInterfacedObject, IDataObjectRouteProvider)
  protected
    function MakeDataObjectRoute: TDataObjectRoute; virtual; abstract;
  public
    function DataObjectRoute: TDataObjectRoute;
  end;

implementation

{ TBaseDataObjectRouteProvider }

function TBaseDataObjectRouteProvider.DataObjectRoute: TDataObjectRoute;
begin
  Result := MakeDataObjectRoute;
end;

end.
