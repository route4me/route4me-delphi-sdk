unit SampleDataObjectRouteProviderUnit;

interface

uses
  SysUtils,
  BaseDataObjectRouteProviderUnit, DataObjectUnit;

type
  TSampleDataObjectRouteTestDataProvider = class(TBaseDataObjectRouteProvider)
  protected
    function MakeDataObjectRoute: TDataObjectRoute; override;
  public

  end;
implementation

{ TSampleDataObjectRouteTestDataProvider }

uses
  DateUtils,
  EnumsUnit, UtilsUnit, JSONDictionaryInterceptorObjectUnit;

function TSampleDataObjectRouteTestDataProvider.MakeDataObjectRoute: TDataObjectRoute;
begin
  Result := TDataObjectRoute.Create;
  // Todo: сделать
end;

end.
