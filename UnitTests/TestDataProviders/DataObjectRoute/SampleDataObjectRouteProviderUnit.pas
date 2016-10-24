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
  Result.OptimizationProblemId := '41CE7EA6236F2256E0A4335D2683B58B';









  Result.RouteId := '37E9606664879A5F1C7237DED3158580';
  // Todo: сделать
end;

end.
