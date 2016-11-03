unit SampleDataObjectRouteProviderUnit;

interface

uses
  SysUtils,
  BaseDataObjectRouteProviderUnit, DataObjectUnit;

type
  // todo: удалить, если не будем делать unit-тест
  TSampleDataObjectRouteTestDataProvider = class(TBaseDataObjectRouteProvider)
  protected
    function MakeDataObjectRoute: TDataObjectRoute; override;
  public

  end;
implementation

{ TSampleDataObjectRouteTestDataProvider }

uses
  DateUtils,
  EnumsUnit, UtilsUnit, JSONDictionaryIntermediateObjectUnit,
  RouteParametersUnit, LinksUnit, AddressUnit;

function TSampleDataObjectRouteTestDataProvider.MakeDataObjectRoute: TDataObjectRoute;
{var
  Parameters: TRouteParameters;
  Route: TDataObjectRoute;
  Address1, Address2, Address3, Address4, Address5, Address6, Address7, Address8, Address9, Address10: TAddress;}
begin
  Result := nil;
{  Parameters := TRouteParameters.Create;
  Parameters.IsUpload := False;
  Parameters.RT := False;
  Parameters.RouteName := 'Single Driver Route 10 Stops';
  Parameters.RouteDate := 0;
  Parameters.RouteTime := 25200;

  Route := TDataObjectRoute.Create;
  Address1 := TAddress.Create;

  Result := TDataObjectRoute.Create;
  Result.OptimizationProblemId := '9B52B812693D8324DC4E8C8392D12B54';
  Result.State := TOptimizationState.Optimized;
  Result.CreatedTimestamp := 1477738597;
  Result.Parameters := Parameters;
  Result.AddAddress(Address1);
  Result.AddAddress(Address2);
  Result.AddAddress(Address3);
  Result.AddAddress(Address4);
  Result.AddAddress(Address5);
  Result.AddAddress(Address6);
  Result.AddAddress(Address7);
  Result.AddAddress(Address8);
  Result.AddAddress(Address9);
  Result.AddAddress(Address10);
  Result.AddRoute(Route);
  Result.Links := TLinks.Create;
  Result.Links.View := 'http://www.route4me.com/api.v4/optimization_problem.php?optimization_problem_id=9B52B812693D8324DC4E8C8392D12B54&api_key=11111111111111111111111111111111&member_id=1';}
end;

end.
