program Route4MeExamplesProject;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  SingleDriverRoute10StopsUnit in 'Examples\SingleDriverRoute10StopsUnit.pas',
  Route4MeExamplesUnit in 'Examples\Route4MeExamplesUnit.pas',
  JSONDictionaryInterceptorObjectUnit in 'Nullabled\JSONDictionaryInterceptorObjectUnit.pas',
  JSONNullableAttributeUnit in 'Nullabled\JSONNullableAttributeUnit.pas',
  JSONNullableConverterUnit in 'Nullabled\JSONNullableConverterUnit.pas',
  NullableBasicTypesUnit in 'Nullabled\NullableBasicTypesUnit.pas',
  NullableInterceptorUnit in 'Nullabled\NullableInterceptorUnit.pas',
  NullableTypesUnit in 'Nullabled\NullableTypesUnit.pas',
  ConnectionUnit in 'Common\ConnectionUnit.pas',
  IConnectionUnit in 'Common\IConnectionUnit.pas',
  MarshalUnMarshalUnit in 'Common\MarshalUnMarshalUnit.pas',
  SettingsUnit in 'Common\SettingsUnit.pas',
  UtilsUnit in 'Common\UtilsUnit.pas',
  AddressBookContactUnit in 'DataTypes\AddressBookContactUnit.pas',
  AddressUnit in 'DataTypes\AddressUnit.pas',
  DataObjectUnit in 'DataTypes\DataObjectUnit.pas',
  EnumsUnit in 'DataTypes\EnumsUnit.pas',
  RouteParametersUnit in 'DataTypes\RouteParametersUnit.pas',
  AddressBookParametersUnit in 'QueryTypes\AddressBookParametersUnit.pas',
  GenericParametersUnit in 'QueryTypes\GenericParametersUnit.pas',
  OptimizationParametersUnit in 'QueryTypes\OptimizationParametersUnit.pas',
  Route4MeManagerUnit in 'Route4MeManagerUnit.pas',
  BaseOptimizationParametersProviderUnit in 'TestDataProviders\OptimizationParameters\BaseOptimizationParametersProviderUnit.pas',
  IOptimizationParametersProviderUnit in 'TestDataProviders\OptimizationParameters\IOptimizationParametersProviderUnit.pas',
  MultipleDepotMultipleDriverTestDataProviderUnit in 'TestDataProviders\OptimizationParameters\MultipleDepotMultipleDriverTestDataProviderUnit.pas',
  MultipleDepotMultipleDriverTimeWindowTestDataProviderUnit in 'TestDataProviders\OptimizationParameters\MultipleDepotMultipleDriverTimeWindowTestDataProviderUnit.pas',
  MultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProviderUnit in 'TestDataProviders\OptimizationParameters\MultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProviderUnit.pas',
  SingleDepotMultipleDriverNoTimeWindowTestDataProviderUnit in 'TestDataProviders\OptimizationParameters\SingleDepotMultipleDriverNoTimeWindowTestDataProviderUnit.pas',
  SingleDriverMultipleTimeWindowsTestDataProviderUnit in 'TestDataProviders\OptimizationParameters\SingleDriverMultipleTimeWindowsTestDataProviderUnit.pas',
  SingleDriverRoundTripTestDataProviderUnit in 'TestDataProviders\OptimizationParameters\SingleDriverRoundTripTestDataProviderUnit.pas',
  SingleDriverRoute10StopsTestDataProviderUnit in 'TestDataProviders\OptimizationParameters\SingleDriverRoute10StopsTestDataProviderUnit.pas',
  AddressBookContactActionsUnit in 'Actions\AddressBookContactActionsUnit.pas',
  BaseActionUnit in 'Actions\BaseActionUnit.pas',
  OptimizationActionUnit in 'Actions\OptimizationActionUnit.pas',
  DirectionUnit in 'DataTypes\DirectionUnit.pas',
  DirectionLocationUnit in 'DataTypes\DirectionLocationUnit.pas',
  DirectionStepUnit in 'DataTypes\DirectionStepUnit.pas',
  DirectionPathPointUnit in 'DataTypes\DirectionPathPointUnit.pas',
  LinksUnit in 'DataTypes\LinksUnit.pas',
  TrackingHistoryUnit in 'DataTypes\TrackingHistoryUnit.pas',
  HttpQueryMemberAttributeUnit in 'QueryTypes\HttpQueryMemberAttributeUnit.pas';

var
  Examples: TRoute4MeExamples;
  DataObject: TDataObject;

begin
  try
//    Examples := TRoute4MeExamples.Create();
    Examples := TRoute4MeExamples.CreateDebug();

    DataObject := Examples.SingleDriverRoute10Stops();

(*    dataObject = dataObject1;
    DataObjectRoute routeSingleDriverRoute10Stops = (dataObject != null && dataObject.Routes != null && dataObject.Routes.Length > 0) ? dataObject.Routes[0] : null;
    string routeId_SingleDriverRoute10Stops = (routeSingleDriverRoute10Stops != null) ? routeSingleDriverRoute10Stops.RouteID : null;

    if (routeSingleDriverRoute10Stops != null)
      examples.ResequenceRouteDestinations(routeSingleDriverRoute10Stops);
    else
      System.Console.WriteLine("ResequenceRouteDestinations not called. routeSingleDriverRoute10Stops == null.");

    int[] destinationIds = examples.AddRouteDestinations(routeId_SingleDriverRoute10Stops);
    if (destinationIds != null && destinationIds.Length > 0)
    {
      examples.RemoveRouteDestination(routeId_SingleDriverRoute10Stops, destinationIds[0]);
    }*)
    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
