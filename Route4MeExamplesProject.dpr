program Route4MeExamplesProject;

{$APPTYPE CONSOLE}

{$R *.res}

//  FastMM4 in 'FastMM\FastMM4.pas',
//  FastMM4Messages in 'FastMM\FastMM4Messages.pas',

uses
//  FastMM4 in 'FastMM\FastMM4.pas',
//  FastMM4Messages in 'FastMM\FastMM4Messages.pas',

  System.SysUtils,
  Route4MeExamplesUnit in 'Examples\Route4MeExamplesUnit.pas',
  JSONDictionaryInterceptorObjectUnit in 'Nullabled\JSONDictionaryInterceptorObjectUnit.pas',
  JSONNullableAttributeUnit in 'Nullabled\JSONNullableAttributeUnit.pas',
  JSONNullableConverterUnit in 'Nullabled\JSONNullableConverterUnit.pas',
  NullableBasicTypesUnit in 'Nullabled\NullableBasicTypesUnit.pas',
  NullableInterceptorUnit in 'Nullabled\NullableInterceptorUnit.pas',
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
  HttpQueryMemberAttributeUnit in 'QueryTypes\HttpQueryMemberAttributeUnit.pas',
  RouteActionUnit in 'Actions\RouteActionUnit.pas',
  AddressesOrderInfoUnit in 'AdditionalDataTypes\AddressesOrderInfoUnit.pas',
  CommonTypesUnit in 'Common\CommonTypesUnit.pas',
  MainExamplesUnit in 'Examples\MainExamplesUnit.pas',
  RemoveRouteDestinationResponseUnit in 'AdditionalDataTypes\RemoveRouteDestinationResponseUnit.pas',
  RemoveRouteDestinationRequestUnit in 'AdditionalDataTypes\RemoveRouteDestinationRequestUnit.pas',
  AddRouteDestinationRequestUnit in 'AdditionalDataTypes\AddRouteDestinationRequestUnit.pas',
  MoveDestinationToRouteResponseUnit in 'AdditionalDataTypes\MoveDestinationToRouteResponseUnit.pas',
  MoveDestinationToRouteRequestUnit in 'AdditionalDataTypes\MoveDestinationToRouteRequestUnit.pas',
  SingleDriverRoundTripGenericTestDataProviderUnit in 'TestDataProviders\OptimizationParameters\SingleDriverRoundTripGenericTestDataProviderUnit.pas',
  SingleDriverRoundTripGenericRequestUnit in 'AdditionalDataTypes\SingleDriverRoundTripGenericRequestUnit.pas',
  SingleDriverRoundTripGenericResponseUnit in 'AdditionalDataTypes\SingleDriverRoundTripGenericResponseUnit.pas',
  IRoute4MeManagerUnit in 'IRoute4MeManagerUnit.pas',
  RouteParametersQueryUnit in 'QueryTypes\RouteParametersQueryUnit.pas',
  DataObjectOptimizationsResponseUnit in 'AdditionalDataTypes\DataObjectOptimizationsResponseUnit.pas',
  RemoveDestinationFromOptimizationResponseUnit in 'AdditionalDataTypes\RemoveDestinationFromOptimizationResponseUnit.pas';

begin
  try
//    ReportMemoryLeaksOnShutdown := True;
    TExamples.Run;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ReadLn;
    end;
  end;
end.
