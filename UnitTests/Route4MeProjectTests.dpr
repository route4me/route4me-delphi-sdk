program Route4MeProjectTests;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  GUITestRunner,
  Route4MeManagerUnit in '..\Route4MeManagerUnit.pas',
  AddressUnit in '..\DataTypes\AddressUnit.pas',
  OptimizationParametersUnit in '..\QueryTypes\OptimizationParametersUnit.pas',
  GenericParametersUnit in '..\QueryTypes\GenericParametersUnit.pas',
  RouteParametersUnit in '..\DataTypes\RouteParametersUnit.pas',
  DataObjectUnit in '..\DataTypes\DataObjectUnit.pas',
  EnumsUnit in '..\DataTypes\EnumsUnit.pas',
  Route4MeManagerStubUnit in 'Route4MeManager\Route4MeManagerStubUnit.pas',
  IRoute4MeManagerUnit in '..\IRoute4MeManagerUnit.pas',
  NullableBasicTypesUnit in '..\Nullabled\NullableBasicTypesUnit.pas',
  JSONNullableAttributeUnit in '..\Nullabled\JSONNullableAttributeUnit.pas',
  NullableInterceptorUnit in '..\Nullabled\NullableInterceptorUnit.pas',
  JSONNullableConverterUnit in '..\Nullabled\JSONNullableConverterUnit.pas',
  JSONDictionaryInterceptorObjectUnit in '..\Nullabled\JSONDictionaryInterceptorObjectUnit.pas',
  AddressBookContactUnit in '..\DataTypes\AddressBookContactUnit.pas',
  BaseRoute4MeTestUnit in 'Route4MeManager\BaseRoute4MeTestUnit.pas',
  AddressBookContactActionsUnit in '..\Actions\AddressBookContactActionsUnit.pas',
  AddressBookParametersUnit in '..\QueryTypes\AddressBookParametersUnit.pas',
  TestAddressBookContactActionsUnit in 'Actions\TestAddressBookContactActionsUnit.pas',
  TestMultipleDepotMultipleDriverUnit in 'Actions\Optimization\TestMultipleDepotMultipleDriverUnit.pas',
  TestSingleDriverRoundTripUnit in 'Actions\Optimization\TestSingleDriverRoundTripUnit.pas',
  TestSingleDriverRoute10StopsUnit in 'Actions\Optimization\TestSingleDriverRoute10StopsUnit.pas',
  TestRoute4MeManagerUnit in 'Actions\Optimization\TestRoute4MeManagerUnit.pas',
  BaseOptimizationParametersProviderUnit in 'TestDataProviders\OptimizationParameters\BaseOptimizationParametersProviderUnit.pas',
  IOptimizationParametersProviderUnit in 'TestDataProviders\OptimizationParameters\IOptimizationParametersProviderUnit.pas',
  MultipleDepotMultipleDriverTestDataProviderUnit in 'TestDataProviders\OptimizationParameters\MultipleDepotMultipleDriverTestDataProviderUnit.pas',
  MultipleDepotMultipleDriverTimeWindowTestDataProviderUnit in 'TestDataProviders\OptimizationParameters\MultipleDepotMultipleDriverTimeWindowTestDataProviderUnit.pas',
  MultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProviderUnit in 'TestDataProviders\OptimizationParameters\MultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProviderUnit.pas',
  SingleDepotMultipleDriverNoTimeWindowTestDataProviderUnit in 'TestDataProviders\OptimizationParameters\SingleDepotMultipleDriverNoTimeWindowTestDataProviderUnit.pas',
  SingleDriverMultipleTimeWindowsTestDataProviderUnit in 'TestDataProviders\OptimizationParameters\SingleDriverMultipleTimeWindowsTestDataProviderUnit.pas',
  SingleDriverRoundTripTestDataProviderUnit in 'TestDataProviders\OptimizationParameters\SingleDriverRoundTripTestDataProviderUnit.pas',
  SingleDriverRoute10StopsTestDataProviderUnit in 'TestDataProviders\OptimizationParameters\SingleDriverRoute10StopsTestDataProviderUnit.pas',
  IAddressBookContactProviderUnit in 'TestDataProviders\AddressBookContact\IAddressBookContactProviderUnit.pas',
  FullAddressBookContactProviderUnit in 'TestDataProviders\AddressBookContact\FullAddressBookContactProviderUnit.pas',
  DefaultAddressBookContactProviderUnit in 'TestDataProviders\AddressBookContact\DefaultAddressBookContactProviderUnit.pas',
  SettingsUnit in '..\Common\SettingsUnit.pas',
  UtilsUnit in '..\Common\UtilsUnit.pas',
  ConnectionUnit in '..\Common\ConnectionUnit.pas',
  IConnectionUnit in '..\Common\IConnectionUnit.pas',
  BaseActionUnit in '..\Actions\BaseActionUnit.pas',
  MarshalUnMarshalUnit in '..\Common\MarshalUnMarshalUnit.pas',
  TestMarshalAddressBookContactUnit in 'Json\Marshal\TestMarshalAddressBookContactUnit.pas',
  TestMarshalNullableUnit in 'Json\Marshal\TestMarshalNullableUnit.pas',
  TestMarshalOptimizationParametersUnit in 'Json\Marshal\TestMarshalOptimizationParametersUnit.pas',
  TestUnmarshalOptimizationParametersUnit in 'Json\Unmarshal\TestUnmarshalOptimizationParametersUnit.pas',
  TestBaseJsonMarshalUnit in 'Json\Marshal\TestBaseJsonMarshalUnit.pas',
  TestBaseJsonUnmarshalUnit in 'Json\Unmarshal\TestBaseJsonUnmarshalUnit.pas',
  TestUnmarshalNullableUnit in 'Json\Unmarshal\TestUnmarshalNullableUnit.pas',
  TestUnmarshalAddressBookContactUnit in 'Json\Unmarshal\TestUnmarshalAddressBookContactUnit.pas',
  HttpQueryMemberAttributeUnit in '..\QueryTypes\HttpQueryMemberAttributeUnit.pas',
  DirectionLocationUnit in '..\DataTypes\DirectionLocationUnit.pas',
  DirectionPathPointUnit in '..\DataTypes\DirectionPathPointUnit.pas',
  DirectionStepUnit in '..\DataTypes\DirectionStepUnit.pas',
  DirectionUnit in '..\DataTypes\DirectionUnit.pas',
  LinksUnit in '..\DataTypes\LinksUnit.pas',
  TrackingHistoryUnit in '..\DataTypes\TrackingHistoryUnit.pas',
  OptimizationActionUnit in '..\Actions\OptimizationActionUnit.pas',
  DeleteTestUnit1 in 'DeleteTestUnit1.pas';

{R *.RES}

begin
  GUITestRunner.RunRegisteredTests;
end.

// RSP-16172
