program Route4MeProjectTests;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  GUITestRunner,
  TestSingleDriverRoute10StopsUnit in 'Route4MeManager\TestSingleDriverRoute10StopsUnit.pas',
  TestSingleDriverRoundTripUnit in 'Route4MeManager\TestSingleDriverRoundTripUnit.pas',
  Route4MeManagerUnit in '..\Route4MeManagerUnit.pas',
  BaseRoute4MeTestUnit in 'BaseRoute4MeTestUnit.pas',
  TestRoute4MeManagerUnit in 'Route4MeManager\TestRoute4MeManagerUnit.pas',
  AddressUnit in '..\DataTypes\AddressUnit.pas',
  OptimizationParametersUnit in '..\QueryTypes\OptimizationParametersUnit.pas',
  GenericParametersUnit in '..\QueryTypes\GenericParametersUnit.pas',
  RouteParametersUnit in '..\DataTypes\RouteParametersUnit.pas',
  DataObjectUnit in '..\DataTypes\DataObjectUnit.pas',
  EnumsUnit in '..\DataTypes\EnumsUnit.pas',
  R4MeInfrastructureSettingsUnit in '..\R4MeInfrastructureSettingsUnit.pas',
  UtilsUnit in '..\UtilsUnit.pas',
  Route4MeManagerStubUnit in 'Route4MeManager\Route4MeManagerStubUnit.pas',
  IRoute4MeManagerUnit in '..\IRoute4MeManagerUnit.pas',
  TestOptimizationParametersToJsonUnit in 'TestOptimizationParametersToJsonUnit.pas',
  JsonSerializerUnit in '..\JsonSerializerUnit.pas',
  NullableBasicTypesUnit in '..\Nullabled\NullableBasicTypesUnit.pas',
  JSONNullableAttributeUnit in '..\Nullabled\JSONNullableAttributeUnit.pas',
  NullableInterceptorUnit in '..\Nullabled\NullableInterceptorUnit.pas',
  JSONNullableConverterUnit in '..\Nullabled\JSONNullableConverterUnit.pas',
  TestNullableJsonSerializationUnit in 'TestNullableJsonSerializationUnit.pas',
  JSONDictionaryInterceptorObjectUnit in '..\Nullabled\JSONDictionaryInterceptorObjectUnit.pas',
  SingleDriverRoute10StopsTestDataProviderUnit in 'TestDataProviders\SingleDriverRoute10StopsTestDataProviderUnit.pas',
  IOptimizationParametersProviderUnit in 'TestDataProviders\IOptimizationParametersProviderUnit.pas',
  BaseOptimizationParametersProviderUnit in 'TestDataProviders\BaseOptimizationParametersProviderUnit.pas',
  SingleDriverRoundTripTestDataProviderUnit in 'TestDataProviders\SingleDriverRoundTripTestDataProviderUnit.pas',
  TestMultipleDepotMultipleDriverUnit in 'Route4MeManager\TestMultipleDepotMultipleDriverUnit.pas',
  MultipleDepotMultipleDriverTestDataProviderUnit in 'TestDataProviders\MultipleDepotMultipleDriverTestDataProviderUnit.pas',
  SingleDepotMultipleDriverNoTimeWindowTestDataProviderUnit in 'TestDataProviders\SingleDepotMultipleDriverNoTimeWindowTestDataProviderUnit.pas',
  SingleDriverMultipleTimeWindowsTestDataProviderUnit in 'TestDataProviders\SingleDriverMultipleTimeWindowsTestDataProviderUnit.pas',
  MultipleDepotMultipleDriverTimeWindowTestDataProviderUnit in 'TestDataProviders\MultipleDepotMultipleDriverTimeWindowTestDataProviderUnit.pas',
  MultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProviderUnit in 'TestDataProviders\MultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProviderUnit.pas';

{R *.RES}

begin
  GUITestRunner.RunRegisteredTests;
end.
