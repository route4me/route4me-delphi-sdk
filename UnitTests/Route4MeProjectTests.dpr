program Route4MeProjectTests;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

//  FastMM4 in '..\FastMM\FastMM4.pas',
//  FastMM4Messages in '..\FastMM\FastMM4Messages.pas',

uses
  GUITestRunner,
  Route4MeManagerUnit in '..\Route4MeManagerUnit.pas',
  AddressUnit in '..\DataTypes\AddressUnit.pas',
  OptimizationParametersUnit in '..\QueryTypes\OptimizationParametersUnit.pas',
  GenericParametersUnit in '..\QueryTypes\GenericParametersUnit.pas',
  RouteParametersUnit in '..\DataTypes\RouteParametersUnit.pas',
  EnumsUnit in '..\DataTypes\EnumsUnit.pas',
  IRoute4MeManagerUnit in '..\IRoute4MeManagerUnit.pas',
  NullableBasicTypesUnit in '..\Nullabled\NullableBasicTypesUnit.pas',
  JSONNullableAttributeUnit in '..\Nullabled\JSONNullableAttributeUnit.pas',
  NullableInterceptorUnit in '..\Nullabled\NullableInterceptorUnit.pas',
  JSONNullableConverterUnit in '..\Nullabled\JSONNullableConverterUnit.pas',
  JSONDictionaryIntermediateObjectUnit in '..\Nullabled\JSONDictionaryIntermediateObjectUnit.pas',
  AddressBookContactUnit in '..\DataTypes\AddressBookContactUnit.pas',
  BaseRoute4MeTestUnit in 'Route4MeManager\BaseRoute4MeTestUnit.pas',
  AddressBookContactActionsUnit in '..\Actions\AddressBookContactActionsUnit.pas',
  AddressBookParametersUnit in '..\QueryTypes\AddressBookParametersUnit.pas',
  TestAddressBookContactActionsUnit in 'Actions\TestAddressBookContactActionsUnit.pas',
  TestMultipleDepotMultipleDriverUnit in 'Actions\Optimization\TestMultipleDepotMultipleDriverUnit.pas',
  TestSingleDriverRoundTripUnit in 'Actions\Optimization\TestSingleDriverRoundTripUnit.pas',
  TestSingleDriverRoute10StopsUnit in 'Actions\Optimization\TestSingleDriverRoute10StopsUnit.pas',
  TestRoute4MeManagerUnit in 'Actions\Optimization\TestRoute4MeManagerUnit.pas',
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
  RouteActionUnit in '..\Actions\RouteActionUnit.pas',
  AddressesOrderInfoUnit in '..\AdditionalDataTypes\AddressesOrderInfoUnit.pas',
  TestUnmarshalDataObjectRouteUnit in 'Json\Unmarshal\TestUnmarshalDataObjectRouteUnit.pas',
  IDataObjectRouteProviderUnit in 'TestDataProviders\DataObjectRoute\IDataObjectRouteProviderUnit.pas',
  SampleDataObjectRouteProviderUnit in 'TestDataProviders\DataObjectRoute\SampleDataObjectRouteProviderUnit.pas',
  BaseDataObjectRouteProviderUnit in 'TestDataProviders\DataObjectRoute\BaseDataObjectRouteProviderUnit.pas',
  DataObjectUnit in '..\DataTypes\DataObjectUnit.pas',
  CommonTypesUnit in '..\Common\CommonTypesUnit.pas',
  MoveDestinationToRouteResponseUnit in '..\AdditionalDataTypes\MoveDestinationToRouteResponseUnit.pas',
  RemoveRouteDestinationRequestUnit in '..\AdditionalDataTypes\RemoveRouteDestinationRequestUnit.pas',
  RemoveRouteDestinationResponseUnit in '..\AdditionalDataTypes\RemoveRouteDestinationResponseUnit.pas',
  AddRouteDestinationRequestUnit in '..\AdditionalDataTypes\AddRouteDestinationRequestUnit.pas',
  RouteParametersQueryUnit in '..\QueryTypes\RouteParametersQueryUnit.pas',
  DataObjectOptimizationsResponseUnit in '..\AdditionalDataTypes\DataObjectOptimizationsResponseUnit.pas',
  RemoveDestinationFromOptimizationResponseUnit in '..\AdditionalDataTypes\RemoveDestinationFromOptimizationResponseUnit.pas',
  TestRoute4MeExamplesUnit in 'Examples\TestRoute4MeExamplesUnit.pas',
  Route4MeExamplesUnit in '..\Examples\Route4MeExamplesUnit.pas',
  SingleDriverRoundTripGenericRequestUnit in '..\AdditionalDataTypes\SingleDriverRoundTripGenericRequestUnit.pas',
  SingleDriverRoundTripGenericResponseUnit in '..\AdditionalDataTypes\SingleDriverRoundTripGenericResponseUnit.pas',
  BaseOptimizationParametersProviderUnit in '..\TestDataProviders\OptimizationParameters\BaseOptimizationParametersProviderUnit.pas',
  IOptimizationParametersProviderUnit in '..\TestDataProviders\OptimizationParameters\IOptimizationParametersProviderUnit.pas',
  MultipleDepotMultipleDriverTestDataProviderUnit in '..\TestDataProviders\OptimizationParameters\MultipleDepotMultipleDriverTestDataProviderUnit.pas',
  MultipleDepotMultipleDriverTimeWindowTestDataProviderUnit in '..\TestDataProviders\OptimizationParameters\MultipleDepotMultipleDriverTimeWindowTestDataProviderUnit.pas',
  MultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProviderUnit in '..\TestDataProviders\OptimizationParameters\MultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProviderUnit.pas',
  SingleDepotMultipleDriverNoTimeWindowTestDataProviderUnit in '..\TestDataProviders\OptimizationParameters\SingleDepotMultipleDriverNoTimeWindowTestDataProviderUnit.pas',
  SingleDriverMultipleTimeWindowsTestDataProviderUnit in '..\TestDataProviders\OptimizationParameters\SingleDriverMultipleTimeWindowsTestDataProviderUnit.pas',
  SingleDriverRoundTripGenericTestDataProviderUnit in '..\TestDataProviders\OptimizationParameters\SingleDriverRoundTripGenericTestDataProviderUnit.pas',
  SingleDriverRoundTripTestDataProviderUnit in '..\TestDataProviders\OptimizationParameters\SingleDriverRoundTripTestDataProviderUnit.pas',
  SingleDriverRoute10StopsTestDataProviderUnit in '..\TestDataProviders\OptimizationParameters\SingleDriverRoute10StopsTestDataProviderUnit.pas',
  DefaultAddressBookContactProviderUnit in '..\TestDataProviders\AddressBookContact\DefaultAddressBookContactProviderUnit.pas',
  FullAddressBookContactProviderUnit in '..\TestDataProviders\AddressBookContact\FullAddressBookContactProviderUnit.pas',
  IAddressBookContactProviderUnit in '..\TestDataProviders\AddressBookContact\IAddressBookContactProviderUnit.pas',
  OutputUnit in '..\Common\OutputUnit.pas',
  ConnectionStubUnit in 'Connection\ConnectionStubUnit.pas',
  UserActionUnit in '..\Actions\UserActionUnit.pas',
  UserUnit in '..\DataTypes\UserUnit.pas',
  ActivityUnit in '..\DataTypes\ActivityUnit.pas',
  OrderUnit in '..\DataTypes\OrderUnit.pas',
  NoteParametersUnit in '..\QueryTypes\NoteParametersUnit.pas',
  AddressNoteUnit in '..\DataTypes\AddressNoteUnit.pas',
  AddressNoteActionUnit in '..\Actions\AddressNoteActionUnit.pas',
  AddressParametersUnit in '..\QueryTypes\AddressParametersUnit.pas',
  AddAddressNoteResponseUnit in '..\AdditionalDataTypes\AddAddressNoteResponseUnit.pas',
  AddressActionUnit in '..\Actions\AddressActionUnit.pas',
  AvoidanceZoneParametersUnit in '..\QueryTypes\AvoidanceZoneParametersUnit.pas',
  TerritoryUnit in '..\DataTypes\TerritoryUnit.pas',
  AvoidanceZoneUnit in '..\DataTypes\AvoidanceZoneUnit.pas',
  AvoidanceZoneActionUnit in '..\Actions\AvoidanceZoneActionUnit.pas',
  AvoidanceZoneQueryUnit in '..\QueryTypes\AvoidanceZoneQueryUnit.pas',
  OrderActionUnit in '..\Actions\OrderActionUnit.pas',
  GetOrdersResponseUnit in '..\AdditionalDataTypes\GetOrdersResponseUnit.pas',
  OrderParametersUnit in '..\QueryTypes\OrderParametersUnit.pas',
  DeleteRouteResponseUnit in '..\AdditionalDataTypes\DeleteRouteResponseUnit.pas',
  DuplicateRouteResponseUnit in '..\AdditionalDataTypes\DuplicateRouteResponseUnit.pas',
  ActivityParametersUnit in '..\QueryTypes\ActivityParametersUnit.pas',
  ActivityActionsUnit in '..\Actions\ActivityActionsUnit.pas',
  GetActivitiesResponseUnit in '..\AdditionalDataTypes\GetActivitiesResponseUnit.pas',
  LogCustomActivityResponseUnit in '..\AdditionalDataTypes\LogCustomActivityResponseUnit.pas',
  RemoveAddressBookContactsRequestUnit in '..\AdditionalDataTypes\RemoveAddressBookContactsRequestUnit.pas',
  RemoveAddressBookContactsResponseUnit in '..\AdditionalDataTypes\RemoveAddressBookContactsResponseUnit.pas',
  GetAddressBookContactsResponseUnit in '..\AdditionalDataTypes\GetAddressBookContactsResponseUnit.pas',
  RemoveOptimizationResponseUnit in '..\AdditionalDataTypes\RemoveOptimizationResponseUnit.pas',
  RemoveOrdersRequestUnit in '..\AdditionalDataTypes\RemoveOrdersRequestUnit.pas',
  RemoveOrdersResponseUnit in '..\AdditionalDataTypes\RemoveOrdersResponseUnit.pas',
  GPSParametersUnit in '..\QueryTypes\GPSParametersUnit.pas',
  TrackingActionsUnit in '..\Actions\TrackingActionsUnit.pas',
  SingleDriverRoute10StopsResponseDataProviderUnit in 'TestDataProviders\DataObject\SingleDriverRoute10StopsResponseDataProviderUnit.pas',
  BaseDataObjectProviderUnit in 'TestDataProviders\DataObject\BaseDataObjectProviderUnit.pas',
  IDataObjectProviderUnit in 'TestDataProviders\DataObject\IDataObjectProviderUnit.pas',
  TestUnmarshalDataObjectUnit in 'Json\Unmarshal\TestUnmarshalDataObjectUnit.pas',
  GeocodingUnit in '..\DataTypes\GeocodingUnit.pas',
  ManifestUnit in '..\DataTypes\ManifestUnit.pas',
  NullableArrayInterceptorUnit in '..\Nullabled\NullableArrayInterceptorUnit.pas';

{R *.RES}

begin
  GUITestRunner.RunRegisteredTests;
end.

// RSP-16172
