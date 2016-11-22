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
  AddressBookContactUnit in '..\DataTypes\AddressBookContactUnit.pas',
  AddressBookContactActionsUnit in '..\Actions\AddressBookContactActionsUnit.pas',
  AddressBookParametersUnit in '..\QueryTypes\AddressBookParametersUnit.pas',
  SettingsUnit in '..\Common\SettingsUnit.pas',
  UtilsUnit in '..\Common\UtilsUnit.pas',
  IConnectionUnit in '..\Connection\IConnectionUnit.pas',
  BaseActionUnit in '..\Actions\BaseActionUnit.pas',
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
  AddressesOrderInfoUnit in '..\QueryTypes\AddressesOrderInfoUnit.pas',
  DataObjectUnit in '..\DataTypes\DataObjectUnit.pas',
  CommonTypesUnit in '..\Common\CommonTypesUnit.pas',
  MoveDestinationToRouteResponseUnit in '..\QueryTypes\MoveDestinationToRouteResponseUnit.pas',
  RemoveRouteDestinationRequestUnit in '..\QueryTypes\RemoveRouteDestinationRequestUnit.pas',
  RemoveRouteDestinationResponseUnit in '..\QueryTypes\RemoveRouteDestinationResponseUnit.pas',
  AddRouteDestinationRequestUnit in '..\QueryTypes\AddRouteDestinationRequestUnit.pas',
  RouteParametersQueryUnit in '..\QueryTypes\RouteParametersQueryUnit.pas',
  DataObjectOptimizationsResponseUnit in '..\QueryTypes\DataObjectOptimizationsResponseUnit.pas',
  RemoveDestinationFromOptimizationResponseUnit in '..\QueryTypes\RemoveDestinationFromOptimizationResponseUnit.pas',
  TestRoute4MeExamplesUnit in 'Examples\TestRoute4MeExamplesUnit.pas',
  SingleDriverRoundTripGenericRequestUnit in '..\QueryTypes\SingleDriverRoundTripGenericRequestUnit.pas',
  SingleDriverRoundTripGenericResponseUnit in '..\QueryTypes\SingleDriverRoundTripGenericResponseUnit.pas',
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
  AddAddressNoteResponseUnit in '..\QueryTypes\AddAddressNoteResponseUnit.pas',
  AddressActionUnit in '..\Actions\AddressActionUnit.pas',
  AvoidanceZoneParametersUnit in '..\QueryTypes\AvoidanceZoneParametersUnit.pas',
  TerritoryUnit in '..\DataTypes\TerritoryUnit.pas',
  AvoidanceZoneUnit in '..\DataTypes\AvoidanceZoneUnit.pas',
  AvoidanceZoneActionUnit in '..\Actions\AvoidanceZoneActionUnit.pas',
  AvoidanceZoneQueryUnit in '..\QueryTypes\AvoidanceZoneQueryUnit.pas',
  OrderActionUnit in '..\Actions\OrderActionUnit.pas',
  GetOrdersResponseUnit in '..\QueryTypes\GetOrdersResponseUnit.pas',
  OrderParametersUnit in '..\QueryTypes\OrderParametersUnit.pas',
  DeleteRouteResponseUnit in '..\QueryTypes\DeleteRouteResponseUnit.pas',
  DuplicateRouteResponseUnit in '..\QueryTypes\DuplicateRouteResponseUnit.pas',
  ActivityActionsUnit in '..\Actions\ActivityActionsUnit.pas',
  GetActivitiesResponseUnit in '..\QueryTypes\GetActivitiesResponseUnit.pas',
  LogCustomActivityResponseUnit in '..\QueryTypes\LogCustomActivityResponseUnit.pas',
  RemoveAddressBookContactsRequestUnit in '..\QueryTypes\RemoveAddressBookContactsRequestUnit.pas',
  RemoveAddressBookContactsResponseUnit in '..\QueryTypes\RemoveAddressBookContactsResponseUnit.pas',
  GetAddressBookContactsResponseUnit in '..\QueryTypes\GetAddressBookContactsResponseUnit.pas',
  RemoveOptimizationResponseUnit in '..\QueryTypes\RemoveOptimizationResponseUnit.pas',
  RemoveOrdersRequestUnit in '..\QueryTypes\RemoveOrdersRequestUnit.pas',
  RemoveOrdersResponseUnit in '..\QueryTypes\RemoveOrdersResponseUnit.pas',
  GPSParametersUnit in '..\QueryTypes\GPSParametersUnit.pas',
  TrackingActionsUnit in '..\Actions\TrackingActionsUnit.pas',
  GeocodingUnit in '..\DataTypes\GeocodingUnit.pas',
  ManifestUnit in '..\DataTypes\ManifestUnit.pas',
  ErrorResponseUnit in '..\DataTypes\ErrorResponseUnit.pas',
  TestUnmarshalAvoidanceZoneUnit in 'TestDataProviders\AvoidanceZone\TestUnmarshalAvoidanceZoneUnit.pas',
  IAvoidanceZoneProviderUnit in 'TestDataProviders\AvoidanceZone\IAvoidanceZoneProviderUnit.pas',
  RealAvoidanceZoneProviderUnit in 'TestDataProviders\AvoidanceZone\RealAvoidanceZoneProviderUnit.pas',
  BaseOptimizationParametersProviderUnit in '..\Examples\TestDataProviders\OptimizationParameters\BaseOptimizationParametersProviderUnit.pas',
  IOptimizationParametersProviderUnit in '..\Examples\TestDataProviders\OptimizationParameters\IOptimizationParametersProviderUnit.pas',
  MultipleDepotMultipleDriverTestDataProviderUnit in '..\Examples\TestDataProviders\OptimizationParameters\MultipleDepotMultipleDriverTestDataProviderUnit.pas',
  MultipleDepotMultipleDriverTimeWindowTestDataProviderUnit in '..\Examples\TestDataProviders\OptimizationParameters\MultipleDepotMultipleDriverTimeWindowTestDataProviderUnit.pas',
  MultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProviderUnit in '..\Examples\TestDataProviders\OptimizationParameters\MultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProviderUnit.pas',
  SingleDepotMultipleDriverNoTimeWindowTestDataProviderUnit in '..\Examples\TestDataProviders\OptimizationParameters\SingleDepotMultipleDriverNoTimeWindowTestDataProviderUnit.pas',
  SingleDriverMultipleTimeWindowsTestDataProviderUnit in '..\Examples\TestDataProviders\OptimizationParameters\SingleDriverMultipleTimeWindowsTestDataProviderUnit.pas',
  SingleDriverRoundTripGenericTestDataProviderUnit in '..\Examples\TestDataProviders\OptimizationParameters\SingleDriverRoundTripGenericTestDataProviderUnit.pas',
  SingleDriverRoundTripTestDataProviderUnit in '..\Examples\TestDataProviders\OptimizationParameters\SingleDriverRoundTripTestDataProviderUnit.pas',
  SingleDriverRoute10StopsTestDataProviderUnit in '..\Examples\TestDataProviders\OptimizationParameters\SingleDriverRoute10StopsTestDataProviderUnit.pas',
  AddressBookContactListProviderUnit in 'TestDataProviders\AddressBookContact\AddressBookContactListProviderUnit.pas',
  DefaultAddressBookContactProviderUnit in 'TestDataProviders\AddressBookContact\DefaultAddressBookContactProviderUnit.pas',
  FullAddressBookContactProviderUnit in 'TestDataProviders\AddressBookContact\FullAddressBookContactProviderUnit.pas',
  IAddressBookContactProviderUnit in 'TestDataProviders\AddressBookContact\IAddressBookContactProviderUnit.pas',
  RealAddressBookContactProviderUnit in 'TestDataProviders\AddressBookContact\RealAddressBookContactProviderUnit.pas',
  ConnectionUnit in '..\Connection\ConnectionUnit.pas',
  NullableBasicTypesUnit in '..\Common\NullableBasicTypesUnit.pas',
  JSONDictionaryIntermediateObjectUnit in '..\Json\JSONDictionaryIntermediateObjectUnit.pas',
  JSONNullableAttributeUnit in '..\Json\JSONNullableAttributeUnit.pas',
  JSONNullableMarshalConverterUnit in '..\Json\JSONNullableMarshalConverterUnit.pas',
  MarshalUnMarshalUnit in '..\Json\MarshalUnMarshalUnit.pas',
  NullableArrayInterceptorUnit in '..\Json\NullableArrayInterceptorUnit.pas',
  NullableInterceptorUnit in '..\Json\NullableInterceptorUnit.pas',
  ActivityParametersUnit in '..\QueryTypes\ActivityParametersUnit.pas',
  StatusResponseUnit in '..\QueryTypes\StatusResponseUnit.pas',
  BaseExampleUnit in '..\Examples\Examples\BaseExampleUnit.pas',
  BaseOptimizationExampleUnit in '..\Examples\Examples\BaseOptimizationExampleUnit.pas',
  MultipleDepotMultipleDriverTimeWindowUnit in '..\Examples\Examples\Optimization\MultipleDepotMultipleDriverTimeWindowUnit.pas',
  MultipleDepotMultipleDriverUnit in '..\Examples\Examples\Optimization\MultipleDepotMultipleDriverUnit.pas',
  MultipleDepotMultipleDriverWith24StopsTimeWindowUnit in '..\Examples\Examples\Optimization\MultipleDepotMultipleDriverWith24StopsTimeWindowUnit.pas',
  SingleDepotMultipleDriverNoTimeWindowUnit in '..\Examples\Examples\Optimization\SingleDepotMultipleDriverNoTimeWindowUnit.pas',
  SingleDriverMultipleTimeWindowsUnit in '..\Examples\Examples\Optimization\SingleDriverMultipleTimeWindowsUnit.pas',
  SingleDriverRoundTripGenericUnit in '..\Examples\Examples\Optimization\SingleDriverRoundTripGenericUnit.pas',
  SingleDriverRoundTripUnit in '..\Examples\Examples\Optimization\SingleDriverRoundTripUnit.pas',
  SingleDriverRoute10StopsUnit in '..\Examples\Examples\Optimization\SingleDriverRoute10StopsUnit.pas',
  Route4MeExamplesUnit in '..\Examples\Examples\Route4MeExamplesUnit.pas',
  GetActivitiesUnit in '..\Examples\Examples\Activities\GetActivitiesUnit.pas',
  LogCustomActivityUnit in '..\Examples\Examples\ActivityFeed\LogCustomActivityUnit.pas',
  AddAddressBookContactUnit in '..\Examples\Examples\AddressBookContact\AddAddressBookContactUnit.pas',
  GetAddressBookContactsUnit in '..\Examples\Examples\AddressBookContact\GetAddressBookContactsUnit.pas',
  RemoveAddressBookContactsUnit in '..\Examples\Examples\AddressBookContact\RemoveAddressBookContactsUnit.pas',
  UpdateAddressBookContactUnit in '..\Examples\Examples\AddressBookContact\UpdateAddressBookContactUnit.pas',
  AddDestinationToOptimizationUnit in '..\Examples\Examples\Addresses\AddDestinationToOptimizationUnit.pas',
  AddRouteDestinationsOptimallyUnit in '..\Examples\Examples\Addresses\AddRouteDestinationsOptimallyUnit.pas',
  AddRouteDestinationsUnit in '..\Examples\Examples\Addresses\AddRouteDestinationsUnit.pas',
  GetAddressUnit in '..\Examples\Examples\Addresses\GetAddressUnit.pas',
  MoveDestinationToRouteUnit in '..\Examples\Examples\Addresses\MoveDestinationToRouteUnit.pas',
  RemoveRouteDestinationUnit in '..\Examples\Examples\Addresses\RemoveRouteDestinationUnit.pas',
  AddAvoidanceZoneUnit in '..\Examples\Examples\AvoidanceZones\AddAvoidanceZoneUnit.pas',
  DeleteAvoidanceZoneUnit in '..\Examples\Examples\AvoidanceZones\DeleteAvoidanceZoneUnit.pas',
  GetAvoidanceZonesUnit in '..\Examples\Examples\AvoidanceZones\GetAvoidanceZonesUnit.pas',
  GetAvoidanceZoneUnit in '..\Examples\Examples\AvoidanceZones\GetAvoidanceZoneUnit.pas',
  UpdateAvoidanceZoneUnit in '..\Examples\Examples\AvoidanceZones\UpdateAvoidanceZoneUnit.pas',
  GenericExampleShortcutUnit in '..\Examples\Examples\Generic\GenericExampleShortcutUnit.pas',
  GenericExampleUnit in '..\Examples\Examples\Generic\GenericExampleUnit.pas',
  GetUsersUnit in '..\Examples\Examples\Members\GetUsersUnit.pas',
  AddAddressNoteUnit in '..\Examples\Examples\Notes\AddAddressNoteUnit.pas',
  GetAddressNotesUnit in '..\Examples\Examples\Notes\GetAddressNotesUnit.pas',
  GetOptimizationsUnit in '..\Examples\Examples\Optimization\GetOptimizationsUnit.pas',
  GetOptimizationUnit in '..\Examples\Examples\Optimization\GetOptimizationUnit.pas',
  RemoveDestinationFromOptimizationUnit in '..\Examples\Examples\Optimization\RemoveDestinationFromOptimizationUnit.pas',
  RemoveOptimizationUnit in '..\Examples\Examples\Optimization\RemoveOptimizationUnit.pas',
  ReOptimizationUnit in '..\Examples\Examples\Optimization\ReOptimizationUnit.pas',
  AddOrderUnit in '..\Examples\Examples\Orders\AddOrderUnit.pas',
  GetOrdersUnit in '..\Examples\Examples\Orders\GetOrdersUnit.pas',
  RemoveOrdersUnit in '..\Examples\Examples\Orders\RemoveOrdersUnit.pas',
  UpdateOrderUnit in '..\Examples\Examples\Orders\UpdateOrderUnit.pas',
  SetGPSPositionUnit in '..\Examples\Examples\Tracking\SetGPSPositionUnit.pas',
  TrackDeviceLastLocationHistoryUnit in '..\Examples\Examples\Tracking\TrackDeviceLastLocationHistoryUnit.pas',
  DeleteRoutesUnit in '..\Examples\Examples\Routes\DeleteRoutesUnit.pas',
  DuplicateRouteUnit in '..\Examples\Examples\Routes\DuplicateRouteUnit.pas',
  GetRoutesUnit in '..\Examples\Examples\Routes\GetRoutesUnit.pas',
  GetRouteUnit in '..\Examples\Examples\Routes\GetRouteUnit.pas',
  ReoptimizeRouteUnit in '..\Examples\Examples\Routes\ReoptimizeRouteUnit.pas',
  ResequenceRouteDestinationsUnit in '..\Examples\Examples\Routes\ResequenceRouteDestinationsUnit.pas',
  ShareRouteUnit in '..\Examples\Examples\Routes\ShareRouteUnit.pas',
  UpdateRouteUnit in '..\Examples\Examples\Routes\UpdateRouteUnit.pas',
  MergeRouteRequestUnit in '..\QueryTypes\MergeRouteRequestUnit.pas',
  MergeRoutesUnit in '..\Examples\Examples\Routes\MergeRoutesUnit.pas',
  UpdateRoutesCustomDataRequestUnit in '..\QueryTypes\UpdateRoutesCustomDataRequestUnit.pas',
  UpdateRoutesCustomFieldsUnit in '..\Examples\Examples\Routes\UpdateRoutesCustomFieldsUnit.pas',
  ResequenceAllRouteDestinationsUnit in '..\Examples\Examples\Routes\ResequenceAllRouteDestinationsUnit.pas',
  ResequenceAllRoutesRequestUnit in '..\QueryTypes\ResequenceAllRoutesRequestUnit.pas',
  AddOrderToRouteRequestUnit in '..\QueryTypes\AddOrderToRouteRequestUnit.pas',
  AddOrderToRouteUnit in '..\Examples\Examples\Orders\AddOrderToRouteUnit.pas',
  AddOrderToRouteParameterProviderUnit in '..\Examples\TestDataProviders\AddOrderToRouteParameterProviderUnit.pas',
  AddOrderToOptimizationRequestUnit in '..\QueryTypes\AddOrderToOptimizationRequestUnit.pas',
  AddOrderToOptimizationUnit in '..\Examples\Examples\Orders\AddOrderToOptimizationUnit.pas',
  GetOrderUnit in '..\Examples\Examples\Orders\GetOrderUnit.pas',
  GetOrdersByDateUnit in '..\Examples\Examples\Orders\GetOrdersByDateUnit.pas',
  GetOrdersScheduledForUnit in '..\Examples\Examples\Orders\GetOrdersScheduledForUnit.pas',
  GetOrdersWithCustomFieldsResponseUnit in '..\QueryTypes\GetOrdersWithCustomFieldsResponseUnit.pas',
  GetOrdersWithCustomFieldsUnit in '..\Examples\Examples\Orders\GetOrdersWithCustomFieldsUnit.pas',
  TestUnmarshalMultipleArrayUnit in 'Json\Unmarshal\TestUnmarshalMultipleArrayUnit.pas',
  GetOrdersWithSpecifiedTextUnit in '..\Examples\Examples\Orders\GetOrdersWithSpecifiedTextUnit.pas',
  MarkAddressAsVisitedRequestUnit in '..\QueryTypes\MarkAddressAsVisitedRequestUnit.pas',
  MarkAddressAsDepartedRequestUnit in '..\QueryTypes\MarkAddressAsDepartedRequestUnit.pas',
  MarkAddressAsDepartedUnit in '..\Examples\Examples\Addresses\MarkAddressAsDepartedUnit.pas',
  MarkAddressAsVisitedUnit in '..\Examples\Examples\Addresses\MarkAddressAsVisitedUnit.pas';

{R *.RES}

begin
  GUITestRunner.RunRegisteredTests;
end.

// RSP-16172
