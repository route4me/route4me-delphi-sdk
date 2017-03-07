program Route4MeProjectTests;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

//  FastMM4 in '..\FastMM\FastMM4.pas',
//  FastMM4Messages in '..\FastMM\FastMM4Messages.pas',

uses
  FastMM4Messages in '..\FastMM\FastMM4Messages.pas',
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
  OptimizationActionsUnit in '..\Actions\OptimizationActionsUnit.pas',
  RouteActionsUnit in '..\Actions\RouteActionsUnit.pas',
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
  TestExamplesRequestsUnit in 'Examples\TestExamplesRequestsUnit.pas',
  SingleDriverRoundTripGenericRequestUnit in '..\QueryTypes\SingleDriverRoundTripGenericRequestUnit.pas',
  SingleDriverRoundTripGenericResponseUnit in '..\QueryTypes\SingleDriverRoundTripGenericResponseUnit.pas',
  OutputUnit in '..\Common\OutputUnit.pas',
  ConnectionStubUnit in 'Connection\ConnectionStubUnit.pas',
  UserActionsUnit in '..\Actions\UserActionsUnit.pas',
  UserUnit in '..\DataTypes\UserUnit.pas',
  OrderUnit in '..\DataTypes\OrderUnit.pas',
  NoteParametersUnit in '..\QueryTypes\NoteParametersUnit.pas',
  AddressNoteUnit in '..\DataTypes\AddressNoteUnit.pas',
  AddressNoteActionsUnit in '..\Actions\AddressNoteActionsUnit.pas',
  AddressParametersUnit in '..\QueryTypes\AddressParametersUnit.pas',
  AddAddressNoteResponseUnit in '..\QueryTypes\AddAddressNoteResponseUnit.pas',
  TerritoryContourUnit in '..\DataTypes\TerritoryContourUnit.pas',
  AvoidanceZoneUnit in '..\DataTypes\AvoidanceZoneUnit.pas',
  AvoidanceZoneActionsUnit in '..\Actions\AvoidanceZoneActionsUnit.pas',
  AvoidanceZoneQueryUnit in '..\QueryTypes\AvoidanceZoneQueryUnit.pas',
  OrderActionsUnit in '..\Actions\OrderActionsUnit.pas',
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
  AddressGeocodingUnit in '..\DataTypes\AddressGeocodingUnit.pas',
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
  GetAllActivitiesUnit in '..\Examples\Examples\Activities\GetAllActivitiesUnit.pas',
  CreateLocationUnit in '..\Examples\Examples\AddressBookContact\CreateLocationUnit.pas',
  GetLocationsUnit in '..\Examples\Examples\AddressBookContact\GetLocationsUnit.pas',
  RemoveLocationsUnit in '..\Examples\Examples\AddressBookContact\RemoveLocationsUnit.pas',
  UpdateLocationUnit in '..\Examples\Examples\AddressBookContact\UpdateLocationUnit.pas',
  AddDestinationToOptimizationUnit in '..\Examples\Examples\Addresses\AddDestinationToOptimizationUnit.pas',
  AddRouteDestinationsOptimallyUnit in '..\Examples\Examples\Addresses\AddRouteDestinationsOptimallyUnit.pas',
  AddRouteDestinationsUnit in '..\Examples\Examples\Addresses\AddRouteDestinationsUnit.pas',
  GetAddressUnit in '..\Examples\Examples\Addresses\GetAddressUnit.pas',
  MoveDestinationToRouteUnit in '..\Examples\Examples\Addresses\MoveDestinationToRouteUnit.pas',
  RemoveRouteDestinationUnit in '..\Examples\Examples\Addresses\RemoveRouteDestinationUnit.pas',
  AddCircleAvoidanceZoneUnit in '..\Examples\Examples\AvoidanceZones\AddCircleAvoidanceZoneUnit.pas',
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
  MarkAddressAsDepartedUnit in '..\Examples\Examples\Addresses\MarkAddressAsDepartedUnit.pas',
  MarkAddressAsVisitedUnit in '..\Examples\Examples\Addresses\MarkAddressAsVisitedUnit.pas',
  MarkAddressAsDetectedAsDepartedRequestUnit in '..\QueryTypes\MarkAddressAsDetectedAsDepartedRequestUnit.pas',
  MarkAddressAsDetectedAsVisitedRequestUnit in '..\QueryTypes\MarkAddressAsDetectedAsVisitedRequestUnit.pas',
  MarkAddressAsDetectedAsDepartedUnit in '..\Examples\Examples\Addresses\MarkAddressAsDetectedAsDepartedUnit.pas',
  MarkAddressAsDetectedAsVisitedUnit in '..\Examples\Examples\Addresses\MarkAddressAsDetectedAsVisitedUnit.pas',
  GeocodingActionsUnit in '..\Actions\GeocodingActionsUnit.pas',
  BatchForwardGeocodeAddressUnit in '..\Examples\Examples\Geocoding\BatchForwardGeocodeAddressUnit.pas',
  ValidateSessionResponseUnit in '..\QueryTypes\ValidateSessionResponseUnit.pas',
  ValidateSessionUnit in '..\Examples\Examples\Members\ValidateSessionUnit.pas',
  AddNewUserResponseUnit in '..\QueryTypes\AddNewUserResponseUnit.pas',
  RegisterAccountUnit in '..\Examples\Examples\Members\RegisterAccountUnit.pas',
  GetUserDetailsUnit in '..\Examples\Examples\Members\GetUserDetailsUnit.pas',
  AddNewUserUnit in '..\Examples\Examples\Members\AddNewUserUnit.pas',
  UserParametersUnit in '..\QueryTypes\UserParametersUnit.pas',
  RegisterAccountResponseUnit in '..\QueryTypes\RegisterAccountResponseUnit.pas',
  UserParameterProviderUnit in '..\Examples\TestDataProviders\UserParameterProviderUnit.pas',
  RemoveUserRequestUnit in '..\QueryTypes\RemoveUserRequestUnit.pas',
  RemoveUserUnit in '..\Examples\Examples\Members\RemoveUserUnit.pas',
  AuthenticationResponseUnit in '..\QueryTypes\AuthenticationResponseUnit.pas',
  AuthenticationUnit in '..\Examples\Examples\Members\AuthenticationUnit.pas',
  DeviceLicenseRequestUnit in '..\QueryTypes\DeviceLicenseRequestUnit.pas',
  DeviceLicenseUnit in '..\Examples\Examples\Members\DeviceLicenseUnit.pas',
  BaseTestOnlineExamplesUnit in 'Examples\Online\BaseTestOnlineExamplesUnit.pas',
  TestMemberSamplesUnit in 'Examples\Online\TestMemberSamplesUnit.pas',
  UserLicenseRequestUnit in '..\QueryTypes\UserLicenseRequestUnit.pas',
  UserLicenseUnit in '..\Examples\Examples\Members\UserLicenseUnit.pas',
  RegisterWebinarRequestUnit in '..\QueryTypes\RegisterWebinarRequestUnit.pas',
  RegisterWebinarUnit in '..\Examples\Examples\Members\RegisterWebinarUnit.pas',
  TestOrderSamplesUnit in 'Examples\Online\TestOrderSamplesUnit.pas',
  LogSpecificMessageUnit in '..\Examples\Examples\Activities\LogSpecificMessageUnit.pas',
  GetTeamActivitiesUnit in '..\Examples\Examples\Activities\GetTeamActivitiesUnit.pas',
  TestActivitiesSamplesUnit in 'Examples\Online\TestActivitiesSamplesUnit.pas',
  ActivityUnit in '..\DataTypes\ActivityUnit.pas',
  ActivityRequestUnit in '..\QueryTypes\ActivityRequestUnit.pas',
  GetAreaUpdatedActivitiesUnit in '..\Examples\Examples\Activities\GetAreaUpdatedActivitiesUnit.pas',
  GetAreaRemovedActivitiesUnit in '..\Examples\Examples\Activities\GetAreaRemovedActivitiesUnit.pas',
  TestAddressesSamplesUnit in 'Examples\Online\TestAddressesSamplesUnit.pas',
  GetDestinationDeletedActivitiesUnit in '..\Examples\Examples\Activities\GetDestinationDeletedActivitiesUnit.pas',
  GetDestinationOutOfSequenceActivitiesUnit in '..\Examples\Examples\Activities\GetDestinationOutOfSequenceActivitiesUnit.pas',
  GetDriverArrivedEarlyActivitiesUnit in '..\Examples\Examples\Activities\GetDriverArrivedEarlyActivitiesUnit.pas',
  GetDriverArrivedLateActivitiesUnit in '..\Examples\Examples\Activities\GetDriverArrivedLateActivitiesUnit.pas',
  GetDriverArrivedOnTimeActivitiesUnit in '..\Examples\Examples\Activities\GetDriverArrivedOnTimeActivitiesUnit.pas',
  GetGeofenceEnteredActivitiesUnit in '..\Examples\Examples\Activities\GetGeofenceEnteredActivitiesUnit.pas',
  GetGeofenceLeftActivitiesUnit in '..\Examples\Examples\Activities\GetGeofenceLeftActivitiesUnit.pas',
  GetDestinationInsertedActivitiesUnit in '..\Examples\Examples\Activities\GetDestinationInsertedActivitiesUnit.pas',
  GetAllDestinationInsertedActivitiesUnit in '..\Examples\Examples\Activities\GetAllDestinationInsertedActivitiesUnit.pas',
  GetDestinationMarkedAsDepartedActivitiesUnit in '..\Examples\Examples\Activities\GetDestinationMarkedAsDepartedActivitiesUnit.pas',
  GetAllDestinationMarkedAsDepartedActivitiesUnit in '..\Examples\Examples\Activities\GetAllDestinationMarkedAsDepartedActivitiesUnit.pas',
  GetAllDestinationMarkedAsVisitedActivitiesUnit in '..\Examples\Examples\Activities\GetAllDestinationMarkedAsVisitedActivitiesUnit.pas',
  GetActivitiesQueryUnit in '..\QueryTypes\GetActivitiesQueryUnit.pas',
  GetAreaAddedActivitiesUnit in '..\Examples\Examples\Activities\GetAreaAddedActivitiesUnit.pas',
  GetMemberCreatedActivitiesUnit in '..\Examples\Examples\Activities\GetMemberCreatedActivitiesUnit.pas',
  GetMemberDeletedActivitiesUnit in '..\Examples\Examples\Activities\GetMemberDeletedActivitiesUnit.pas',
  GetMemberModifiedActivitiesUnit in '..\Examples\Examples\Activities\GetMemberModifiedActivitiesUnit.pas',
  GetDestinationMovedActivitiesUnit in '..\Examples\Examples\Activities\GetDestinationMovedActivitiesUnit.pas',
  GetNoteInsertedActivitiesUnit in '..\Examples\Examples\Activities\GetNoteInsertedActivitiesUnit.pas',
  GetAllNoteInsertedActivitiesUnit in '..\Examples\Examples\Activities\GetAllNoteInsertedActivitiesUnit.pas',
  GetRouteDeletedActivitiesUnit in '..\Examples\Examples\Activities\GetRouteDeletedActivitiesUnit.pas',
  GetRouteOptimizedActivitiesUnit in '..\Examples\Examples\Activities\GetRouteOptimizedActivitiesUnit.pas',
  GetRouteOwnerChangedActivitiesUnit in '..\Examples\Examples\Activities\GetRouteOwnerChangedActivitiesUnit.pas',
  GetDestinationUpdatedActivitiesUnit in '..\Examples\Examples\Activities\GetDestinationUpdatedActivitiesUnit.pas',
  GetLocationsByIdsUnit in '..\Examples\Examples\AddressBookContact\GetLocationsByIdsUnit.pas',
  GetLocationUnit in '..\Examples\Examples\AddressBookContact\GetLocationUnit.pas',
  DisplayRoutedUnit in '..\Examples\Examples\AddressBookContact\DisplayRoutedUnit.pas',
  LocationSearchUnit in '..\Examples\Examples\AddressBookContact\LocationSearchUnit.pas',
  TestAddressBookSamplesUnit in 'Examples\Online\TestAddressBookSamplesUnit.pas',
  AddressBookContactFindResponseUnit in '..\QueryTypes\AddressBookContactFindResponseUnit.pas',
  AddPolygonAvoidanceZoneUnit in '..\Examples\Examples\AvoidanceZones\AddPolygonAvoidanceZoneUnit.pas',
  AddRectangularAvoidanceZoneUnit in '..\Examples\Examples\AvoidanceZones\AddRectangularAvoidanceZoneUnit.pas',
  TestAvoidanceZoneSamplesUnit in 'Examples\Online\TestAvoidanceZoneSamplesUnit.pas',
  TerritoryActionsUnit in '..\Actions\TerritoryActionsUnit.pas',
  GetTerritoriesResponseUnit in '..\QueryTypes\GetTerritoriesResponseUnit.pas',
  TerritoryUnit in '..\DataTypes\TerritoryUnit.pas',
  AddCircleTerritoryUnit in '..\Examples\Examples\Territories\AddCircleTerritoryUnit.pas',
  RemoveTerritoryUnit in '..\Examples\Examples\Territories\RemoveTerritoryUnit.pas',
  AddressActionsUnit in '..\Actions\AddressActionsUnit.pas',
  TestTerritoriesSamplesUnit in 'Examples\Online\TestTerritoriesSamplesUnit.pas',
  AddPolygonTerritoryUnit in '..\Examples\Examples\Territories\AddPolygonTerritoryUnit.pas',
  AddRectangularTerritoryUnit in '..\Examples\Examples\Territories\AddRectangularTerritoryUnit.pas',
  UpdateTerritoryUnit in '..\Examples\Examples\Territories\UpdateTerritoryUnit.pas',
  GetTerritoryUnit in '..\Examples\Examples\Territories\GetTerritoryUnit.pas',
  GetTerritoriesUnit in '..\Examples\Examples\Territories\GetTerritoriesUnit.pas',
  TestAddressNotesSamplesUnit in 'Examples\Online\TestAddressNotesSamplesUnit.pas',
  UpdateTerritoryRequestUnit in '..\QueryTypes\UpdateTerritoryRequestUnit.pas',
  PositionUnit in '..\DataTypes\PositionUnit.pas',
  TestUnmarshalTerritoryListUnit in 'Json\Unmarshal\TestUnmarshalTerritoryListUnit.pas',
  ReverseGeocodeAddressUnit in '..\Examples\Examples\Geocoding\ReverseGeocodeAddressUnit.pas',
  GeocodingUnit in '..\DataTypes\GeocodingUnit.pas',
  GeocodingAddressUnit in '..\DataTypes\GeocodingAddressUnit.pas',
  TestGeocodingSamplesUnit in 'Examples\Online\TestGeocodingSamplesUnit.pas',
  GetSingleGeocodingAddressUnit in '..\Examples\Examples\Geocoding\GetSingleGeocodingAddressUnit.pas',
  GetGeocodingAddressesUnit in '..\Examples\Examples\Geocoding\GetGeocodingAddressesUnit.pas',
  GetLimitedGeocodingAddressesUnit in '..\Examples\Examples\Geocoding\GetLimitedGeocodingAddressesUnit.pas',
  GetZipCodesUnit in '..\Examples\Examples\Geocoding\GetZipCodesUnit.pas',
  GetLimitedZipCodesUnit in '..\Examples\Examples\Geocoding\GetLimitedZipCodesUnit.pas',
  GetZipCodeAndHouseNumberUnit in '..\Examples\Examples\Geocoding\GetZipCodeAndHouseNumberUnit.pas',
  GetLimitedZipCodeAndHouseNumberUnit in '..\Examples\Examples\Geocoding\GetLimitedZipCodeAndHouseNumberUnit.pas',
  BulkGeocodingRequestUnit in '..\QueryTypes\BulkGeocodingRequestUnit.pas',
  BulkForwardGeocodeAddressesUnit in '..\Examples\Examples\Geocoding\BulkForwardGeocodeAddressesUnit.pas',
  SearchRoutesForSpecifiedTextUnit in '..\Examples\Examples\Routes\SearchRoutesForSpecifiedTextUnit.pas',
  TestTrackingSamplesUnit in 'Examples\Online\TestTrackingSamplesUnit.pas',
  TrackingHistoryRequestUnit in '..\QueryTypes\TrackingHistoryRequestUnit.pas',
  TrackingHistoryResponseUnit in '..\QueryTypes\TrackingHistoryResponseUnit.pas',
  GetLocationHistoryFromTimeRangeUnit in '..\Examples\Examples\Tracking\GetLocationHistoryFromTimeRangeUnit.pas',
  TrackingDataUnit in '..\DataTypes\TrackingDataUnit.pas',
  GetAssetTrackingDataUnit in '..\Examples\Examples\Tracking\GetAssetTrackingDataUnit.pas',
  AddNoteFileResponseUnit in '..\QueryTypes\AddNoteFileResponseUnit.pas',
  TestRouteSamplesUnit in 'Examples\Online\TestRouteSamplesUnit.pas',
  DeviceLicenseResponseUnit in '..\QueryTypes\DeviceLicenseResponseUnit.pas',
  ConfigValueRequestUnit in '..\QueryTypes\ConfigValueRequestUnit.pas',
  ConfigValueResponseUnit in '..\QueryTypes\ConfigValueResponseUnit.pas',
  AddConfigValueUnit in '..\Examples\Examples\Members\AddConfigValueUnit.pas',
  UpdateConfigValueUnit in '..\Examples\Examples\Members\UpdateConfigValueUnit.pas',
  DeleteConfigValueUnit in '..\Examples\Examples\Members\DeleteConfigValueUnit.pas',
  UpdateUserUnit in '..\Examples\Examples\Members\UpdateUserUnit.pas',
  GetConfigValueResponseUnit in '..\QueryTypes\GetConfigValueResponseUnit.pas',
  GetConfigValueUnit in '..\Examples\Examples\Members\GetConfigValueUnit.pas',
  GetAllConfigValuesUnit in '..\Examples\Examples\Members\GetAllConfigValuesUnit.pas',
  VehicleActionsUnit in '..\Actions\VehicleActionsUnit.pas',
  VehicleUnit in '..\DataTypes\VehicleUnit.pas',
  GetVehiclesUnit in '..\Examples\Examples\Vehicles\GetVehiclesUnit.pas',
  GetVehicleUnit in '..\Examples\Examples\Vehicles\GetVehicleUnit.pas',
  TestVehicleSamplesUnit in 'Examples\Online\TestVehicleSamplesUnit.pas',
  FileUploadingActionsUnit in '..\Actions\FileUploadingActionsUnit.pas',
  PreviewFileUnit in '..\Examples\Examples\FileUploading\PreviewFileUnit.pas',
  TestFileUploadingSamplesUnit in 'Examples\Online\TestFileUploadingSamplesUnit.pas',
  UploadFileGeocodingUnit in '..\Examples\Examples\FileUploading\UploadFileGeocodingUnit.pas',
  FileUploadErrorsResponseUnit in '..\QueryTypes\FileUploadErrorsResponseUnit.pas';

{R *.RES}

begin
  GUITestRunner.RunRegisteredTests;
end.

// RSP-16172

// ������ �� UpdateRouteCustomFields ���� ������
