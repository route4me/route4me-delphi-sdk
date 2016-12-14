program Route4MeExamplesProject;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  SettingsUnit in '..\Common\SettingsUnit.pas',
  UtilsUnit in '..\Common\UtilsUnit.pas',
  AddressBookContactUnit in '..\DataTypes\AddressBookContactUnit.pas',
  AddressUnit in '..\DataTypes\AddressUnit.pas',
  DataObjectUnit in '..\DataTypes\DataObjectUnit.pas',
  EnumsUnit in '..\DataTypes\EnumsUnit.pas',
  RouteParametersUnit in '..\DataTypes\RouteParametersUnit.pas',
  Route4MeManagerUnit in '..\Route4MeManagerUnit.pas',
  AddressBookContactActionsUnit in '..\Actions\AddressBookContactActionsUnit.pas',
  BaseActionUnit in '..\Actions\BaseActionUnit.pas',
  OptimizationActionsUnit in '..\Actions\OptimizationActionsUnit.pas',
  DirectionUnit in '..\DataTypes\DirectionUnit.pas',
  DirectionLocationUnit in '..\DataTypes\DirectionLocationUnit.pas',
  DirectionStepUnit in '..\DataTypes\DirectionStepUnit.pas',
  DirectionPathPointUnit in '..\DataTypes\DirectionPathPointUnit.pas',
  LinksUnit in '..\DataTypes\LinksUnit.pas',
  TrackingHistoryUnit in '..\DataTypes\TrackingHistoryUnit.pas',
  RouteActionsUnit in '..\Actions\RouteActionsUnit.pas',
  CommonTypesUnit in '..\Common\CommonTypesUnit.pas',
  MainExamplesUnit in 'MainExamplesUnit.pas',
  IRoute4MeManagerUnit in '..\IRoute4MeManagerUnit.pas',
  OutputUnit in '..\Common\OutputUnit.pas',
  UserActionsUnit in '..\Actions\UserActionsUnit.pas',
  UserUnit in '..\DataTypes\UserUnit.pas',
  ActivityActionsUnit in '..\Actions\ActivityActionsUnit.pas',
  AddressActionsUnit in '..\Actions\AddressActionsUnit.pas',
  AddressNoteActionsUnit in '..\Actions\AddressNoteActionsUnit.pas',
  AvoidanceZoneActionsUnit in '..\Actions\AvoidanceZoneActionsUnit.pas',
  OrderActionsUnit in '..\Actions\OrderActionsUnit.pas',
  TrackingActionsUnit in '..\Actions\TrackingActionsUnit.pas',
  AddressNoteUnit in '..\DataTypes\AddressNoteUnit.pas',
  AvoidanceZoneUnit in '..\DataTypes\AvoidanceZoneUnit.pas',
  OrderUnit in '..\DataTypes\OrderUnit.pas',
  ManifestUnit in '..\DataTypes\ManifestUnit.pas',
  GeocodingUnit in '..\DataTypes\GeocodingUnit.pas',
  ErrorResponseUnit in '..\DataTypes\ErrorResponseUnit.pas',
  BaseOptimizationParametersProviderUnit in 'TestDataProviders\OptimizationParameters\BaseOptimizationParametersProviderUnit.pas',
  IOptimizationParametersProviderUnit in 'TestDataProviders\OptimizationParameters\IOptimizationParametersProviderUnit.pas',
  MultipleDepotMultipleDriverTestDataProviderUnit in 'TestDataProviders\OptimizationParameters\MultipleDepotMultipleDriverTestDataProviderUnit.pas',
  MultipleDepotMultipleDriverTimeWindowTestDataProviderUnit in 'TestDataProviders\OptimizationParameters\MultipleDepotMultipleDriverTimeWindowTestDataProviderUnit.pas',
  MultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProviderUnit in 'TestDataProviders\OptimizationParameters\MultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProviderUnit.pas',
  SingleDepotMultipleDriverNoTimeWindowTestDataProviderUnit in 'TestDataProviders\OptimizationParameters\SingleDepotMultipleDriverNoTimeWindowTestDataProviderUnit.pas',
  SingleDriverMultipleTimeWindowsTestDataProviderUnit in 'TestDataProviders\OptimizationParameters\SingleDriverMultipleTimeWindowsTestDataProviderUnit.pas',
  SingleDriverRoundTripGenericTestDataProviderUnit in 'TestDataProviders\OptimizationParameters\SingleDriverRoundTripGenericTestDataProviderUnit.pas',
  SingleDriverRoundTripTestDataProviderUnit in 'TestDataProviders\OptimizationParameters\SingleDriverRoundTripTestDataProviderUnit.pas',
  SingleDriverRoute10StopsTestDataProviderUnit in 'TestDataProviders\OptimizationParameters\SingleDriverRoute10StopsTestDataProviderUnit.pas',
  ConnectionUnit in '..\Connection\ConnectionUnit.pas',
  IConnectionUnit in '..\Connection\IConnectionUnit.pas',
  NullableBasicTypesUnit in '..\Common\NullableBasicTypesUnit.pas',
  JSONDictionaryIntermediateObjectUnit in '..\Json\JSONDictionaryIntermediateObjectUnit.pas',
  JSONNullableAttributeUnit in '..\Json\JSONNullableAttributeUnit.pas',
  JSONNullableMarshalConverterUnit in '..\Json\JSONNullableMarshalConverterUnit.pas',
  MarshalUnMarshalUnit in '..\Json\MarshalUnMarshalUnit.pas',
  NullableArrayInterceptorUnit in '..\Json\NullableArrayInterceptorUnit.pas',
  NullableInterceptorUnit in '..\Json\NullableInterceptorUnit.pas',
  ActivityParametersUnit in '..\QueryTypes\ActivityParametersUnit.pas',
  AddAddressNoteResponseUnit in '..\QueryTypes\AddAddressNoteResponseUnit.pas',
  AddressBookParametersUnit in '..\QueryTypes\AddressBookParametersUnit.pas',
  AddressesOrderInfoUnit in '..\QueryTypes\AddressesOrderInfoUnit.pas',
  AddressParametersUnit in '..\QueryTypes\AddressParametersUnit.pas',
  AddRouteDestinationRequestUnit in '..\QueryTypes\AddRouteDestinationRequestUnit.pas',
  AvoidanceZoneQueryUnit in '..\QueryTypes\AvoidanceZoneQueryUnit.pas',
  DataObjectOptimizationsResponseUnit in '..\QueryTypes\DataObjectOptimizationsResponseUnit.pas',
  DeleteRouteResponseUnit in '..\QueryTypes\DeleteRouteResponseUnit.pas',
  DuplicateRouteResponseUnit in '..\QueryTypes\DuplicateRouteResponseUnit.pas',
  GenericParametersUnit in '..\QueryTypes\GenericParametersUnit.pas',
  GetActivitiesResponseUnit in '..\QueryTypes\GetActivitiesResponseUnit.pas',
  GetAddressBookContactsResponseUnit in '..\QueryTypes\GetAddressBookContactsResponseUnit.pas',
  GetOrdersResponseUnit in '..\QueryTypes\GetOrdersResponseUnit.pas',
  GPSParametersUnit in '..\QueryTypes\GPSParametersUnit.pas',
  HttpQueryMemberAttributeUnit in '..\QueryTypes\HttpQueryMemberAttributeUnit.pas',
  MoveDestinationToRouteResponseUnit in '..\QueryTypes\MoveDestinationToRouteResponseUnit.pas',
  NoteParametersUnit in '..\QueryTypes\NoteParametersUnit.pas',
  OptimizationParametersUnit in '..\QueryTypes\OptimizationParametersUnit.pas',
  OrderParametersUnit in '..\QueryTypes\OrderParametersUnit.pas',
  RemoveAddressBookContactsRequestUnit in '..\QueryTypes\RemoveAddressBookContactsRequestUnit.pas',
  RemoveDestinationFromOptimizationResponseUnit in '..\QueryTypes\RemoveDestinationFromOptimizationResponseUnit.pas',
  RemoveOptimizationResponseUnit in '..\QueryTypes\RemoveOptimizationResponseUnit.pas',
  RemoveOrdersRequestUnit in '..\QueryTypes\RemoveOrdersRequestUnit.pas',
  RemoveRouteDestinationRequestUnit in '..\QueryTypes\RemoveRouteDestinationRequestUnit.pas',
  RemoveRouteDestinationResponseUnit in '..\QueryTypes\RemoveRouteDestinationResponseUnit.pas',
  RouteParametersQueryUnit in '..\QueryTypes\RouteParametersQueryUnit.pas',
  SingleDriverRoundTripGenericRequestUnit in '..\QueryTypes\SingleDriverRoundTripGenericRequestUnit.pas',
  SingleDriverRoundTripGenericResponseUnit in '..\QueryTypes\SingleDriverRoundTripGenericResponseUnit.pas',
  StatusResponseUnit in '..\QueryTypes\StatusResponseUnit.pas',
  BaseExampleUnit in 'Examples\BaseExampleUnit.pas',
  SingleDriverRoute10StopsUnit in 'Examples\Optimization\SingleDriverRoute10StopsUnit.pas',
  SingleDriverRoundTripUnit in 'Examples\Optimization\SingleDriverRoundTripUnit.pas',
  SingleDriverRoundTripGenericUnit in 'Examples\Optimization\SingleDriverRoundTripGenericUnit.pas',
  MultipleDepotMultipleDriverUnit in 'Examples\Optimization\MultipleDepotMultipleDriverUnit.pas',
  MultipleDepotMultipleDriverTimeWindowUnit in 'Examples\Optimization\MultipleDepotMultipleDriverTimeWindowUnit.pas',
  SingleDepotMultipleDriverNoTimeWindowUnit in 'Examples\Optimization\SingleDepotMultipleDriverNoTimeWindowUnit.pas',
  MultipleDepotMultipleDriverWith24StopsTimeWindowUnit in 'Examples\Optimization\MultipleDepotMultipleDriverWith24StopsTimeWindowUnit.pas',
  SingleDriverMultipleTimeWindowsUnit in 'Examples\Optimization\SingleDriverMultipleTimeWindowsUnit.pas',
  ResequenceRouteDestinationsUnit in 'Examples\Routes\ResequenceRouteDestinationsUnit.pas',
  BaseOptimizationExampleUnit in 'Examples\BaseOptimizationExampleUnit.pas',
  AddRouteDestinationsOptimallyUnit in 'Examples\Addresses\AddRouteDestinationsOptimallyUnit.pas',
  AddRouteDestinationsUnit in 'Examples\Addresses\AddRouteDestinationsUnit.pas',
  RemoveRouteDestinationUnit in 'Examples\Addresses\RemoveRouteDestinationUnit.pas',
  MoveDestinationToRouteUnit in 'Examples\Addresses\MoveDestinationToRouteUnit.pas',
  GetOptimizationsUnit in 'Examples\Optimization\GetOptimizationsUnit.pas',
  GetOptimizationUnit in 'Examples\Optimization\GetOptimizationUnit.pas',
  AddDestinationToOptimizationUnit in 'Examples\Addresses\AddDestinationToOptimizationUnit.pas',
  RemoveDestinationFromOptimizationUnit in 'Examples\Optimization\RemoveDestinationFromOptimizationUnit.pas',
  ReOptimizationUnit in 'Examples\Optimization\ReOptimizationUnit.pas',
  ReoptimizeRouteUnit in 'Examples\Routes\ReoptimizeRouteUnit.pas',
  UpdateRouteUnit in 'Examples\Routes\UpdateRouteUnit.pas',
  GetRouteUnit in 'Examples\Routes\GetRouteUnit.pas',
  GetRoutesUnit in 'Examples\Routes\GetRoutesUnit.pas',
  GetUsersUnit in 'Examples\Members\GetUsersUnit.pas',
  GetAddressUnit in 'Examples\Addresses\GetAddressUnit.pas',
  GetAddressNotesUnit in 'Examples\Notes\GetAddressNotesUnit.pas',
  AddAddressNoteUnit in 'Examples\Notes\AddAddressNoteUnit.pas',
  DuplicateRouteUnit in 'Examples\Routes\DuplicateRouteUnit.pas',
  ShareRouteUnit in 'Examples\Routes\ShareRouteUnit.pas',
  DeleteRoutesUnit in 'Examples\Routes\DeleteRoutesUnit.pas',
  RemoveOptimizationUnit in 'Examples\Optimization\RemoveOptimizationUnit.pas',
  CreateLocationUnit in 'Examples\AddressBookContact\CreateLocationUnit.pas',
  GetLocationsUnit in 'Examples\AddressBookContact\GetLocationsUnit.pas',
  UpdateLocationUnit in 'Examples\AddressBookContact\UpdateLocationUnit.pas',
  RemoveLocationsUnit in 'Examples\AddressBookContact\RemoveLocationsUnit.pas',
  GetAvoidanceZoneUnit in 'Examples\AvoidanceZones\GetAvoidanceZoneUnit.pas',
  UpdateAvoidanceZoneUnit in 'Examples\AvoidanceZones\UpdateAvoidanceZoneUnit.pas',
  DeleteAvoidanceZoneUnit in 'Examples\AvoidanceZones\DeleteAvoidanceZoneUnit.pas',
  GetAvoidanceZonesUnit in 'Examples\AvoidanceZones\GetAvoidanceZonesUnit.pas',
  GetAllActivitiesUnit in 'Examples\Activities\GetAllActivitiesUnit.pas',
  AddCircleAvoidanceZoneUnit in 'Examples\AvoidanceZones\AddCircleAvoidanceZoneUnit.pas',
  AddOrderUnit in 'Examples\Orders\AddOrderUnit.pas',
  GetOrdersUnit in 'Examples\Orders\GetOrdersUnit.pas',
  UpdateOrderUnit in 'Examples\Orders\UpdateOrderUnit.pas',
  RemoveOrdersUnit in 'Examples\Orders\RemoveOrdersUnit.pas',
  SetGPSPositionUnit in 'Examples\Tracking\SetGPSPositionUnit.pas',
  TrackDeviceLastLocationHistoryUnit in 'Examples\Tracking\TrackDeviceLastLocationHistoryUnit.pas',
  GenericExampleUnit in 'Examples\Generic\GenericExampleUnit.pas',
  GenericExampleShortcutUnit in 'Examples\Generic\GenericExampleShortcutUnit.pas',
  Route4MeExamplesUnit in 'Examples\Route4MeExamplesUnit.pas',
  MergeRouteRequestUnit in '..\QueryTypes\MergeRouteRequestUnit.pas',
  MergeRoutesUnit in 'Examples\Routes\MergeRoutesUnit.pas',
  UpdateRoutesCustomDataRequestUnit in '..\QueryTypes\UpdateRoutesCustomDataRequestUnit.pas',
  UpdateRoutesCustomFieldsUnit in 'Examples\Routes\UpdateRoutesCustomFieldsUnit.pas',
  ResequenceAllRoutesRequestUnit in '..\QueryTypes\ResequenceAllRoutesRequestUnit.pas',
  ResequenceAllRouteDestinationsUnit in 'Examples\Routes\ResequenceAllRouteDestinationsUnit.pas',
  AddOrderToRouteUnit in 'Examples\Orders\AddOrderToRouteUnit.pas',
  AddOrderToRouteRequestUnit in '..\QueryTypes\AddOrderToRouteRequestUnit.pas',
  AddOrderToRouteParameterProviderUnit in 'TestDataProviders\AddOrderToRouteParameterProviderUnit.pas',
  AddOrderToOptimizationUnit in 'Examples\Orders\AddOrderToOptimizationUnit.pas',
  AddOrderToOptimizationRequestUnit in '..\QueryTypes\AddOrderToOptimizationRequestUnit.pas',
  GetOrderUnit in 'Examples\Orders\GetOrderUnit.pas',
  GetOrdersByDateUnit in 'Examples\Orders\GetOrdersByDateUnit.pas',
  GetOrdersScheduledForUnit in 'Examples\Orders\GetOrdersScheduledForUnit.pas',
  GetOrdersWithCustomFieldsResponseUnit in '..\QueryTypes\GetOrdersWithCustomFieldsResponseUnit.pas',
  GetOrdersWithCustomFieldsUnit in 'Examples\Orders\GetOrdersWithCustomFieldsUnit.pas',
  GetOrdersWithSpecifiedTextUnit in 'Examples\Orders\GetOrdersWithSpecifiedTextUnit.pas',
  MarkAddressAsDetectedAsVisitedRequestUnit in '..\QueryTypes\MarkAddressAsDetectedAsVisitedRequestUnit.pas',
  MarkAddressAsDepartedUnit in 'Examples\Addresses\MarkAddressAsDepartedUnit.pas',
  MarkAddressAsVisitedUnit in 'Examples\Addresses\MarkAddressAsVisitedUnit.pas',
  MarkAddressAsDetectedAsVisitedUnit in 'Examples\Addresses\MarkAddressAsDetectedAsVisitedUnit.pas',
  MarkAddressAsDetectedAsDepartedUnit in 'Examples\Addresses\MarkAddressAsDetectedAsDepartedUnit.pas',
  MarkAddressAsDetectedAsDepartedRequestUnit in '..\QueryTypes\MarkAddressAsDetectedAsDepartedRequestUnit.pas',
  ForwardGeocodeAddressUnit in 'Examples\Geocoding\ForwardGeocodeAddressUnit.pas',
  GeocodingActionsUnit in '..\Actions\GeocodingActionsUnit.pas',
  ValidateSessionResponseUnit in '..\QueryTypes\ValidateSessionResponseUnit.pas',
  ValidateSessionUnit in 'Examples\Members\ValidateSessionUnit.pas',
  AddNewUserResponseUnit in '..\QueryTypes\AddNewUserResponseUnit.pas',
  RegisterAccountUnit in 'Examples\Members\RegisterAccountUnit.pas',
  GetUserDetailsUnit in 'Examples\Members\GetUserDetailsUnit.pas',
  RegisterAccountResponseUnit in '..\QueryTypes\RegisterAccountResponseUnit.pas',
  AddNewUserUnit in 'Examples\Members\AddNewUserUnit.pas',
  UserParameterProviderUnit in 'TestDataProviders\UserParameterProviderUnit.pas',
  UserParametersUnit in '..\QueryTypes\UserParametersUnit.pas',
  UpdateUserUnit in 'Examples\Members\UpdateUserUnit.pas',
  RemoveUserUnit in 'Examples\Members\RemoveUserUnit.pas',
  RemoveUserRequestUnit in '..\QueryTypes\RemoveUserRequestUnit.pas',
  AuthenticationUnit in 'Examples\Members\AuthenticationUnit.pas',
  AuthenticationResponseUnit in '..\QueryTypes\AuthenticationResponseUnit.pas',
  DeviceLicenseUnit in 'Examples\Members\DeviceLicenseUnit.pas',
  DeviceLicenseRequestUnit in '..\QueryTypes\DeviceLicenseRequestUnit.pas',
  UserLicenseRequestUnit in '..\QueryTypes\UserLicenseRequestUnit.pas',
  UserLicenseUnit in 'Examples\Members\UserLicenseUnit.pas',
  RegisterWebinarRequestUnit in '..\QueryTypes\RegisterWebinarRequestUnit.pas',
  RegisterWebinarUnit in 'Examples\Members\RegisterWebinarUnit.pas',
  LogSpecificMessageUnit in 'Examples\Activities\LogSpecificMessageUnit.pas',
  GetTeamActivitiesUnit in 'Examples\Activities\GetTeamActivitiesUnit.pas',
  ActivityRequestUnit in '..\QueryTypes\ActivityRequestUnit.pas',
  ActivityUnit in '..\DataTypes\ActivityUnit.pas',
  GetActivitiesQueryUnit in '..\QueryTypes\GetActivitiesQueryUnit.pas',
  GetAllDestinationInsertedActivitiesUnit in 'Examples\Activities\GetAllDestinationInsertedActivitiesUnit.pas',
  GetAllDestinationMarkedAsDepartedActivitiesUnit in 'Examples\Activities\GetAllDestinationMarkedAsDepartedActivitiesUnit.pas',
  GetAllDestinationMarkedAsVisitedActivitiesUnit in 'Examples\Activities\GetAllDestinationMarkedAsVisitedActivitiesUnit.pas',
  GetAllNoteInsertedActivitiesUnit in 'Examples\Activities\GetAllNoteInsertedActivitiesUnit.pas',
  GetAreaAddedActivitiesUnit in 'Examples\Activities\GetAreaAddedActivitiesUnit.pas',
  GetAreaRemovedActivitiesUnit in 'Examples\Activities\GetAreaRemovedActivitiesUnit.pas',
  GetAreaUpdatedActivitiesUnit in 'Examples\Activities\GetAreaUpdatedActivitiesUnit.pas',
  GetDestinationDeletedActivitiesUnit in 'Examples\Activities\GetDestinationDeletedActivitiesUnit.pas',
  GetDestinationInsertedActivitiesUnit in 'Examples\Activities\GetDestinationInsertedActivitiesUnit.pas',
  GetDestinationMarkedAsDepartedActivitiesUnit in 'Examples\Activities\GetDestinationMarkedAsDepartedActivitiesUnit.pas',
  GetDestinationMovedActivitiesUnit in 'Examples\Activities\GetDestinationMovedActivitiesUnit.pas',
  GetDestinationOutOfSequenceActivitiesUnit in 'Examples\Activities\GetDestinationOutOfSequenceActivitiesUnit.pas',
  GetDestinationUpdatedActivitiesUnit in 'Examples\Activities\GetDestinationUpdatedActivitiesUnit.pas',
  GetDriverArrivedEarlyActivitiesUnit in 'Examples\Activities\GetDriverArrivedEarlyActivitiesUnit.pas',
  GetDriverArrivedLateActivitiesUnit in 'Examples\Activities\GetDriverArrivedLateActivitiesUnit.pas',
  GetDriverArrivedOnTimeActivitiesUnit in 'Examples\Activities\GetDriverArrivedOnTimeActivitiesUnit.pas',
  GetGeofenceEnteredActivitiesUnit in 'Examples\Activities\GetGeofenceEnteredActivitiesUnit.pas',
  GetGeofenceLeftActivitiesUnit in 'Examples\Activities\GetGeofenceLeftActivitiesUnit.pas',
  GetMemberCreatedActivitiesUnit in 'Examples\Activities\GetMemberCreatedActivitiesUnit.pas',
  GetMemberDeletedActivitiesUnit in 'Examples\Activities\GetMemberDeletedActivitiesUnit.pas',
  GetMemberModifiedActivitiesUnit in 'Examples\Activities\GetMemberModifiedActivitiesUnit.pas',
  GetNoteInsertedActivitiesUnit in 'Examples\Activities\GetNoteInsertedActivitiesUnit.pas',
  GetRouteDeletedActivitiesUnit in 'Examples\Activities\GetRouteDeletedActivitiesUnit.pas',
  GetRouteOptimizedActivitiesUnit in 'Examples\Activities\GetRouteOptimizedActivitiesUnit.pas',
  GetRouteOwnerChangedActivitiesUnit in 'Examples\Activities\GetRouteOwnerChangedActivitiesUnit.pas',
  GetLocationsByIdsUnit in 'Examples\AddressBookContact\GetLocationsByIdsUnit.pas',
  GetLocationUnit in 'Examples\AddressBookContact\GetLocationUnit.pas',
  DisplayRoutedUnit in 'Examples\AddressBookContact\DisplayRoutedUnit.pas',
  LocationSearchUnit in 'Examples\AddressBookContact\LocationSearchUnit.pas',
  AddressBookContactFindResponseUnit in '..\QueryTypes\AddressBookContactFindResponseUnit.pas',
  AddPolygonAvoidanceZoneUnit in 'Examples\AvoidanceZones\AddPolygonAvoidanceZoneUnit.pas',
  AddRectangularAvoidanceZoneUnit in 'Examples\AvoidanceZones\AddRectangularAvoidanceZoneUnit.pas',
  CustomerQuestionAbout7StopsUnit in 'CustomerQuestions\CustomerQuestionAbout7StopsUnit.pas',
  TerritoryActionsUnit in '..\Actions\TerritoryActionsUnit.pas',
  TerritoryContourUnit in '..\DataTypes\TerritoryContourUnit.pas',
  TerritoryUnit in '..\DataTypes\TerritoryUnit.pas',
  GetTerritoriesResponseUnit in '..\QueryTypes\GetTerritoriesResponseUnit.pas',
  AddCircleTerritoryUnit in 'Examples\Territories\AddCircleTerritoryUnit.pas',
  AddPolygonTerritoryUnit in 'Examples\Territories\AddPolygonTerritoryUnit.pas',
  AddRectangularTerritoryUnit in 'Examples\Territories\AddRectangularTerritoryUnit.pas',
  GetTerritoriesUnit in 'Examples\Territories\GetTerritoriesUnit.pas',
  GetTerritoryUnit in 'Examples\Territories\GetTerritoryUnit.pas',
  RemoveTerritoryUnit in 'Examples\Territories\RemoveTerritoryUnit.pas',
  UpdateTerritoryUnit in 'Examples\Territories\UpdateTerritoryUnit.pas',
  UpdateTerritoryRequestUnit in '..\QueryTypes\UpdateTerritoryRequestUnit.pas',
  PositionUnit in '..\DataTypes\PositionUnit.pas';

{var
  CustomerSample: TCustomerQuestionAbout7Stops;}
begin
  try
    TExamples.Run();
{    CustomerSample := TCustomerQuestionAbout7Stops.Create;
    try
      CustomerSample.Execute;
    finally
      FreeAndNil(CustomerSample);
    end;}
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ReadLn;
    end;
  end;
end.
