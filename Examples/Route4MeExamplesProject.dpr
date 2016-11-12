program Route4MeExamplesProject;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Route4MeExamplesUnit in 'Route4MeExamplesUnit.pas',
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
  OptimizationActionUnit in '..\Actions\OptimizationActionUnit.pas',
  DirectionUnit in '..\DataTypes\DirectionUnit.pas',
  DirectionLocationUnit in '..\DataTypes\DirectionLocationUnit.pas',
  DirectionStepUnit in '..\DataTypes\DirectionStepUnit.pas',
  DirectionPathPointUnit in '..\DataTypes\DirectionPathPointUnit.pas',
  LinksUnit in '..\DataTypes\LinksUnit.pas',
  TrackingHistoryUnit in '..\DataTypes\TrackingHistoryUnit.pas',
  RouteActionUnit in '..\Actions\RouteActionUnit.pas',
  CommonTypesUnit in '..\Common\CommonTypesUnit.pas',
  MainExamplesUnit in 'MainExamplesUnit.pas',
  IRoute4MeManagerUnit in '..\IRoute4MeManagerUnit.pas',
  OutputUnit in '..\Common\OutputUnit.pas',
  UserActionUnit in '..\Actions\UserActionUnit.pas',
  UserUnit in '..\DataTypes\UserUnit.pas',
  ActivityActionsUnit in '..\Actions\ActivityActionsUnit.pas',
  AddressActionUnit in '..\Actions\AddressActionUnit.pas',
  AddressNoteActionUnit in '..\Actions\AddressNoteActionUnit.pas',
  AvoidanceZoneActionUnit in '..\Actions\AvoidanceZoneActionUnit.pas',
  OrderActionUnit in '..\Actions\OrderActionUnit.pas',
  TrackingActionsUnit in '..\Actions\TrackingActionsUnit.pas',
  ActivityUnit in '..\DataTypes\ActivityUnit.pas',
  AddressNoteUnit in '..\DataTypes\AddressNoteUnit.pas',
  AvoidanceZoneUnit in '..\DataTypes\AvoidanceZoneUnit.pas',
  OrderUnit in '..\DataTypes\OrderUnit.pas',
  TerritoryUnit in '..\DataTypes\TerritoryUnit.pas',
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
  AvoidanceZoneParametersUnit in '..\QueryTypes\AvoidanceZoneParametersUnit.pas',
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
  StatusResponseUnit in '..\QueryTypes\StatusResponseUnit.pas';

begin
  try
//    ReportMemoryLeaksOnShutdown := True;
    TExamples.Run();
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ReadLn;
    end;
  end;
end.
