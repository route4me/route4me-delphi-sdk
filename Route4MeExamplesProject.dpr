program Route4MeExamplesProject;

{$APPTYPE CONSOLE}

{$R *.res}

//  FastMM4 in 'FastMM\FastMM4.pas',
//  FastMM4Messages in 'FastMM\FastMM4Messages.pas',

uses
  System.SysUtils,
  Route4MeExamplesUnit in 'Examples\Route4MeExamplesUnit.pas',
  JSONDictionaryIntermediateObjectUnit in 'Nullabled\JSONDictionaryIntermediateObjectUnit.pas',
  JSONNullableAttributeUnit in 'Nullabled\JSONNullableAttributeUnit.pas',
  JSONNullableMarshalConverterUnit in 'Nullabled\JSONNullableMarshalConverterUnit.pas',
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
  SingleDriverRoundTripGenericTestDataProviderUnit in 'TestDataProviders\OptimizationParameters\SingleDriverRoundTripGenericTestDataProviderUnit.pas',
  SingleDriverRoundTripGenericRequestUnit in 'AdditionalDataTypes\SingleDriverRoundTripGenericRequestUnit.pas',
  SingleDriverRoundTripGenericResponseUnit in 'AdditionalDataTypes\SingleDriverRoundTripGenericResponseUnit.pas',
  IRoute4MeManagerUnit in 'IRoute4MeManagerUnit.pas',
  RouteParametersQueryUnit in 'QueryTypes\RouteParametersQueryUnit.pas',
  DataObjectOptimizationsResponseUnit in 'AdditionalDataTypes\DataObjectOptimizationsResponseUnit.pas',
  RemoveDestinationFromOptimizationResponseUnit in 'AdditionalDataTypes\RemoveDestinationFromOptimizationResponseUnit.pas',
  OutputUnit in 'Common\OutputUnit.pas',
  UserActionUnit in 'Actions\UserActionUnit.pas',
  UserUnit in 'DataTypes\UserUnit.pas',
  ActivityActionsUnit in 'Actions\ActivityActionsUnit.pas',
  AddressActionUnit in 'Actions\AddressActionUnit.pas',
  AddressNoteActionUnit in 'Actions\AddressNoteActionUnit.pas',
  AvoidanceZoneActionUnit in 'Actions\AvoidanceZoneActionUnit.pas',
  OrderActionUnit in 'Actions\OrderActionUnit.pas',
  TrackingActionsUnit in 'Actions\TrackingActionsUnit.pas',
  AddAddressNoteResponseUnit in 'AdditionalDataTypes\AddAddressNoteResponseUnit.pas',
  DeleteRouteResponseUnit in 'AdditionalDataTypes\DeleteRouteResponseUnit.pas',
  DuplicateRouteResponseUnit in 'AdditionalDataTypes\DuplicateRouteResponseUnit.pas',
  GetActivitiesResponseUnit in 'AdditionalDataTypes\GetActivitiesResponseUnit.pas',
  GetAddressBookContactsResponseUnit in 'AdditionalDataTypes\GetAddressBookContactsResponseUnit.pas',
  GetOrdersResponseUnit in 'AdditionalDataTypes\GetOrdersResponseUnit.pas',
  LogCustomActivityResponseUnit in 'AdditionalDataTypes\LogCustomActivityResponseUnit.pas',
  RemoveAddressBookContactsRequestUnit in 'AdditionalDataTypes\RemoveAddressBookContactsRequestUnit.pas',
  RemoveAddressBookContactsResponseUnit in 'AdditionalDataTypes\RemoveAddressBookContactsResponseUnit.pas',
  RemoveOptimizationResponseUnit in 'AdditionalDataTypes\RemoveOptimizationResponseUnit.pas',
  RemoveOrdersRequestUnit in 'AdditionalDataTypes\RemoveOrdersRequestUnit.pas',
  RemoveOrdersResponseUnit in 'AdditionalDataTypes\RemoveOrdersResponseUnit.pas',
  ActivityUnit in 'DataTypes\ActivityUnit.pas',
  AddressNoteUnit in 'DataTypes\AddressNoteUnit.pas',
  AvoidanceZoneUnit in 'DataTypes\AvoidanceZoneUnit.pas',
  OrderUnit in 'DataTypes\OrderUnit.pas',
  TerritoryUnit in 'DataTypes\TerritoryUnit.pas',
  ActivityParametersUnit in 'QueryTypes\ActivityParametersUnit.pas',
  AddressParametersUnit in 'QueryTypes\AddressParametersUnit.pas',
  AvoidanceZoneParametersUnit in 'QueryTypes\AvoidanceZoneParametersUnit.pas',
  AvoidanceZoneQueryUnit in 'QueryTypes\AvoidanceZoneQueryUnit.pas',
  GPSParametersUnit in 'QueryTypes\GPSParametersUnit.pas',
  NoteParametersUnit in 'QueryTypes\NoteParametersUnit.pas',
  OrderParametersUnit in 'QueryTypes\OrderParametersUnit.pas',
  ManifestUnit in 'DataTypes\ManifestUnit.pas',
  GeocodingUnit in 'DataTypes\GeocodingUnit.pas',
  NullableArrayInterceptorUnit in 'Nullabled\NullableArrayInterceptorUnit.pas';

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
