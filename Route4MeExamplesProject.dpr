program Route4MeExamplesProject;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
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
  HttpQueryMemberAttributeUnit in 'QueryTypes\HttpQueryMemberAttributeUnit.pas',
  RouteActionUnit in 'Actions\RouteActionUnit.pas',
  AddressesOrderInfoUnit in 'AdditionalDataTypes\AddressesOrderInfoUnit.pas',
  CommonTypesUnit in 'Common\CommonTypesUnit.pas';

var
  Examples: TRoute4MeExamples;
  DataObject: TDataObject;
  RouteSingleDriverRoute10Stops: TDataObjectRoute;
  RouteId_SingleDriverRoute10Stops: NullableString;
begin
  try
    Examples := TRoute4MeExamples.Create();
//    Examples := TRoute4MeExamples.CreateDebug();

    DataObject := Examples.SingleDriverRoute10Stops();

    if (DataObject <> nil) and (DataObject.Routes <> nil) and (Length(DataObject.Routes) > 0) then
    begin
      RouteSingleDriverRoute10Stops := DataObject.Routes[0];
      RouteId_SingleDriverRoute10Stops := RouteSingleDriverRoute10Stops.RouteId;
      Examples.ResequenceRouteDestinations(RouteSingleDriverRoute10Stops);
    end
    else
    begin
      RouteSingleDriverRoute10Stops := nil;
      RouteId_SingleDriverRoute10Stops := NullableString.Null;
      WriteLn('ResequenceRouteDestinations not called. RouteSingleDriverRoute10Stops = null.');
    end;

    ReadLn;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ReadLn;
    end;
  end;
end.
