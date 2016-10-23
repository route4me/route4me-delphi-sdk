program Route4MeProject;

uses
  Vcl.Forms,
  MainFormUnit in 'MainFormUnit.pas' {Form1},
  GenericParametersUnit in 'QueryTypes\GenericParametersUnit.pas',
  OptimizationParametersUnit in 'QueryTypes\OptimizationParametersUnit.pas',
  RouteParametersUnit in 'DataTypes\RouteParametersUnit.pas',
  AddressUnit in 'DataTypes\AddressUnit.pas',
  Route4MeManagerUnit in 'Route4MeManagerUnit.pas',
  DataObjectUnit in 'DataTypes\DataObjectUnit.pas',
  EnumsUnit in 'DataTypes\EnumsUnit.pas',
  JSONNullableAttributeUnit in 'Nullabled\JSONNullableAttributeUnit.pas',
  NullableInterceptorUnit in 'Nullabled\NullableInterceptorUnit.pas',
  JSONNullableConverterUnit in 'Nullabled\JSONNullableConverterUnit.pas',
  NullableBasicTypesUnit in 'Nullabled\NullableBasicTypesUnit.pas',
  JSONDictionaryInterceptorObjectUnit in 'Nullabled\JSONDictionaryInterceptorObjectUnit.pas',
  AddressBookContactUnit in 'DataTypes\AddressBookContactUnit.pas',
  AddressBookContactActionsUnit in 'Actions\AddressBookContactActionsUnit.pas',
  AddressBookParametersUnit in 'QueryTypes\AddressBookParametersUnit.pas',
  MarshalUnMarshalUnit in 'Common\MarshalUnMarshalUnit.pas',
  ConnectionUnit in 'Common\ConnectionUnit.pas',
  IConnectionUnit in 'Common\IConnectionUnit.pas',
  SettingsUnit in 'Common\SettingsUnit.pas',
  BaseActionUnit in 'Actions\BaseActionUnit.pas',
  UtilsUnit in 'Common\UtilsUnit.pas',
  HttpQueryMemberAttributeUnit in 'QueryTypes\HttpQueryMemberAttributeUnit.pas',
  OptimizationActionUnit in 'Actions\OptimizationActionUnit.pas',
  DirectionPathPointUnit in 'DataTypes\DirectionPathPointUnit.pas',
  DirectionLocationUnit in 'DataTypes\DirectionLocationUnit.pas',
  DirectionStepUnit in 'DataTypes\DirectionStepUnit.pas',
  LinksUnit in 'DataTypes\LinksUnit.pas',
  TrackingHistoryUnit in 'DataTypes\TrackingHistoryUnit.pas',
  DirectionUnit in 'DataTypes\DirectionUnit.pas',
  RouteActionUnit in 'Actions\RouteActionUnit.pas',
  AddressesOrderInfoUnit in 'AdditionalDataTypes\AddressesOrderInfoUnit.pas',
  CommonTypesUnit in 'Common\CommonTypesUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
