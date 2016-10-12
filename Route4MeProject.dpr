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
  R4MeInfrastructureSettingsUnit in 'R4MeInfrastructureSettingsUnit.pas',
  JsonSerializerUnit in 'JsonSerializerUnit.pas',
  JSONNullableAttributeUnit in 'Nullabled\JSONNullableAttributeUnit.pas',
  NullableInterceptorUnit in 'Nullabled\NullableInterceptorUnit.pas',
  JSONNullableConverterUnit in 'Nullabled\JSONNullableConverterUnit.pas',
  NullableBasicTypesUnit in 'Nullabled\NullableBasicTypesUnit.pas',
  JSONDictionaryInterceptorObjectUnit in 'Nullabled\JSONDictionaryInterceptorObjectUnit.pas',
  AddressBookContactUnit in 'DataTypes\AddressBookContactUnit.pas',
  AddressBookContactActionsUnit in 'Actions\AddressBookContactActionsUnit.pas',
  AddressBookParametersUnit in 'QueryTypes\AddressBookParametersUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
