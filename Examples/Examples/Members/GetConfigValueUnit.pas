unit GetConfigValueUnit;

interface

uses SysUtils, BaseExampleUnit, EnumsUnit;

type
  TGetConfigValue = class(TBaseExample)
  public
    procedure Execute(Key: String);
  end;

implementation

uses
  CommonTypesUnit, NullableBasicTypesUnit;

procedure TGetConfigValue.Execute(Key: String);
var
  ErrorString: String;
  Value: NullableString;
begin
  Value := Route4MeManager.User.GetConfigValue(Key, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
  begin
    if Value.IsNotNull then
      WriteLn(
        Format('GetConfigValue successfully. Key="%s", Value="%s"',
        [Key, Value.Value]))
    else
      WriteLn('GetConfigValue error');
    WriteLn('');
  end
  else
    WriteLn(Format('GetConfigValue error: "%s"', [ErrorString]));
end;

end.
