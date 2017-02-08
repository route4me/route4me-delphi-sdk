unit UpdateConfigValueUnit;

interface

uses SysUtils, BaseExampleUnit, EnumsUnit;

type
  TUpdateConfigValue = class(TBaseExample)
  public
    function Execute(Key, Value: String): boolean;
  end;

implementation

function TUpdateConfigValue.Execute(Key, Value: String): boolean;
var
  ErrorString: String;
begin
  Result := Route4MeManager.User.UpdateConfigValue(Key, Value, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
  begin
    if Result then
      WriteLn('UpdateConfigValue successfully')
    else
      WriteLn('UpdateConfigValue error');
    WriteLn('');
  end
  else
    WriteLn(Format('UpdateConfigValue error: "%s"', [ErrorString]));
end;

end.
