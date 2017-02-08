unit AddConfigValueUnit;

interface

uses SysUtils, BaseExampleUnit, EnumsUnit;

type
  TAddConfigValue = class(TBaseExample)
  public
    function Execute(Key, Value: String): boolean;
  end;

implementation

function TAddConfigValue.Execute(Key, Value: String): boolean;
var
  ErrorString: String;
begin
  Result := Route4MeManager.User.AddConfigValue(Key, Value, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
  begin
    if Result then
      WriteLn('AddConfigValue successfully')
    else
      WriteLn('AddConfigValue error');
    WriteLn('');
  end
  else
    WriteLn(Format('AddConfigValue error: "%s"', [ErrorString]));
end;

end.
