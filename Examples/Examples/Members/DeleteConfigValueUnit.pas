unit DeleteConfigValueUnit;

interface

uses SysUtils, BaseExampleUnit, EnumsUnit;

type
  TDeleteConfigValue = class(TBaseExample)
  public
    function Execute(Key: String): boolean;
  end;

implementation

function TDeleteConfigValue.Execute(Key: String): boolean;
var
  ErrorString: String;
begin
  Result := Route4MeManager.User.DeleteConfigValue(Key, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
  begin
    if Result then
      WriteLn('DeleteConfigValue successfully')
    else
      WriteLn('DeleteConfigValue error');
    WriteLn('');
  end
  else
    WriteLn(Format('DeleteConfigValue error: "%s"', [ErrorString]));
end;

end.
