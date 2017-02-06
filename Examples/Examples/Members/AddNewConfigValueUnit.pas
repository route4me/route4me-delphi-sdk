unit AddNewConfigValueUnit;

interface

uses SysUtils, BaseExampleUnit, EnumsUnit;

type
  TAddNewConfigValue = class(TBaseExample)
  public
    function Execute(Key, Value: String): boolean;
  end;

implementation

function TAddNewConfigValue.Execute(Key, Value: String): boolean;
var
  ErrorString: String;
begin
  Result := Route4MeManager.User.AddNewConfigValue(Key, Value, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
  begin
    if Result then
      WriteLn('AddNewConfigValue successfully')
    else
      WriteLn('AddNewConfigValue error');
    WriteLn('');
  end
  else
    WriteLn(Format('AddNewConfigValue error: "%s"', [ErrorString]));
end;

end.
