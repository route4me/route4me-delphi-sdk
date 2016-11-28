unit AuthenticationUnit;

interface

uses SysUtils, BaseExampleUnit, NullableBasicTypesUnit, UserParametersUnit;

type
  TAuthentication = class(TBaseExample)
  public
    function Execute(EMail, Password: String): NullableInteger;
  end;

implementation

function TAuthentication.Execute(EMail, Password: String): NullableInteger;
var
  ErrorString: String;
begin
  Result := Route4MeManager.User.Authentication(EMail, Password, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
  begin
    WriteLn(Format('Authentication successfully, SessionId = %d', [Result.Value]));
    WriteLn('');
  end
  else
    WriteLn(Format('Authentication error: "%s"', [ErrorString]));
end;

end.
