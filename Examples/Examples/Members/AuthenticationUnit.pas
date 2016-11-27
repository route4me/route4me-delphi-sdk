unit AuthenticationUnit;

interface

uses SysUtils, BaseExampleUnit, NullableBasicTypesUnit, UserParametersUnit;

type
  TAuthentication = class(TBaseExample)
  public
    function Execute(EMail, Password: String): NullableString;
  end;

implementation

function TAuthentication.Execute(EMail, Password: String): NullableString;
var
  ErrorString: String;
begin
  Result := Route4MeManager.User.Authentication(EMail, Password, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
  begin
    WriteLn(Format('Authentication successfully, SessionId = %s', [Result.Value]));
    WriteLn('');
  end
  else
    WriteLn(Format('Authentication error: "%s"', [ErrorString]));
end;

end.
