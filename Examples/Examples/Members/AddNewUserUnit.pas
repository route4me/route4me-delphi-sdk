unit AddNewUserUnit;

interface

uses SysUtils, BaseExampleUnit, NullableBasicTypesUnit, UserParametersUnit;

type
  TAddNewUser = class(TBaseExample)
  public
    function Execute(Parameters: TUserParameters): NullableInteger;
  end;

implementation

function TAddNewUser.Execute(Parameters: TUserParameters): NullableInteger;
var
  ErrorString: String;
begin
  Result := Route4MeManager.User.AddNewUser(Parameters, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
  begin
    WriteLn(Format('New user added successfully, MemberId = %d', [Result.Value]));
    WriteLn('');
  end
  else
    WriteLn(Format('AddNewUser error: "%s"', [ErrorString]));
end;

end.
