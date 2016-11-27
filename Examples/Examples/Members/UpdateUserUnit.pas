unit UpdateUserUnit;

interface

uses SysUtils, BaseExampleUnit, UserParametersUnit;

type
  TUpdateUser = class(TBaseExample)
  public
    procedure Execute(Parameters: TUserParameters);
  end;

implementation

procedure TUpdateUser.Execute(Parameters: TUserParameters);
var
  ErrorString: String;
begin
  Route4MeManager.User.Update(Parameters, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
  begin
    WriteLn('User updated successfully');
    WriteLn('');
  end
  else
    WriteLn(Format('UpdateUser error: "%s"', [ErrorString]));
end;

end.
