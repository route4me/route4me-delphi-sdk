unit RemoveUserUnit;

interface

uses SysUtils, BaseExampleUnit, UserParametersUnit;

type
  TRemoveUser = class(TBaseExample)
  public
    procedure Execute(MemberId: integer);
  end;

implementation

procedure TRemoveUser.Execute(MemberId: integer);
var
  ErrorString: String;
begin
  Route4MeManager.User.Remove(MemberId, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
  begin
    WriteLn('User removed successfully');
    WriteLn('');
  end
  else
    WriteLn(Format('RemoveUser error: "%s"', [ErrorString]));
end;

end.
