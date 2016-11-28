unit GetUsersUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetUsers = class(TBaseExample)
  public
    procedure Execute;
  end;

implementation

uses UserUnit;

procedure TGetUsers.Execute;
var
  ErrorString: String;
  Users: TUserList;
begin
  Users := Route4MeManager.User.Get(ErrorString);
  try
    WriteLn('');

    if (Users.Count > 0) then
    begin
      WriteLn(Format('GetUsers executed successfully, %d users returned',
        [Users.Count]));
      WriteLn('');
    end
    else
      WriteLn(Format('GetUsers error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Users);
  end;
end;

end.
