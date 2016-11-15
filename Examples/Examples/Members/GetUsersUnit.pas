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
  Users: TArray<TUser>;
  i: integer;
begin
  Users := Route4MeManager.User.Get(ErrorString);
  try
    WriteLn('');

    if (Length(Users) > 0) then
    begin
      WriteLn(Format('GetUsers executed successfully, %d users returned',
        [Length(Users)]));
      WriteLn('');
    end
    else
      WriteLn(Format('GetUsers error: "%s"', [ErrorString]));
  finally
    for i := Length(Users) - 1 downto 0 do
      FreeAndNil(Users[i]);
  end;
end;

end.
