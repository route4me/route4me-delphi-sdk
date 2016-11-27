unit GetUserDetailsUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetUserDetails = class(TBaseExample)
  public
    procedure Execute(MemberId: integer);
  end;

implementation

uses UserUnit;

procedure TGetUserDetails.Execute(MemberId: integer);
var
  ErrorString: String;
  User: TUser;
begin
  User := Route4MeManager.User.Get(MemberId, ErrorString);
  try
    WriteLn('');

    if (User <> nil) then
    begin
      WriteLn('GetUserDetails executed successfully');
      WriteLn('');
    end
    else
      WriteLn(Format('GetUserDetails error: "%s"', [ErrorString]));
  finally
    FreeAndNil(User);
  end;
end;

end.
