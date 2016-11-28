unit ValidateSessionUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TValidateSession = class(TBaseExample)
  public
    procedure Execute(SessionId, MemberId: integer);
  end;

implementation

uses UserUnit;

procedure TValidateSession.Execute(SessionId, MemberId: integer);
var
  ErrorString: String;
  IsSessionValid: boolean;
  IsSessionValidStr: String;
begin
  IsSessionValid := Route4MeManager.User.IsSessionValid(
    SessionId, MemberId, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
  begin
    if IsSessionValid then
      IsSessionValidStr := 'is valid'
    else
      IsSessionValidStr := 'not is valid';

    WriteLn(Format('ValidateSession executed successfully, session %s',
      [IsSessionValidStr]));
    WriteLn('');
  end
  else
    WriteLn(Format('ValidateSession error: "%s"', [ErrorString]));
end;

end.
