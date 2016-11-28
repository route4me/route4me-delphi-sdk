unit RegisterWebinarUnit;

interface

uses SysUtils, BaseExampleUnit, EnumsUnit;

type
  TRegisterWebinar = class(TBaseExample)
  public
    function Execute(EMail, FirstName, LastName, Phone, Company: String;
      MemberId: integer; Date: TDateTime): boolean;
  end;

implementation

function TRegisterWebinar.Execute(EMail, FirstName, LastName, Phone,
  Company: String; MemberId: integer; Date: TDateTime): boolean;
var
  ErrorString: String;
begin
  Result := Route4MeManager.User.RegisterWebinar(EMail, FirstName, LastName,
    Phone, Company, MemberId, Date, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
  begin
    if Result then
      WriteLn('RegisterWebinar successfully')
    else
      WriteLn('RegisterWebinar error');
    WriteLn('');
  end
  else
    WriteLn(Format('RegisterWebinar error: "%s"', [ErrorString]));
end;

end.
