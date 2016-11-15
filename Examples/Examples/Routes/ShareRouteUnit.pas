unit ShareRouteUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TShareRoute = class(TBaseExample)
  public
    procedure Execute(RouteId: String; RecipientEmail: String);
  end;

implementation

procedure TShareRoute.Execute(RouteId: String; RecipientEmail: String);
var
  ErrorString: String;
begin
  Route4MeManager.Route.Share(RouteId, RecipientEmail, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
    WriteLn('ShareRoute executed successfully')
  else
    WriteLn(Format('ShareRoute error: "%s"', [ErrorString]));
end;

end.
