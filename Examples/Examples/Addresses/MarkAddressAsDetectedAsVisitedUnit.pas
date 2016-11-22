unit MarkAddressAsDetectedAsVisitedUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TMarkAddressAsDetectedAsVisited = class(TBaseExample)
  public
    procedure Execute(RouteId: String; RouteDestinationId: integer;
      IsVisited: boolean);
  end;

implementation

procedure TMarkAddressAsDetectedAsVisited.Execute(RouteId: String;
  RouteDestinationId: integer; IsVisited: boolean);
var
  ErrorString: String;
begin
  Route4MeManager.Address.MarkAsDetectedAsVisited(
    RouteId, RouteDestinationId, IsVisited, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
    WriteLn('MarkAddressAsDetectedAsVisited executed successfully')
  else
    WriteLn(Format('MarkAddressAsDetectedAsVisited error: "%s"', [ErrorString]));
end;

end.
