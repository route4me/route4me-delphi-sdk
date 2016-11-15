unit MoveDestinationToRouteUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TMoveDestinationToRoute = class(TBaseExample)
  public
    procedure Execute(ToRouteId: String;
      RouteDestinationId, AfterDestinationId: integer);
  end;

implementation

procedure TMoveDestinationToRoute.Execute(ToRouteId: String;
  RouteDestinationId, AfterDestinationId: integer);
var
  ErrorString: String;
  Success: boolean;
begin
  Success := Route4MeManager.Route.MoveDestinationToRoute(ToRouteId,
    RouteDestinationId, AfterDestinationId, ErrorString);

  WriteLn('');

  if (Success) then
  begin
    WriteLn('MoveDestinationToRoute executed successfully');
    WriteLn(Format(
      'Destination %d moved to Route %s after Destination %d',
      [RouteDestinationId, ToRouteId, AfterDestinationId]));
  end
  else
    WriteLn(Format('MoveDestinationToRoute error: "%s"', [ErrorString]));
end;

end.
