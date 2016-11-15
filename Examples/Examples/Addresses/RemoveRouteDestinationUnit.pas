unit RemoveRouteDestinationUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TRemoveRouteDestination = class(TBaseExample)
  public
    procedure Execute(RouteId: String; DestinationId: integer);
  end;

implementation

procedure TRemoveRouteDestination.Execute(RouteId: String; DestinationId: integer);
var
  ErrorString: String;
  Deleted: boolean;
begin
  Deleted := Route4MeManager.Route.Remove(RouteId, DestinationId, ErrorString);

  WriteLn('');

  if (Deleted) then
  begin
    WriteLn('RemoveRouteDestination executed successfully');
    WriteLn(Format('Destination ID: %d', [DestinationId]));
  end
  else
    WriteLn(Format('RemoveRouteDestination error: "%s"', [ErrorString]));
end;

end.
