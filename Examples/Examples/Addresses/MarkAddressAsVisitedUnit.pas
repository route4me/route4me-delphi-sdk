unit MarkAddressAsVisitedUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TMarkAddressAsVisited = class(TBaseExample)
  public
    procedure Execute(RouteId: String; RouteDestinationId: integer;
      IsVisited: boolean);
  end;

implementation

procedure TMarkAddressAsVisited.Execute(RouteId: String;


  ErrorString: String;
begin
  Route4MeManager.Address.MarkAsVisited(
    RouteId, RouteDestinationId, IsVisited, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
    WriteLn('MarkAddressAsVisited executed successfully')
  else
    WriteLn(Format('MarkAddressAsVisited error: "%s"', [ErrorString]));
end;

end.