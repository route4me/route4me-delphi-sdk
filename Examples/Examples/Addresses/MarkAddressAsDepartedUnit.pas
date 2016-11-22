unit MarkAddressAsDepartedUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TMarkAddressAsDeparted = class(TBaseExample)
  public
    procedure Execute(RouteId: String; RouteDestinationId: integer;
      IsDeparted: boolean);
  end;

implementation

procedure TMarkAddressAsDeparted.Execute(RouteId: String;
  RouteDestinationId: integer; IsDeparted: boolean);
var
  ErrorString: String;
begin
  Route4MeManager.Address.MarkAsDeparted(
    RouteId, RouteDestinationId, IsDeparted, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
    WriteLn('MarkAddressAsDeparted executed successfully')
  else
    WriteLn(Format('MarkAddressAsDeparted error: "%s"', [ErrorString]));
end;

end.
