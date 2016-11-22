unit MarkAddressAsDetectedAsDepartedUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TMarkAddressAsDetectedAsDeparted = class(TBaseExample)
  public
    procedure Execute(RouteId: String; RouteDestinationId: integer;
      IsDeparted: boolean);
  end;

implementation

procedure TMarkAddressAsDetectedAsDeparted.Execute(RouteId: String;
  RouteDestinationId: integer; IsDeparted: boolean);
var
  ErrorString: String;
begin
  Route4MeManager.Address.MarkAsDetectedAsDeparted(
    RouteId, RouteDestinationId, IsDeparted, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
    WriteLn('MarkAddressAsDetectedAsDeparted executed successfully')
  else
    WriteLn(Format('MarkAddressAsDetectedAsDeparted error: "%s"', [ErrorString]));
end;

end.
