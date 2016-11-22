unit MarkAddressAsVisitedUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TMarkAddressAsVisited = class(TBaseExample)
  public
    procedure Execute(RouteId: String; AddressId, MemberId: integer;
      IsVisited: boolean);
  end;

implementation

procedure TMarkAddressAsVisited.Execute(RouteId: String;
  AddressId, MemberId: integer; IsVisited: boolean);
var
  ErrorString: String;
begin
  Route4MeManager.Address.MarkAsVisited(
    RouteId, AddressId, MemberId, IsVisited, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
    WriteLn('MarkAddressAsVisited executed successfully')
  else
    WriteLn(Format('MarkAddressAsVisited error: "%s"', [ErrorString]));
end;

end.
