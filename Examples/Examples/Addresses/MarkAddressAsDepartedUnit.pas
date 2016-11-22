unit MarkAddressAsDepartedUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TMarkAddressAsDeparted = class(TBaseExample)
  public
    procedure Execute(RouteId: String; AddressId, MemberId: integer;
      IsDeparted: boolean);
  end;

implementation

procedure TMarkAddressAsDeparted.Execute(RouteId: String;
  AddressId, MemberId: integer; IsDeparted: boolean);
var
  ErrorString: String;
begin
  Route4MeManager.Address.MarkAsDeparted(
    RouteId, AddressId, MemberId, IsDeparted, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
    WriteLn('MarkAddressAsDeparted executed successfully')
  else
    WriteLn(Format('MarkAddressAsDeparted error: "%s"', [ErrorString]));
end;

end.
