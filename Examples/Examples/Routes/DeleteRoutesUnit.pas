unit DeleteRoutesUnit;

interface

uses SysUtils, BaseExampleUnit, CommonTypesUnit;

type
  TDeleteRoutes = class(TBaseExample)
  public
    procedure Execute(RouteIds: TStringArray);
  end;

implementation

procedure TDeleteRoutes.Execute(RouteIds: TStringArray);
var
  ErrorString: String;
  DeletedRouteIds: TStringArray;
begin
  DeletedRouteIds := Route4MeManager.Route.Delete(RouteIds, ErrorString);

  WriteLn('');

  if (Length(DeletedRouteIds) > 0) then
  begin
    WriteLn(Format('DeleteRoutes executed successfully, %d routes deleted',
      [Length(DeletedRouteIds)]));
    WriteLn('');
  end
  else
    WriteLn(Format('DeleteRoutes error "%s"', [ErrorString]));
end;

end.
