unit MergeRoutesUnit;

interface

uses SysUtils, BaseExampleUnit, CommonTypesUnit;

type
  TMergeRoutes = class(TBaseExample)
  public
    procedure Execute(RouteIds: TListString);
  end;

implementation

procedure TMergeRoutes.Execute(RouteIds: TListString);
var
  ErrorString: String;
begin
  Route4MeManager.Route.Merge(RouteIds.ToArray, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
  begin
    WriteLn('MergeRoutes executed successfully');
    WriteLn('');
  end
  else
    WriteLn(Format('MergeRoutes error "%s"', [ErrorString]));
end;

end.
