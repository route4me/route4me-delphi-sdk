unit UpdateRoutesCustomFieldsUnit;

interface

uses SysUtils, BaseExampleUnit, CommonTypesUnit;

type
  TUpdateRoutesCustomFields = class(TBaseExample)
  public
    procedure Execute(RouteId: String; RouteDestinationId: integer;
      CustomFields: TListStringPair);
  end;

implementation

procedure TUpdateRoutesCustomFields.Execute(RouteId: String;
  RouteDestinationId: integer; CustomFields: TListStringPair);
var
  ErrorString: String;
begin
  Route4MeManager.Route.UpdateCustomFields(RouteId, RouteDestinationId,
    CustomFields, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
  begin
    WriteLn('UpdateCustomFields executed successfully');
    WriteLn(Format('Route ID: %s', [RouteId]));
  end
  else
    WriteLn(Format('UpdateCustomFields error: %s', [ErrorString]));
end;

end.
