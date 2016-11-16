unit UpdateRoutesCustomFieldsUnit;

interface

uses SysUtils, BaseExampleUnit, CommonTypesUnit;

type
  TUpdateRoutesCustomFields = class(TBaseExample)
  public
    procedure Execute(RouteId: String; RouteDestinationId: integer);
  end;

implementation

procedure TUpdateRoutesCustomFields.Execute(RouteId: String;
  RouteDestinationId: integer);
var
  ErrorString: String;
  CustomFields: TListStringPair;
begin
  CustomFields := TListStringPair.Create;
  try
    CustomFields.Add(TStringPair.Create('animal', 'lion'));
    CustomFields.Add(TStringPair.Create('form', 'rectangle'));

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
  finally
    FreeAndNil(CustomFields);
  end;
end;

end.
