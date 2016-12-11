unit RemoveTerritoryUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TRemoveTerritory = class(TBaseExample)
  public
    procedure Execute(TerritoryId: String);
  end;

implementation

procedure TRemoveTerritory.Execute(TerritoryId: String);
var
  ErrorString: String;
begin
  Route4MeManager.Territory.Remove(TerritoryId, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
  begin
    WriteLn('RemoveTerritory executed successfully');
    WriteLn(Format('Territory ID: %s', [TerritoryId]));
  end
  else
    WriteLn(Format('RemoveTerritory error: "%s"', [ErrorString]));
end;

end.
