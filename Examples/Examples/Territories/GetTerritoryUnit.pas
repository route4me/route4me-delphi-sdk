unit GetTerritoryUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetTerritory = class(TBaseExample)
  public
    procedure Execute(TerritoryId: String);
  end;

implementation

uses TerritoryUnit;

procedure TGetTerritory.Execute(TerritoryId: String);
var
  ErrorString: String;
  Territory: TTerritory;
  GetEnclosedAddresses: boolean;
begin
  GetEnclosedAddresses := False;
  Territory := Route4MeManager.Territory.Get(
    TerritoryId, GetEnclosedAddresses, ErrorString);
  try
    WriteLn('');

    if (Territory <> nil) then
    begin
      WriteLn('GetTerritory executed successfully');
      WriteLn(Format('Territory ID: %s', [Territory.Id.Value]));
    end
    else
      WriteLn(Format('GetTerritory error: %s', [ErrorString]));
  finally
    FreeAndNil(Territory);
  end;
end;

end.
