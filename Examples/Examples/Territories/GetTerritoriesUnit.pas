unit GetTerritoriesUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetTerritories = class(TBaseExample)
  public
    procedure Execute;
  end;

implementation

uses TerritoryUnit;

procedure TGetTerritories.Execute;
var
  ErrorString: String;
  Territories: TTerritoryList;
begin
  Territories := Route4MeManager.Territory.GetList(ErrorString);
  try
    WriteLn('');

    if (Territories.Count > 0) then
      WriteLn(Format('GetTerritories executed successfully, %d zones returned',
        [Territories.Count]))
    else
      WriteLn(Format('GetTerritories error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Territories);
  end;
end;

end.
