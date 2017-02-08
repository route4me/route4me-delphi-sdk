unit GetAllConfigValuesUnit;

interface

uses SysUtils, BaseExampleUnit, EnumsUnit;

type
  TGetAllConfigValues = class(TBaseExample)
  public
    procedure Execute;
  end;

implementation

uses
  CommonTypesUnit;

procedure TGetAllConfigValues.Execute;
var
  ErrorString: String;
  Values: TListStringPair;
  i: Integer;
begin
  Values := Route4MeManager.User.GetAllConfigValues(ErrorString);
  try
    WriteLn('');

    if (ErrorString = EmptyStr) then
    begin
      WriteLn('GetAllConfigValues successfully.');

      for i := 0 to Values.Count - 1 do
        WriteLn(Format('Key="%s", Value="%s"', [Values[i].Key, Values[i].Value]));

      WriteLn('');
    end
    else
      WriteLn(Format('GetAllConfigValues error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Values);
  end;
end;

end.
