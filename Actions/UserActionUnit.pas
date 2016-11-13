unit UserActionUnit;

interface

uses
  SysUtils, BaseActionUnit,
  DataObjectUnit, GenericParametersUnit, UserUnit;

type
  TUserActions = class(TBaseAction)
  public
    function Get(out ErrorString: String): TArray<TUser>;
  end;

implementation
{ TUserActions }

uses SettingsUnit;

function TUserActions.Get(out ErrorString: String): TArray<TUser>;
var
  List: TUserList;
  Parameters: TGenericParameters;
begin
  Parameters := TGenericParameters.Create;
  try
    List := FConnection.Get(TSettings.GetUsersHost,
      Parameters, TUserList, ErrorString) as TUserList;
    try
      if (List <> nil) then
        Result := List.ToArray
      else
        SetLength(Result, 0);
    finally
      FreeAndNil(List);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
