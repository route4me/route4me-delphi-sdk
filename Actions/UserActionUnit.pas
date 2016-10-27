unit UserActionUnit;

interface

uses
  SysUtils, BaseActionUnit,
  DataObjectUnit, GenericParametersUnit, UserUnit;

type
  TUserActions = class(TBaseAction)
  public
    function Get(Parameters: TGenericParameters;
      out ErrorString: String): TArray<TUser>;
  end;

implementation
{ TUserActions }

uses SettingsUnit;

function TUserActions.Get(Parameters: TGenericParameters;
  out ErrorString: String): TArray<TUser>;
var
  List: TUserList;
begin
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
end;

end.
