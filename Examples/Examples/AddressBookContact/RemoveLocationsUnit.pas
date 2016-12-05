unit RemoveLocationsUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TRemoveLocations = class(TBaseExample)
  public
    procedure Execute(AddressIds: TArray<integer>);
  end;

implementation

procedure TRemoveLocations.Execute(AddressIds: TArray<integer>);
var
  ErrorString: String;
  Removed: boolean;
begin
  Removed := Route4MeManager.AddressBookContact.Remove(AddressIds, ErrorString);

  WriteLn('');

  if (Removed) then
    WriteLn(Format('RemoveLocations executed successfully, %d contacts deleted',
      [Length(AddressIds)]))
  else
    WriteLn(Format('RemoveLocations error: "%s"', [ErrorString]));
end;

end.
