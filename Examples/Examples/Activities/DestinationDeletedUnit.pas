unit DestinationDeletedUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TDestinationDeleted = class(TBaseExample)
  public
    procedure Execute;
  end;

implementation

procedure TDestinationDeleted.Execute;
var
  ErrorString: String;
begin
  Route4MeManager.ActivityFeed.DestinationDeleted(ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
    WriteLn('DestinationDeleted executed successfully')
  else
    WriteLn(Format('DestinationDeleted error: "%s"', [ErrorString]));
end;

end.
