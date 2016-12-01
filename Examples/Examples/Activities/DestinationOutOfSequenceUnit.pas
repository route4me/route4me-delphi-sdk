unit DestinationOutOfSequenceUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TDestinationOutOfSequence = class(TBaseExample)
  public
    procedure Execute;
  end;

implementation

procedure TDestinationOutOfSequence.Execute;
var
  ErrorString: String;
begin
  Route4MeManager.ActivityFeed.DestinationOutOfSequence(ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
    WriteLn('DestinationOutOfSequence executed successfully')
  else
    WriteLn(Format('DestinationOutOfSequence error: "%s"', [ErrorString]));
end;

end.
