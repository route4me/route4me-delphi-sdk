unit AddAddressNoteUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TAddAddressNote = class(TBaseExample)
  public
    procedure Execute(RouteId: String; AddressId: integer);
  end;

implementation

uses NoteParametersUnit, AddressNoteUnit, EnumsUnit;

procedure TAddAddressNote.Execute(RouteId: String; AddressId: integer);
var
  ErrorString: String;
  Parameters: TNoteParameters;
  Contents: String;
  Note: TAddressNote;
begin
  Parameters := TNoteParameters.Create();
  try
    Parameters.RouteId := RouteId;
    Parameters.AddressId := AddressId;
    Parameters.Latitude := 33.132675170898;
    Parameters.Longitude := -83.244743347168;
    Parameters.DeviceType := TDeviceType.Web;
    Parameters.ActivityType := TStatusUpdateType.DropOff;

    Contents := 'Test Note Contents ' + DateTimeToStr(Now);
    Note := Route4MeManager.AddressNote.Add(Parameters, Contents, ErrorString);
    try
      WriteLn('');

      if (Note <> nil) then
      begin
        WriteLn('AddAddressNote executed successfully');
        WriteLn(Format('Note ID: %d', [Note.NoteId.Value]));
      end
      else
        WriteLn(Format('AddAddressNote error: "%s"', [ErrorString]));
    finally
      FreeAndNil(Note);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
