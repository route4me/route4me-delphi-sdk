unit GetAddressNotesUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetAddressNotes = class(TBaseExample)
  public
    procedure Execute(RouteId: String; RouteDestinationId: integer);
  end;

implementation

uses NoteParametersUnit, AddressNoteUnit;

procedure TGetAddressNotes.Execute(RouteId: String; RouteDestinationId: integer);
var
  ErrorString: String;
  Parameters: TNoteParameters;
  Notes: TAddressNoteList;
begin
  Parameters := TNoteParameters.Create();
  try
    Parameters.RouteId := RouteId;
    Parameters.AddressId := RouteDestinationId;

    Notes := Route4MeManager.AddressNote.Get(Parameters, ErrorString);
    try
      WriteLn('');

      if (Notes <> nil) then
        WriteLn(Format('GetAddressNotes executed successfully, %d notes returned',
          [Notes.Count]))
      else
        WriteLn(Format('GetAddressNotes error: "%s"', [ErrorString]));

      WriteLn('');
    finally
      FreeAndNil(Notes);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
