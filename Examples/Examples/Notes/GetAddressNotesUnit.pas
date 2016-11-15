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
  Notes: TAddressNoteArray;
  i: integer;
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
          [Length(Notes)]))
      else
        WriteLn(Format('GetAddressNotes error: "%s"', [ErrorString]));

      WriteLn('');
    finally
      for i := Length(Notes) - 1 downto 0 do
        FreeAndNil(Notes[i]);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
