unit AddressNoteActionUnit;

interface

uses
  SysUtils, BaseActionUnit,
  DataObjectUnit, NoteParametersUnit, AddressNoteUnit, IConnectionUnit;

type
  TAddressNoteActions = class(TBaseAction)
  private
    FRoute4MeManager: IUnknown; //IRoute4MeManager;
  public
    constructor Create(Connection: IConnection; Route4MeManager: IUnknown{IRoute4MeManager});

    function Get(NoteParameters: TNoteParameters;
      out ErrorString: String): TAddressNoteArray;
    function Add(NoteParameters: TNoteParameters; NoteContents: String;
      out ErrorString: String): TAddressNote;
  end;

implementation

{ TAddressNoteActions }

uses
  System.Generics.Collections,
  SettingsUnit, IRoute4MeManagerUnit,
  GenericParametersUnit, CommonTypesUnit, AddressParametersUnit, AddressUnit,
  AddAddressNoteResponseUnit;

function TAddressNoteActions.Add(NoteParameters: TNoteParameters;
  NoteContents: String; out ErrorString: String): TAddressNote;
var
  Response: TAddAddressNoteResponse;
  Parameters: TGenericParameters;
  StrUpdateType: String;
begin
  Result := nil;

  Parameters := TGenericParameters.Create;
  try
    if (NoteParameters.ActivityType.IsNotNull) then
      StrUpdateType := NoteParameters.ActivityType
    else
      StrUpdateType := 'unclassified';
    Parameters.AddParameter('strUpdateType', StrUpdateType);
    Parameters.AddParameter('strNoteContents', NoteContents);

    Response := FConnection.Post(TSettings.AddRouteNotesHost, Parameters,
      TAddAddressNoteResponse, ErrorString) as TAddAddressNoteResponse;
    try
      if (Response <> nil) then
        if (Response.Note <> nil) then
          Result := Response.Note
        else
          if (Response.Status = False) then
            ErrorString := 'Note not added';
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

constructor TAddressNoteActions.Create(Connection: IConnection;
  Route4MeManager: IUnknown{IRoute4MeManager});
begin
  Inherited Create(Connection);
  FRoute4MeManager := Route4MeManager;
end;

function TAddressNoteActions.Get(NoteParameters: TNoteParameters;
  out ErrorString: String): TAddressNoteArray;
var
  AddressParameters: TAddressParameters;
  Address: TAddress;
begin
  Result := nil;

  AddressParameters := TAddressParameters.Create();
  try
    AddressParameters.RouteId := NoteParameters.RouteId;
    AddressParameters.RouteDestinationId := NoteParameters.AddressId;
    AddressParameters.Notes := True;

    Address := (FRoute4MeManager as IRoute4MeManager).Address.Get(AddressParameters, ErrorString);
    try
      if (Address <> nil) then
        Result := Address.Notes.ToArray;
    finally
      FreeAndNil(Address);
    end;
  finally
    FreeAndNil(AddressParameters);
  end;
end;

end.
