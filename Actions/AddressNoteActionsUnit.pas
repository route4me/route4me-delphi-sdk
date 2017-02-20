unit AddressNoteActionsUnit;

interface

uses
  SysUtils, Classes, BaseActionUnit,
  DataObjectUnit, NoteParametersUnit, AddressNoteUnit, IConnectionUnit,
  AddNoteFileResponseUnit, EnumsUnit;

type
  TAddressNoteActions = class(TBaseAction)
  private
    FRoute4MeManager: IUnknown; //IRoute4MeManager;
  public
    /// <remarks>
    ///  Route4MeManager it must be IRoute4MeManager
    /// <remarks>
    constructor Create(Connection: IConnection; Route4MeManager: IUnknown);
    destructor Destroy; override;

    function Get(NoteParameters: TNoteParameters;
      out ErrorString: String): TAddressNoteList;

    function Add(NoteParameters: TNoteParameters; NoteContents: String;
      out ErrorString: String): TAddressNote;

    function AddFile(RouteId: String; AddressId: integer;
      Latitude, Longitude: double; DeviceType: TDeviceType;
      ActivityType: TStatusUpdateType; Filename: String;
      out ErrorString: String): TAddressNote;
  end;

implementation

{ TAddressNoteActions }

uses
  System.Generics.Collections, System.NetEncoding,
  SettingsUnit, IRoute4MeManagerUnit,
  GenericParametersUnit, CommonTypesUnit, AddressParametersUnit, AddressUnit,
  AddAddressNoteResponseUnit;

function TAddressNoteActions.Add(NoteParameters: TNoteParameters;
  NoteContents: String; out ErrorString: String): TAddressNote;
var
  Response: TAddAddressNoteResponse;
  Parameters: TGenericParameters;
  NoteParameterPairs: TListStringPair;
  i: integer;
begin
  Result := nil;

  Parameters := TGenericParameters.Create;
  try
    Parameters.AddBodyParameter('strUpdateType', TStatusUpdateTypeDescription[NoteParameters.ActivityType]);
    Parameters.AddBodyParameter('strNoteContents', TNetEncoding.URL.Encode(NoteContents));
    NoteParameterPairs := NoteParameters.Serialize('');
    try
      for i := 0 to NoteParameterPairs.Count - 1 do
        Parameters.AddParameter(NoteParameterPairs[i].Key, NoteParameterPairs[i].Value);
    finally
      FreeAndNil(NoteParameterPairs);
    end;

    Response := FConnection.Post(TSettings.EndPoints.AddRouteNotes, Parameters,
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

function TAddressNoteActions.AddFile(RouteId: String; AddressId: integer;
  Latitude, Longitude: double; DeviceType: TDeviceType;
  ActivityType: TStatusUpdateType; Filename: String;
  out ErrorString: String): TAddressNote;
var
  Response: TAddAddressNoteResponse;
  Parameters: TGenericParameters;
  NoteParameterPairs: TListStringPair;
  i: integer;
  NoteParameters: TNoteParameters;
  FileStream: TStream;
begin
  Result := nil;
  if not FileExists(Filename) then
  begin
    ErrorString := Format('File "%s" not existed', [Filename]);
    Exit;
  end;

  NoteParameters := TNoteParameters.Create;
  try
    NoteParameters.RouteId := RouteId;
    NoteParameters.AddressId := AddressId;
    NoteParameters.Latitude := Latitude;
    NoteParameters.Longitude := Longitude;
    NoteParameters.DeviceType := DeviceType;
    NoteParameters.ActivityType := ActivityType;

    Parameters := TGenericParameters.Create;
    try
      NoteParameterPairs := NoteParameters.Serialize('');
      try
        for i := 0 to NoteParameterPairs.Count - 1 do
          Parameters.AddParameter(NoteParameterPairs[i].Key, NoteParameterPairs[i].Value);
      finally
        FreeAndNil(NoteParameterPairs);
      end;

      FileStream := TFileStream.Create(Filename, fmOpenRead);
      try
        // todo 1: сделать Add a Note File
        Response := FConnection.Post(TSettings.EndPoints.AddRouteNotes, Parameters,
          FileStream, TAddAddressNoteResponse, ErrorString) as TAddAddressNoteResponse;
        try
          if (Response <> nil) then
            if (Response.Note <> nil) then
              Result := Response.Note
            else
              if (Response.Status = False) then
                ErrorString := 'Note file not added';
        finally
          FreeAndNil(Response);
        end;
      finally
        FreeAndNil(FileStream);
      end;
    finally
      FreeAndNil(Parameters);
    end;
  finally
    FreeAndNil(NoteParameters);
  end;
end;

constructor TAddressNoteActions.Create(Connection: IConnection;
  Route4MeManager: IUnknown);
begin
  Inherited Create(Connection);
  FRoute4MeManager := Route4MeManager;
end;

destructor TAddressNoteActions.Destroy;
begin
  FRoute4MeManager := nil;

  inherited;
end;

function TAddressNoteActions.Get(NoteParameters: TNoteParameters;
  out ErrorString: String): TAddressNoteList;
var
  AddressParameters: TAddressParameters;
  Address: TAddress;
  i: integer;
begin
  Result := TAddressNoteList.Create;

  AddressParameters := TAddressParameters.Create();
  try
    AddressParameters.RouteId := NoteParameters.RouteId;
    AddressParameters.RouteDestinationId := NoteParameters.AddressId;
    AddressParameters.Notes := True;

    Address := (FRoute4MeManager as IRoute4MeManager).Address.Get(AddressParameters, ErrorString);
    try
      if (Address <> nil) then
      begin
        Address.OwnsNotes := False;
        for i := 0 to High(Address.Notes) do
          Result.Add(Address.Notes[i]);
      end;
    finally
      FreeAndNil(Address);
    end;
  finally
    FreeAndNil(AddressParameters);
  end;
end;

end.
