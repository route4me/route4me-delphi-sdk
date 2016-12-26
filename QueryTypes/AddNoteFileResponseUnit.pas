unit AddNoteFileResponseUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit, JSONNullableAttributeUnit, NullableBasicTypesUnit,
  AddressNoteUnit;

type
  TAddNoteFileResponse = class(TGenericParameters)
  private
    [JSONName('status')]
    FStatus: Boolean;

    [JSONName('note_id')]
    [Nullable]
    FNodeId: NullableInteger;

    [JSONName('upload_id')]
    [Nullable]
    FUploadId: NullableString;

    [JSONName('note')]
    [NullableObject(TAddressNote)]
    FNote: NullableObject;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Status: boolean read FStatus write FStatus;
    property NodeId: NullableInteger read FNodeId write FNodeId;
    property UploadId: NullableString read FUploadId write FUploadId;
    property Note: NullableObject read FNote write FNote;
  end;

implementation

{ TAddNoteFileResponse }

constructor TAddNoteFileResponse.Create;
begin
  inherited;

  FNodeId := NullableInteger.Null;
  FUploadId := NullableString.Null;
  FNote := NullableObject.Null;
end;

destructor TAddNoteFileResponse.Destroy;
begin
  FNote.Free;

  inherited;
end;

end.
