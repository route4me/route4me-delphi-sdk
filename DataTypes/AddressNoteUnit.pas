unit AddressNoteUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections, Classes, SysUtils,
  Generics.Defaults, JSONNullableAttributeUnit,
  NullableBasicTypesUnit, GenericParametersUnit,
  EnumsUnit;

type

  TAddressNote = class(TGenericParameters)
  private
    [JSONName('note_id')]
    [Nullable]
    FNoteId: NullableInteger;

    [JSONName('route_id')]
    [Nullable]
    FRouteId: NullableString;

    [JSONName('route_destination_id')]
    FRouteDestinationId: Integer;

    [JSONName('upload_id')]
    FUploadId: NullableString;

    [JSONName('ts_added')]
    [Nullable]
    FTimestampAdded: NullableInteger;

    [JSONName('lat')]
    [Nullable]
    FLatitude: NullableDouble;

    [JSONName('lng')]
    [Nullable]
    FLongitude: NullableDouble;

    [JSONName('activity_type')]
    [Nullable]
    FActivityType: NullableString;

    [JSONName('contents')]
    [Nullable]
    FContents: NullableString;

    [JSONName('upload_type')]
    [Nullable]
    FUploadType: NullableString;

    [JSONName('upload_url')]
    [Nullable]
    FUploadUrl: NullableString;

    [JSONName('upload_extension')]
    [Nullable]
    FUploadExtension: NullableString;

    [JSONName('device_type')]
    [Nullable]
    FDeviceType: NullableString;

    function GetDeviceType: TDeviceType;
    procedure SetDeviceType(const Value: TDeviceType);
    function GetUploadType: TUploadType;
    procedure SetUploadType(const Value: TUploadType);
  public
    /// <remarks>
    ///  Constructor with 0-arguments must be and be public.
    ///  For JSON-deserialization.
    /// </remarks>
    constructor Create; override;

    function Equals(Obj: TObject): Boolean; override;

    /// <summary>
    ///  The id of the note in the system
    /// </summary>
    property NoteId: NullableInteger read FNoteId write FNoteId;

    /// <summary>
    ///  Route ID
    /// </summary>
    property RouteId: NullableString read FRouteId write FRouteId;

    /// <summary>
    ///  Route Destination ID
    /// </summary>
    property RouteDestinationId: Integer read FRouteDestinationId write FRouteDestinationId;

    /// <summary>
    ///  The unique and randomly generated ID of the file attachment that is associated with this note
    /// </summary>
    property UploadId: NullableString read FUploadId write FUploadId;

    /// <summary>
    ///  The unix timestamp when the note was added
    /// </summary>
    property TimestampAdded: NullableInteger read FTimestampAdded write FTimestampAdded;

    /// <summary>
    ///  Latitude
    /// </summary>
    property Latitude: NullableDouble read FLatitude write FLatitude;

    /// <summary>
    ///  Longitude
    /// </summary>
    property Longitude: NullableDouble read FLongitude write FLongitude;

    /// <summary>
    ///  Activity Type
    /// </summary>
    property ActivityType: NullableString read FActivityType write FActivityType;

    /// <summary>
    ///  Contents
    /// </summary>
    property Contents: NullableString read FContents write FContents;

    /// <summary>
    ///  Upload Type
    /// </summary>
    property UploadType: TUploadType read GetUploadType write SetUploadType;

    /// <summary>
    ///  The direct CDN URL of the attachment uploaded with a note
    /// </summary>
    property UploadUrl: NullableString read FUploadUrl write FUploadUrl;

    /// <summary>
    ///  The extension of the attachment ('pdf', 'csv' etc)
    /// </summary>
    property UploadExtension: NullableString read FUploadExtension write FUploadExtension;

    /// <summary>
    ///  Device type ('web', 'phone', 'ipad', 'android phone', 'android tablet' etc)
    /// </summary>
    property DeviceType: TDeviceType read GetDeviceType write SetDeviceType;
  end;

  TAddressNoteArray = TArray<TAddressNote>;
  TAddressNoteList = TList<TAddressNote>;
//  TAddressNoteListClass = class(TAddressNoteList);

function SortAddressNotes(AddressNotes: TAddressNoteArray): TAddressNoteArray;

implementation

function SortAddressNotes(AddressNotes: TAddressNoteArray): TAddressNoteArray;
begin
  SetLength(Result, Length(AddressNotes));
  TArray.Copy<TAddressNote>(AddressNotes, Result, Length(AddressNotes));
  TArray.Sort<TAddressNote>(Result, TComparer<TAddressNote>.Construct(
    function (const AddressNote1, AddressNote2: TAddressNote): Integer
    begin
      Result := AddressNote1.NoteId.Compare(AddressNote2.NoteId);
    end));
end;

{ TAddressNote }

constructor TAddressNote.Create;
begin
  Inherited Create;

  FRouteDestinationId := 0;

  FLatitude := NullableDouble.Null;
  FLongitude := NullableDouble.Null;
  FNoteId := NullableInteger.Null;
  FRouteId := NullableString.Null;
  FUploadId := NullableString.Null;
  FTimestampAdded := NullableInteger.Null;
  FActivityType := NullableString.Null;
  FContents := NullableString.Null;
  FUploadType := NullableString.Null;
  FUploadUrl := NullableString.Null;
  FUploadExtension := NullableString.Null;
  FDeviceType := NullableString.Null;
end;

function TAddressNote.Equals(Obj: TObject): Boolean;
var
  Other: TAddressNote;
begin
  Result := False;

  if not (Obj is TAddressNote) then
    Exit;

  Other := TAddressNote(Obj);

  Result :=
    (FNoteId = Other.FNoteId) and
    (FRouteId = Other.FRouteId) and
    (FRouteDestinationId = Other.FRouteDestinationId) and
    (FUploadId = Other.FUploadId) and
    (FTimestampAdded = Other.FTimestampAdded) and
    (FLatitude = Other.FLatitude) and
    (FLongitude = Other.FLongitude) and
    (FActivityType = Other.FActivityType) and
    (FContents = Other.FContents) and
    (FUploadType = Other.FUploadType) and
    (FUploadUrl = Other.FUploadUrl) and
    (FUploadExtension = Other.FUploadExtension) and
    (FDeviceType = Other.FDeviceType);
end;

function TAddressNote.GetDeviceType: TDeviceType;
var
  DeviceType: TDeviceType;
begin
  if FDeviceType.IsNull then
    Exit(TDeviceType.UnknownDevice);

  for DeviceType := Low(TDeviceType) to High(TDeviceType) do
    if (FDeviceType = TDeviceTypeDescription[DeviceType]) then
      Exit(DeviceType);
end;

function TAddressNote.GetUploadType: TUploadType;
var
  UploadType: TUploadType;
begin
  if FUploadType.IsNull then
    Exit(TUploadType.UnknownUploadType);

  for UploadType := Low(TUploadType) to High(TUploadType) do
    if (FUploadType = TUploadTypeDescription[UploadType]) then
      Exit(UploadType);
end;

procedure TAddressNote.SetDeviceType(const Value: TDeviceType);
begin
  FDeviceType := TDeviceTypeDescription[Value];
end;

procedure TAddressNote.SetUploadType(const Value: TUploadType);
begin
  FUploadType := TUploadTypeDescription[Value];
end;

end.
