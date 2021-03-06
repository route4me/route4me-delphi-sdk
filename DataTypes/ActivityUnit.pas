unit ActivityUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections,
  JSONNullableAttributeUnit, GenericParametersUnit, NullableBasicTypesUnit,
  EnumsUnit;

type
  /// <summary>
  ///  Activity
  /// </summary>
  /// <remarks>
  ///  https://github.com/route4me/json-schemas/blob/master/Activity_response.dtd
  /// </remarks>
  TActivity = class(TGenericParameters)
  private
    [JSONName('activity_id')]
    [Nullable]
    FActivityId: NullableString;

    [JSONName('route_id')]
    [Nullable]
    FRouteId: NullableString;

    [JSONName('route_destination_id')]
    [Nullable]
    FRouteDestinationId: NullableInteger;

    [JSONName('activity_type')]
    [Nullable]
    FActivityType: NullableString;

    [JSONName('member_id')]
    [Nullable]
    FMemberId: NullableInteger;

    [JSONName('activity_message')]
    [Nullable]
    FActivityMessage: NullableString;

    [JSONName('note_id')]
    [Nullable]
    FNoteId: NullableString;

    [JSONName('note_attachment_url')]
    [Nullable]
    FNoteAttachmentUrl: NullableString;

    [JSONName('activity_timestamp')]
    [Nullable]
    FActivityTimestamp: NullableInteger;

    function GetActivityType: TActivityType;
    procedure SetActivityType(const Value: TActivityType);
  public
    /// <remarks>
    ///  Constructor with 0-arguments must be and be public.
    ///  For JSON-deserialization.
    /// </remarks>
    constructor Create; override;

    function Equals(Obj: TObject): Boolean; override;

    /// <summary>
    /// Activity ID
    /// </summary>
    property Id: NullableString read FActivityId write FActivityId;

    /// <summary>
    /// Parent route
    /// </summary>
    property RouteId: NullableString read FRouteId write FRouteId;

    /// <summary>
    /// An address on the route
    /// </summary>
    property RouteDestinationId: NullableInteger read FRouteDestinationId write FRouteDestinationId;

    /// <summary>
    /// Route's owner member's unique ID number
    /// </summary>
    property MemberId: NullableInteger read FMemberId write FMemberId;

    /// <summary>
    ///  Activity type
    /// </summary>
    property ActivityType: TActivityType read GetActivityType write SetActivityType;

    /// <summary>
    /// Activity message
    /// </summary>
    property ActivityMessage: NullableString read FActivityMessage write FActivityMessage;

    /// <summary>
    /// Note ID
    /// </summary>
    property NoteId: NullableString read FNoteId write FNoteId;

    /// <summary>
    /// URL of the uploaded note
    /// </summary>
    property NoteAttachmentUrl: NullableString read FNoteAttachmentUrl write FNoteAttachmentUrl;

    /// <summary>
    /// Time when activity happened
    /// </summary>
    property ActivityTimestamp: NullableInteger read FActivityTimestamp write FActivityTimestamp;
  end;

  TActivityArray = TArray<TActivity>;
  TActivityList = TObjectList<TActivity>;

implementation

{ TActivity }

constructor TActivity.Create;
begin
  Inherited;

  FActivityId := NullableString.Null;
  FRouteId := NullableString.Null;
  FRouteDestinationId := NullableInteger.Null;
  FActivityType := NullableString.Null;
  FMemberId := NullableInteger.Null;
  FActivityMessage := NullableString.Null;
  FNoteId := NullableString.Null;
  FNoteAttachmentUrl := NullableString.Null;
  FActivityTimestamp := NullableInteger.Null;
end;

function TActivity.Equals(Obj: TObject): Boolean;
var
  Other: TActivity;
begin
  Result := False;

  if not (Obj is TActivity) then
    Exit;

  Other := TActivity(Obj);

  Result :=
    (FActivityId = Other.FActivityId) and
    (FRouteId = Other.FRouteId) and
    (FRouteDestinationId = Other.FRouteDestinationId) and
    (FActivityType = Other.FActivityType) and
    (FMemberId = Other.FMemberId) and
    (FNoteId = Other.FNoteId) and
    (FNoteAttachmentUrl = Other.FNoteAttachmentUrl) and
    (FActivityTimestamp = Other.FActivityTimestamp) and
    (FActivityMessage = Other.FActivityMessage);
end;

function TActivity.GetActivityType: TActivityType;
var
  ActivityType: TActivityType;
begin
  Result := TActivityType.atUnknown;
  if FActivityType.IsNotNull then
    for ActivityType := Low(TActivityType) to High(TActivityType) do
      if (FActivityType = TActivityTypeDescription[ActivityType]) then
        Exit(ActivityType);
end;

procedure TActivity.SetActivityType(const Value: TActivityType);
begin
  FActivityType := TActivityTypeDescription[Value];
end;

end.
