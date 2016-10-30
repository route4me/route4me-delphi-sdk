unit ActivityUnit;

interface

uses
  REST.Json.Types,
  JSONNullableAttributeUnit,
  GenericParametersUnit, NullableBasicTypesUnit, EnumsUnit;

type
  TActivity = class(TGenericParameters)
  private
    [JSONName('device_id')]
    [Nullable]
    FDeviceId: NullableString;

    [JSONName('activity_id')]
    [Nullable]
    FActivityId: NullableString;

    [JSONName('activity_type')]
    [Nullable]
    FActivityType: NullableString;

    [JSONName('offset')]
    [Nullable]
    FOffset: NullableInteger;

    [JSONName('team')]
    [Nullable]
    FTeam: NullableString;

    [JSONName('member_id')]
    [Nullable]
    FMemberId: NullableString;

    [JSONName('route_id')]
    [Nullable]
    FRouteId: NullableString;

    [JSONName('start')]
    [Nullable]
    FStartTime: NullableInteger;

    [JSONName('end')]
    [Nullable]
    FEndTime: NullableInteger;

    [JSONName('format')]
    [Nullable]
    FFormat: NullableString;

    function GetFormat: TFormatEnum;
    procedure SetFormat(const Value: TFormatEnum);
  public
    /// <remarks>
    ///  Constructor with 0-arguments must be and be public.
    ///  For JSON-deserialization.
    /// </remarks>
    constructor Create; override;

    function Equals(Obj: TObject): Boolean; override;

    /// <summary>
    /// Device ID
    /// </summary>
    property DeviceId: NullableString read FDeviceId write FDeviceId;

    /// <summary>
    /// Activity Id
    /// </summary>
    property ActivityId: NullableString read FActivityId write FActivityId;

    /// <summary>
    /// delete-destination - remove address from route;
    /// insert-destination - add new address;
    /// mark-destination-departed - mark address departed;
    /// move-destination - change address sequence;
    /// update-destinations - update address attribute;
    /// mark-destination-visited - mark address visited;
    /// member-created - created team member;
    /// member-deleted - delete team member;
    /// member-modified - modified member;
    /// note-insert - note insert;
    /// route-delete - delete route;
    /// route-optimized - optimization problem;
    /// route-owner-changed - assigned other user to route,
    /// user_message - user sent message
    /// </summary>
    property ActivityType: NullableString read FActivityType write FActivityType;

    /// <summary>
    /// Start position for searching
    /// </summary>
    property Offset: NullableInteger read FOffset write FOffset;

    /// <summary>
    /// 1 - include team
    /// </summary>
    property Team: NullableString read FTeam write FTeam;

    /// <summary>
    /// Member Id
    /// </summary>
    property MemberId: NullableString read FMemberId write FMemberId;

    /// <summary>
    /// Parent route
    /// </summary>
    property RouteId: NullableString read FRouteId write FRouteId;

    /// <summary>
    /// Start time
    /// </summary>
    property StartTime: NullableInteger read FStartTime write FStartTime;

    /// <summary>
    /// End time
    /// </summary>
    property EndTime: NullableInteger read FEndTime write FEndTime;

    /// <summary>
    /// Response format
    /// </summary>
    property Format: TFormatEnum read GetFormat write SetFormat;
  end;

  TActivityArray = TArray<TActivity>;

implementation

{ TActivity }

constructor TActivity.Create;
begin
  Inherited;

  FDeviceId := NullableString.Null;
  FActivityId := NullableString.Null;
  FActivityType := NullableString.Null;
  FOffset := NullableInteger.Null;
  FTeam := NullableString.Null;
  FMemberId := NullableString.Null;
  FRouteId := NullableString.Null;
  FStartTime := NullableInteger.Null;
  FEndTime := NullableInteger.Null;
  FFormat := NullableString.Null;
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
    (DeviceId = Other.DeviceId) and
    (ActivityId = Other.ActivityId) and
    (ActivityType = Other.ActivityType) and
    (Offset = Other.Offset) and
    (Team = Other.Team) and
    (MemberId = Other.MemberId) and
    (RouteId = Other.RouteId) and
    (StartTime = Other.StartTime) and
    (EndTime = Other.EndTime) and
    (Format = Other.Format);
end;

function TActivity.GetFormat: TFormatEnum;
var
  Format: TFormatEnum;
begin
  if FFormat.IsNull then
    Exit(TFormatEnum.UndefinedFormat);

  for Format := Low(TFormatEnum) to High(TFormatEnum) do
    if (FFormat = TFormatDescription[Format]) then
      Exit(Format);
end;

procedure TActivity.SetFormat(const Value: TFormatEnum);
begin
  FFormat := TFormatDescription[Value];
end;

end.
