unit ActivityUnit;

interface

uses
  REST.Json.Types,
  JSONNullableAttributeUnit, GenericParametersUnit, NullableBasicTypesUnit;

type
  /// <summary>
  ///  Activity
  /// </summary>
  /// <remarks>
  ///  https://github.com/route4me/json-schemas/blob/master/Activity.dtd
  /// </remarks>
  TActivity = class(TGenericParameters)
  private
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
  public
    /// <remarks>
    ///  Constructor with 0-arguments must be and be public.
    ///  For JSON-deserialization.
    /// </remarks>
    constructor Create; override;

    function Equals(Obj: TObject): Boolean; override;

    /// <summary>
    /// Route ID
    /// </summary>
    property RouteId: NullableString read FRouteId write FRouteId;

    /// <summary>
    /// Route destination ID
    /// </summary>
    property RouteDestinationId: NullableInteger read FRouteDestinationId write FRouteDestinationId;

    /// <summary>
    /// Member Id
    /// </summary>
    property MemberId: NullableInteger read FMemberId write FMemberId;

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
    /// Activity message
    /// </summary>
    property ActivityMessage: NullableString read FActivityMessage write FActivityMessage;
  end;

  TActivityArray = TArray<TActivity>;

implementation

{ TActivity }

constructor TActivity.Create;
begin
  Inherited;

  FRouteId := NullableString.Null;
  FRouteDestinationId := NullableInteger.Null;
  FActivityType := NullableString.Null;
  FMemberId := NullableInteger.Null;
  FActivityMessage := NullableString.Null;
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
    (FRouteId = Other.FRouteId) and
    (FRouteDestinationId = Other.FRouteDestinationId) and
    (FActivityType = Other.FActivityType) and
    (FMemberId = Other.FMemberId) and
    (FActivityMessage = Other.FActivityMessage);
end;

end.
