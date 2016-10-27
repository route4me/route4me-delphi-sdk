unit RouteParametersQueryUnit;

interface

uses
  REST.Json.Types, HttpQueryMemberAttributeUnit, JSONNullableAttributeUnit,
  GenericParametersUnit, NullableBasicTypesUnit, RouteParametersUnit;

type
  /// <summary>
  /// Route parameters accepted by endpoints
  /// </summary>
  TRouteParametersQuery = class(TGenericParameters)
  private
    [JSONMarshalled(False)]
    [Nullable]
    [HttpQueryMember('route_id')]
    FRouteId: NullableString;

    [JSONMarshalled(False)]
    [Nullable]
    [HttpQueryMember('directions')]
    FDirections: NullableBoolean;

    [JSONMarshalled(False)]
    [Nullable]
    [HttpQueryMember('route_path_output')]
    FRoutePathOutput: NullableString;

    [JSONMarshalled(False)]
    [Nullable]
    [HttpQueryMember('device_tracking_history')]
    FDeviceTrackingHistory: NullableBoolean;

    [JSONMarshalled(False)]
    [Nullable]
    [HttpQueryMember('limit')]
    FLimit: NullableInteger;

    [JSONMarshalled(False)]
    [Nullable]
    [HttpQueryMember('offset')]
    FOffset: NullableInteger;

    [JSONMarshalled(False)]
    [Nullable]
    [HttpQueryMember('original')]
    FOriginal: NullableBoolean;

    [JSONMarshalled(False)]
    [Nullable]
    [HttpQueryMember('notes')]
    FNotes: NullableBoolean;

    [JSONMarshalled(False)]
    [Nullable]
    [HttpQueryMember('query')]
    FQuery: NullableString;

    [JSONMarshalled(False)]
    [Nullable]
    [HttpQueryMember('reoptimize')]
    FReOptimize: NullableBoolean;

    [JSONMarshalled(False)]
    [Nullable]
    [HttpQueryMember('recompute_directions')]
    FRecomputeDirections: NullableBoolean;

    [JSONName('parameters')]
    [Nullable]
    FParameters: NullableObject; //TRouteParameters;
  public
    /// <remarks>
    ///  Constructor with 0-arguments must be and be public.
    ///  For JSON-deserialization.
    /// </remarks>
    constructor Create; overload; override;

    /// <summary>
    /// Route Identifier
    /// </summary>
    property RouteId: NullableString read FRouteId write FRouteId;

    /// <summary>
    /// Pass True to return directions and the route path
    /// </summary>
    property Directions: NullableBoolean read FDirections write FDirections;

    /// <summary>
    /// "None" - no path output. "Points" - points path output
    /// </summary>
    property RoutePathOutput: NullableString read FRoutePathOutput write FRoutePathOutput;

    /// <summary>
    /// Output route tracking data in response
    /// </summary>
    property DeviceTrackingHistory: NullableBoolean read FDeviceTrackingHistory write FDeviceTrackingHistory;

    /// <summary>
    /// The number of existing routes that should be returned per response when looking at a list of all the routes.
    /// </summary>
    property Limit: NullableInteger read FLimit write FLimit;

    /// <summary>
    /// The page number for route listing pagination.
    /// Increment the offset by the limit number to move to the next page.
    /// </summary>
    property Offset: NullableInteger read FOffset write FOffset;

    /// <summary>
    /// Output addresses and directions in the original optimization request sequence.
    /// This is to allow us to compare routes before & after optimization.
    /// </summary>
    property Original: NullableBoolean read FOriginal write FOriginal;

    /// <summary>
    /// Output route and stop-specific notes.
    /// The notes will have timestamps, note types, and geospatial information if available
    /// </summary>
    property Notes: NullableBoolean read FNotes write FNotes;

    /// <summary>
    /// Search query
    /// </summary>
    property Query: NullableString read FQuery write FQuery;

    /// <summary>
    /// Updating a route supports the reoptimize=1 parameter, which reoptimizes only that route.
    /// Also supports the parameters from GET.
    /// </summary>
    property ReOptimize: NullableBoolean read FReOptimize write FReOptimize;

    /// <summary>
    /// By sending recompute_directions=1 we request that the route directions be recomputed
    /// (note that this does happen automatically if certain properties of the route are updated,
    /// such as stop sequence_no changes or round-tripness)
    /// </summary>
    property RecomputeDirections: NullableBoolean read FRecomputeDirections write FRecomputeDirections;

    /// <summary>
    /// Route Parameters to update.
    /// (After a PUT there is no guarantee that the route_destination_id values are preserved!
    /// It may create copies resulting in new destination IDs, especially when dealing with multiple depots.)
    /// </summary>
    [JSONName('parameters')]
    property Parameters: NullableObject {TRouteParameters} read FParameters write FParameters;
  end;

implementation

{ TRouteParametersQuery }

constructor TRouteParametersQuery.Create;
begin
  Inherited Create;

  FRouteId := NullableString.Null;
  FDirections := NullableBoolean.Null;
  FRoutePathOutput := NullableString.Null;
  FDeviceTrackingHistory := NullableBoolean.Null;
  FLimit := NullableInteger.Null;
  FOffset := NullableInteger.Null;
  FOriginal := NullableBoolean.Null;
  FNotes := NullableBoolean.Null;
  FQuery := NullableString.Null;
  FReOptimize := NullableBoolean.Null;
  FRecomputeDirections := NullableBoolean.Null;
  FParameters := NullableObject.Null;
end;

end.
