unit GetActivitiesQueryUnit;

interface

uses
  HttpQueryMemberAttributeUnit, JSONNullableAttributeUnit,
  NullableBasicTypesUnit, GenericParametersUnit, EnumsUnit;

type
  TGetActivitiesQuery = class(TGenericParameters)
  private
    [HttpQueryMember('route_id')]
    [Nullable]
    FRouteId: NullableString;

    [HttpQueryMember('limit')]
    [Nullable]
    FLimit: NullableInteger;

    [HttpQueryMember('offset')]
    [Nullable]
    FOffset: NullableInteger;

    [HttpQueryMember('activity_type')]
    FActivityType: String;
  public
    constructor Create; overload; override;
    constructor Create(ActivityType: TActivityType); reintroduce; overload;
    constructor Create(ActivityType: TActivityType; Limit, Offset: integer); reintroduce; overload;
    constructor Create(ActivityType: TActivityType; RouteId: String; Limit, Offset: integer); reintroduce; overload;
  end;

implementation

constructor TGetActivitiesQuery.Create;
begin
  Inherited Create;

  FRouteId := NullableString.Null;
  FLimit := NullableInteger.Null;
  FOffset := NullableInteger.Null;
  FActivityType := '';
end;

constructor TGetActivitiesQuery.Create(ActivityType: TActivityType);
begin
  Create();

  FActivityType := TActivityTypeDescription[ActivityType];
end;

constructor TGetActivitiesQuery.Create(ActivityType: TActivityType;
  Limit, Offset: integer);
begin
  Create(ActivityType);

  FLimit := Limit;
  FOffset := Offset;
end;

constructor TGetActivitiesQuery.Create(ActivityType: TActivityType;
  RouteId: String; Limit, Offset: integer);
begin
  Create(ActivityType, Limit, Offset);

  FRouteId := RouteId;
end;

end.
