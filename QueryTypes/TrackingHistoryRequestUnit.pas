unit TrackingHistoryRequestUnit;

interface

uses
  REST.Json.Types, HttpQueryMemberAttributeUnit, JSONNullableAttributeUnit,
  GenericParametersUnit, NullableBasicTypesUnit;

type
  TTrackingHistoryRequest = class(TGenericParameters)
  private
    [HttpQueryMember('route_id')]
    FRouteId: String;

    [HttpQueryMember('format')]
    [Nullable]
    FFormat: NullableString;

    [HttpQueryMember('last_position')]
    [Nullable]
    FLastPosition: NullableBoolean;

    [HttpQueryMember('time_period')]
    FTimePeriod: String;

    [HttpQueryMember('start_date')]
    [Nullable]
    FStartDate: NullableString;

    [HttpQueryMember('end_date')]
    [Nullable]
    FEndDate: NullableString;
  public
    constructor Create(RouteId: String); reintroduce;

    property RouteId: String read FRouteId write FRouteId;
    property Format: NullableString read FFormat write FFormat;
    property LastPosition: NullableBoolean read FLastPosition write FLastPosition;
    property TimePeriod: String read FTimePeriod write FTimePeriod;
    property StartDate: NullableString read FStartDate write FStartDate;
    property EndDate: NullableString read FEndDate write FEndDate;
  end;

implementation

{ TTrackingHistoryRequest }

constructor TTrackingHistoryRequest.Create(RouteId: String);
begin
  inherited Create;

  FRouteId := RouteId;
  FFormat := NullableString.Null;
  FLastPosition := NullableBoolean.Null;
  FStartDate := NullableString.Null;
  FEndDate := NullableString.Null;
end;

end.
