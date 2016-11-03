unit ActivityParametersUnit;

interface

uses
  HttpQueryMemberAttributeUnit, JSONNullableAttributeUnit,
  NullableBasicTypesUnit, GenericParametersUnit;

type
  TActivityParameters = class(TGenericParameters)
  private
    [HttpQueryMember('route_id')]
    [Nullable]
    FRouteId: NullableString;

    [HttpQueryMember('device_id')]
    [Nullable]
    FDeviceID: NullableString;

    [HttpQueryMember('member_id')]
    [Nullable]
    FMemberId: NullableInteger;

    [HttpQueryMember('limit')]
    [Nullable]
    FLimit: NullableInteger;

    [HttpQueryMember('offset')]
    [Nullable]
    FOffset: NullableInteger;

    [HttpQueryMember('start')]
    [Nullable]
    FStart: NullableInteger;

    [HttpQueryMember('end')]
    [Nullable]
    FEnd: NullableInteger;
  public
    constructor Create; override;

    property RouteId: NullableString read FRouteId write FRouteId;
    property DeviceID: NullableString read FDeviceID write FDeviceID;
    property MemberId: NullableInteger read FMemberId write FMemberId;
    property Limit: NullableInteger read FLimit write FLimit;
    property Offset: NullableInteger read FOffset write FOffset;
    property Start: NullableInteger read FStart write FStart;
    property End_: NullableInteger read FEnd write FEnd;
  end;

implementation

{ TActivityParameters }

constructor TActivityParameters.Create;
begin
  Inherited Create;

  RouteId := NullableString.Null;
  DeviceID := NullableString.Null;
  MemberId := NullableInteger.Null;
  Limit := NullableInteger.Null;
  Offset := NullableInteger.Null;
  Start := NullableInteger.Null;
  End_ := NullableInteger.Null;
end;

end.
