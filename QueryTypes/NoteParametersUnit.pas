unit NoteParametersUnit;

interface

uses
  NullableBasicTypesUnit,
  GenericParametersUnit, HttpQueryMemberAttributeUnit, JSONNullableAttributeUnit;

type
  TNoteParameters = class(TGenericParameters)
  private
    [HttpQueryMember('route_id')]
    [Nullable]
    FRouteId: NullableString;

    [HttpQueryMember('address_id')]
    [Nullable]
    FAddressId: NullableInteger;

    [HttpQueryMember('dev_lat')]
    [Nullable]
    FLatitude: NullableDouble;

    [HttpQueryMember('dev_lng')]
    [Nullable]
    FLongitude: NullableDouble;

    [HttpQueryMember('device_type')]
    [Nullable]
    FDeviceType: NullableString;

    [HttpQueryMember('strUpdateType')]
    [Nullable]
    FActivityType: NullableString;
  public
    constructor Create; override;

    property RouteId: NullableString read FRouteId write FRouteId;
    property AddressId: NullableInteger read FAddressId write FAddressId;
    property Latitude: NullableDouble read FLatitude write FLatitude;
    property Longitude: NullableDouble read FLongitude write FLongitude;
    property DeviceType: NullableString read FDeviceType write FDeviceType;
    property ActivityType: NullableString read FActivityType write FActivityType;
  end;

implementation

{ TNoteParameters }

constructor TNoteParameters.Create;
begin
  Inherited Create;

  FRouteId := NullableString.Null;
  FAddressId := NullableInteger.Null;
  FLatitude := NullableDouble.Null;
  FLongitude := NullableDouble.Null;
  FDeviceType := NullableString.Null;
  FActivityType := NullableString.Null;
end;

end.
