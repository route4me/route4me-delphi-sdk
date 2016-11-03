unit GPSParametersUnit;

interface

uses
  HttpQueryMemberAttributeUnit, JSONNullableAttributeUnit,
  NullableBasicTypesUnit, GenericParametersUnit;

type
  TGPSParameters = class(TGenericParameters)
  private
    [HttpQueryMember('format')]
    [Nullable]
    FFormat: NullableString;

    [HttpQueryMember('member_id')]
    [Nullable]
    FMemberId: NullableInteger;

    [HttpQueryMember('route_id')]
    [Nullable]
    FRouteId: NullableString;

    [HttpQueryMember('tx_id')]
    [Nullable]
    FTxId: NullableString;

    [HttpQueryMember('vehicle_id')]
    [Nullable]
    FVehicleId: NullableInteger;

    [HttpQueryMember('course')]
    [Nullable]
    FCourse: NullableInteger;

    [HttpQueryMember('speed')]
    [Nullable]
    FSpeed: NullableDouble;

    [HttpQueryMember('lat')]
    [Nullable]
    FLatitude: NullableDouble;

    [HttpQueryMember('lng')]
    [Nullable]
    FLongitude: NullableDouble;

    [HttpQueryMember('altitude')]
    [Nullable]
    FAltitude: NullableDouble;

    [HttpQueryMember('device_type')]
    [Nullable]
    FDeviceType: NullableString;

    [HttpQueryMember('device_guid')]
    [Nullable]
    FDeviceGuid: NullableString;

    [HttpQueryMember('device_timestamp')]
    [Nullable]
    FDeviceTimestamp: NullableString;

    [HttpQueryMember('app_version')]
    [Nullable]
    FAppVersion: NullableString;
  public
    constructor Create; override;

    property Format: NullableString read FFormat write FFormat;
    property MemberId: NullableInteger read FMemberId write FMemberId;
    property RouteId: NullableString read FRouteId write FRouteId;
    property TxId: NullableString read FTxId write FTxId;
    property VehicleId: NullableInteger read FVehicleId write FVehicleId;
    property Course: NullableInteger read FCourse write FCourse;
    property Speed: NullableDouble read FSpeed write FSpeed;
    property Latitude: NullableDouble read FLatitude write FLatitude;
    property Longitude: NullableDouble read FLongitude write FLongitude;
    property Altitude: NullableDouble read FAltitude write FAltitude;
    property DeviceType: NullableString read FDeviceType write FDeviceType;
    property DeviceGuid: NullableString read FDeviceGuid write FDeviceGuid;
    property DeviceTimestamp: NullableString read FDeviceTimestamp write FDeviceTimestamp;
    property AppVersion: NullableString read FAppVersion write FAppVersion;
  end;

implementation

{ TActivityParameters }

constructor TGPSParameters.Create;
begin
  Inherited Create;

  FFormat := NullableString.Null;
  FMemberId := NullableInteger.Null;
  FRouteId := NullableString.Null;
  FTxId := NullableString.Null;
  FVehicleId := NullableInteger.Null;
  FCourse := NullableInteger.Null;
  FSpeed := NullableDouble.Null;
  FLatitude := NullableDouble.Null;
  FLongitude := NullableDouble.Null;
  FAltitude := NullableDouble.Null;
  FDeviceType := NullableString.Null;
  FDeviceGuid := NullableString.Null;
  FDeviceTimestamp := NullableString.Null;
  FAppVersion := NullableString.Null;
end;

end.
