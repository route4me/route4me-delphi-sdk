unit NoteParametersUnit;

interface

uses
  HttpQueryMemberAttributeUnit, JSONNullableAttributeUnit,
  NullableBasicTypesUnit, GenericParametersUnit, EnumsUnit;

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

    function GetDeviceType: TDeviceType;
    procedure SetDeviceType(const Value: TDeviceType);
    function GetActivityType: TStatusUpdateType;
    procedure SetActivityType(const Value: TStatusUpdateType);
  public
    constructor Create; override;

    property RouteId: NullableString read FRouteId write FRouteId;
    property AddressId: NullableInteger read FAddressId write FAddressId;
    property Latitude: NullableDouble read FLatitude write FLatitude;
    property Longitude: NullableDouble read FLongitude write FLongitude;
    property DeviceType: TDeviceType read GetDeviceType write SetDeviceType;
    property ActivityType: TStatusUpdateType read GetActivityType write SetActivityType;
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

function TNoteParameters.GetActivityType: TStatusUpdateType;
var
  ActivityType: TStatusUpdateType;
begin
  Result := TStatusUpdateType.Unclassified;
  if FActivityType.IsNotNull then
    for ActivityType := Low(TStatusUpdateType) to High(TStatusUpdateType) do
      if (FActivityType = TStatusUpdateTypeDescription[ActivityType]) then
        Exit(ActivityType);
end;

function TNoteParameters.GetDeviceType: TDeviceType;
var
  DeviceType: TDeviceType;
begin
  Result := TDeviceType.UnknownDevice;
  if FDeviceType.IsNotNull then
    for DeviceType := Low(TDeviceType) to High(TDeviceType) do
      if (FDeviceType = TDeviceTypeDescription[DeviceType]) then
        Exit(DeviceType);
end;

procedure TNoteParameters.SetActivityType(const Value: TStatusUpdateType);
begin
  FActivityType := TStatusUpdateTypeDescription[Value];
end;

procedure TNoteParameters.SetDeviceType(const Value: TDeviceType);
begin
  FDeviceType := TDeviceTypeDescription[Value];
end;

end.
