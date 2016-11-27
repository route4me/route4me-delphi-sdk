unit UserParametersUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections, SysUtils, Classes,
  JSONNullableAttributeUnit,
  NullableBasicTypesUnit, GenericParametersUnit, EnumsUnit;

type
  TUserParameters = class(TGenericParameters)
  private
    [JSONName('member_id')]
    [Nullable]
    FMemberId: NullableInteger;

    [JSONName('HIDE_ROUTED_ADDRESSES')]
    [Nullable]
    FHideRoutedAddresses: NullableString;

    [JSONName('member_phone')]
    [Nullable]
    FPhoneNumber: NullableString;

    [JSONName('member_zipcode')]
    [Nullable]
    FZip: NullableString;

    [JSONName('route_count')]
    [Nullable]
    FRouteCount: NullableInteger;

    [JSONName('member_email')]
    [Nullable]
    FEmail: NullableString;

    [JSONName('HIDE_VISITED_ADDRESSES')]
    [Nullable]
    FHideVisitedAddresses: NullableString;

    [JSONName('READONLY_USER')]
    [Nullable]
    FReadonlyUser: NullableString;

    [JSONName('member_type')]
    [Nullable]
    FMemberType: NullableString;

    [JSONName('date_of_birth')]
    [Nullable]
    FDateOfBirth: NullableString;

    [JSONName('member_first_name')]
    [Nullable]
    FFirstName: NullableString;

    [JSONName('member_last_name')]
    [Nullable]
    FLastName: NullableString;

    [JSONName('member_password')]
    [Nullable]
    FPassword: NullableString;

    [JSONName('HIDE_NONFUTURE_ROUTES')]
    [Nullable]
    FHideNonFutureRoutes: NullableString;

    [JSONName('SHOW_ALL_VEHICLES')]
    [Nullable]
    FShowAllVehicles: NullableString;

    [JSONName('SHOW_ALL_DRIVERS')]
    [Nullable]
    FShowAllDrivers: NullableString;

    function GetMemberType: TMemberType;
    procedure SetMemberType(const Value: TMemberType);

    function GetHideRoutedAddresses: NullableBoolean;
    function GetHideNonFutureRoutes: NullableBoolean;
    function GetHideVisitedAddresses: NullableBoolean;
    function GetShowAllDrivers: NullableBoolean;
    function GetShowAllVehicles: NullableBoolean;
    function GetReadonlyUser: NullableBoolean;
    procedure SetHideRoutedAddresses(const Value: NullableBoolean);
    procedure SetHideNonFutureRoutes(const Value: NullableBoolean);
    procedure SetHideVisitedAddresses(const Value: NullableBoolean);
    procedure SetShowAllDrivers(const Value: NullableBoolean);
    procedure SetShowAllVehicles(const Value: NullableBoolean);
    procedure SetReadonlyUser(const Value: NullableBoolean);
  public
    /// <remarks>
    ///  Constructor with 0-arguments must be and be public.
    ///  For JSON-deserialization.
    /// </remarks>
    constructor Create; override;

    property MemberId: NullableInteger read FMemberId write FMemberId;

    /// <summary>
    ///  A first name of the member
    /// </summary>
    property FirstName: NullableString read FFirstName write FFirstName;

    /// <summary>
    ///  A last name of the member
    /// </summary>
    property LastName: NullableString read FLastName write FLastName;

    /// <summary>
    ///  An email of the member
    /// </summary>
    property Email: NullableString read FEmail write FEmail;

    /// <summary>
    ///  A phone number of the member
    /// </summary>
    property PhoneNumber: NullableString read FPhoneNumber write FPhoneNumber;

    /// <summary>
    ///  A type of the member
    /// </summary>
    property MemberType: TMemberType read GetMemberType write SetMemberType;


    /// <summary>
    ///  Zip
    /// </summary>
    property Zip: NullableString read FZip write FZip;

    /// <summary>
    ///  Zip
    /// </summary>
    property RouteCount: NullableInteger read FRouteCount write FRouteCount;

    /// <summary>
    ///  Date of user birth
    /// </summary>
    property DateOfBirth: NullableString read FDateOfBirth write FDateOfBirth;

    /// <summary>
    ///  Password
    /// </summary>
    property Password: NullableString read FPassword write FPassword;

    /// <summary>
    ///
    /// </summary>
    property ReadonlyUser: NullableBoolean read GetReadonlyUser write SetReadonlyUser;

    /// <summary>
    ///
    /// </summary>
    property HideRoutedAddresses: NullableBoolean read GetHideRoutedAddresses write SetHideRoutedAddresses;

    /// <summary>
    ///
    /// </summary>
    property HideVisitedAddresses: NullableBoolean read GetHideVisitedAddresses write SetHideVisitedAddresses;

    /// <summary>
    ///
    /// </summary>
    property HideNonFutureRoutes: NullableBoolean read GetHideNonFutureRoutes write SetHideNonFutureRoutes;

    /// <summary>
    ///
    /// </summary>
    property ShowAllVehicles: NullableBoolean read GetShowAllVehicles write SetShowAllVehicles;

    /// <summary>
    ///
    /// </summary>
    property ShowAllDrivers: NullableBoolean read GetShowAllDrivers write SetShowAllDrivers;
  end;

implementation

constructor TUserParameters.Create;
begin
  Inherited Create;

  FMemberId := NullableInteger.Null;
  FFirstName := NullableString.Null;
  FLastName := NullableString.Null;
  FEmail := NullableString.Null;
  FPhoneNumber := NullableString.Null;
  FMemberType := NullableString.Null;
  FReadonlyUser := NullableString.Null;
  FZip := NullableString.Null;
  FRouteCount := NullableInteger.Null;
  FDateOfBirth := NullableString.Null;
  FPassword := NullableString.Null;
  FHideRoutedAddresses := NullableString.Null;
  FHideVisitedAddresses := NullableString.Null;
  FHideNonFutureRoutes := NullableString.Null;
  FShowAllVehicles := NullableString.Null;
  FShowAllDrivers := NullableString.Null;
end;

function TUserParameters.GetHideNonFutureRoutes: NullableBoolean;
begin
  if FHideNonFutureRoutes.IsNotNull then
    Result := StrToBool(FHideNonFutureRoutes.Value)
  else
    Result := NullableBoolean.Null;
end;

function TUserParameters.GetHideRoutedAddresses: NullableBoolean;
begin
  if FHideRoutedAddresses.IsNotNull then
    Result := StrToBool(FHideRoutedAddresses.Value)
  else
    Result := NullableBoolean.Null;
end;

function TUserParameters.GetHideVisitedAddresses: NullableBoolean;
begin
  if FHideVisitedAddresses.IsNotNull then
    Result := StrToBool(FHideVisitedAddresses.Value)
  else
    Result := NullableBoolean.Null;
end;

function TUserParameters.GetMemberType: TMemberType;
var
  MemberType: TMemberType;
begin
  Result := TMemberType.mtUnknown;
  if FMemberType.IsNotNull then
    for MemberType := Low(TMemberType) to High(TMemberType) do
      if (FMemberType = TMemberTypeDescription[MemberType]) then
        Exit(MemberType);
end;

function TUserParameters.GetReadonlyUser: NullableBoolean;
begin
  if FReadonlyUser.IsNotNull then
    Result := StrToBool(FReadonlyUser.Value)
  else
    Result := NullableBoolean.Null;
end;

function TUserParameters.GetShowAllDrivers: NullableBoolean;
begin
  if FShowAllDrivers.IsNotNull then
    Result := StrToBool(FShowAllDrivers.Value)
  else
    Result := NullableBoolean.Null;
end;

function TUserParameters.GetShowAllVehicles: NullableBoolean;
begin
  if FShowAllVehicles.IsNotNull then
    Result := StrToBool(FShowAllVehicles.Value)
  else
    Result := NullableBoolean.Null;
end;

procedure TUserParameters.SetHideNonFutureRoutes(
  const Value: NullableBoolean);
begin
  if Value.IsNull then
    FHideNonFutureRoutes := NullableString.Null
  else
    FHideNonFutureRoutes := UpperCase(BoolToStr(Value, True));
end;

procedure TUserParameters.SetHideRoutedAddresses(
  const Value: NullableBoolean);
begin
  if Value.IsNull then
    FHideRoutedAddresses := NullableString.Null
  else
    FHideRoutedAddresses := UpperCase(BoolToStr(Value, True));
end;

procedure TUserParameters.SetHideVisitedAddresses(
  const Value: NullableBoolean);
begin
  if Value.IsNull then
    FHideVisitedAddresses := NullableString.Null
  else
    FHideVisitedAddresses := UpperCase(BoolToStr(Value, True));
end;

procedure TUserParameters.SetMemberType(const Value: TMemberType);
begin
  FMemberType := TMemberTypeDescription[Value];
end;

procedure TUserParameters.SetReadonlyUser(const Value: NullableBoolean);
begin
  if Value.IsNull then
    FReadonlyUser := NullableString.Null
  else
    FReadonlyUser := UpperCase(BoolToStr(Value, True));
end;

procedure TUserParameters.SetShowAllDrivers(const Value: NullableBoolean);
begin
  if Value.IsNull then
    FShowAllDrivers := NullableString.Null
  else
    FShowAllDrivers := UpperCase(BoolToStr(Value, True));
end;

procedure TUserParameters.SetShowAllVehicles(
  const Value: NullableBoolean);
begin
  if Value.IsNull then
    FShowAllVehicles := NullableString.Null
  else
    FShowAllVehicles := UpperCase(BoolToStr(Value, True));
end;

end.
