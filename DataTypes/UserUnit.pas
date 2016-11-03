unit UserUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections, SysUtils,
  JSONNullableAttributeUnit,
  NullableBasicTypesUnit, GenericParametersUnit, EnumsUnit;

type
  /// <summary>
  ///  Response json schema for the retrieved member's object
  /// </summary>
  /// <remarks>
  ///  https://github.com/route4me/json-schemas/blob/master/Member_response.dtd
  /// </remarks>
  TUser = class(TGenericParameters)
  private
    [JSONName('member_id')]
    [Nullable]
    FMemberId: NullableInteger;

    [JSONName('account_type_id')]
    [Nullable]
    FAccountTypeId: NullableInteger;

    [JSONName('member_type')]
    [Nullable]
    FMemberType: NullableString;

    [JSONName('member_first_name')]
    [Nullable]
    FFirstName: NullableString;

    [JSONName('member_last_name')]
    [Nullable]
    FLastName: NullableString;

    [JSONName('member_email')]
    [Nullable]
    FEmail: NullableString;

    [JSONName('phone_number')]
    [Nullable]
    FPhoneNumber: NullableString;

    [JSONName('readonly_user')]
    [Nullable]
    FReadonlyUser: NullableBoolean;

    [JSONName('show_superuser_addresses')]
    [Nullable]
    FShowSuperuserAddresses: NullableBoolean;

    [JSONName('api_key')]
    [Nullable]
    FApiKey: NullableString;

    [JSONName('hashed_member_id')]
    [Nullable]
    FHashedMemberId: NullableString;

    function GetMemberType: TMemberType;
    procedure SetMemberType(const Value: TMemberType);
  public
    /// <remarks>
    ///  Constructor with 0-arguments must be and be public.
    ///  For JSON-deserialization.
    /// </remarks>
    constructor Create; overload; override;

    function Equals(Obj: TObject): Boolean; override;

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
    ///  An unique ID of the member
    /// </summary>
    property MemberId: NullableInteger read FMemberId write FMemberId;

    /// <summary>
    ///  Account type ID of the member
    /// </summary>
    property AccountTypeId: NullableInteger read FAccountTypeId write FAccountTypeId;

    /// <summary>
    ///  A type of the member
    /// </summary>
    property MemberType: TMemberType read GetMemberType write SetMemberType;

    /// <summary>
    ///  True if value is '1'
    /// </summary>
    property ReadonlyUser: NullableBoolean read FReadonlyUser write FReadonlyUser;

    /// <summary>
    ///  Show superuser addresses
    /// </summary>
    property ShowSuperuserAddresses: NullableBoolean read FShowSuperuserAddresses write FShowSuperuserAddresses;

    /// <summary>
    ///  Api key of the user
    /// </summary>
    property ApiKey: NullableString read FApiKey write FApiKey;

    /// <summary>
    ///  Hashed member ID string
    /// </summary>
    property HashedMemberId: NullableString read FHashedMemberId write FHashedMemberId;
  end;

  TUserList = TList<TUser>;

implementation

{ TUser }

constructor TUser.Create;
begin
  Inherited Create;

  FMemberId := NullableInteger.Null;
  FAccountTypeId := NullableInteger.Null;
  FMemberType := NullableString.Null;
  FReadonlyUser := NullableBoolean.Null;
  FShowSuperuserAddresses := NullableBoolean.Null;

  FFirstName := EmptyStr;
  FLastName := EmptyStr;
  FEmail := EmptyStr;
  FPhoneNumber := EmptyStr;
end;

{ TUser }

function TUser.Equals(Obj: TObject): Boolean;
var
  Other: TUser;
begin
  Result := False;

  if not (Obj is TUser) then
    Exit;

  Other := TUser(Obj);

  Result :=
    (FMemberId = Other.FMemberId) and
    (FAccountTypeId = Other.FAccountTypeId) and
    (FEmail = Other.FEmail) and
    (FMemberId = Other.FMemberId) and
    (FReadonlyUser = Other.FReadonlyUser) and
    (FShowSuperuserAddresses = Other.FShowSuperuserAddresses) and
    (FFirstName = Other.FFirstName) and
    (FLastName = Other.FLastName) and
    (FPhoneNumber = Other.FPhoneNumber);
end;

function TUser.GetMemberType: TMemberType;
var
  MemberType: TMemberType;
begin
  Result := TMemberType.mtUnknown;
  if FMemberType.IsNotNull then
    for MemberType := Low(TMemberType) to High(TMemberType) do
      if (FMemberType = TMemberTypeDescription[MemberType]) then
        Exit(MemberType);
end;

procedure TUser.SetMemberType(const Value: TMemberType);
begin
  FMemberType := TMemberTypeDescription[Value];
end;

end.
