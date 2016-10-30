unit UserUnit;

interface

uses
  SysUtils, System.Generics.Collections,
  REST.Json.Types, JSONNullableAttributeUnit,
  NullableBasicTypesUnit, GenericParametersUnit,
  JSONDictionaryInterceptorObjectUnit;

type
  // todo: привести в соответствии схеме
  /// <remarks>
  ///  https://github.com/route4me/json-schemas/blob/master/User_v4_response.dtd
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
  public
    /// <remarks>
    ///  Constructor with 0-arguments must be and be public.
    ///  For JSON-deserialization.
    /// </remarks>
    constructor Create; overload; override;

    function Equals(Obj: TObject): Boolean; override;

    /// <summary>
    ///  The first name of the user
    /// </summary>
    property FirstName: NullableString read FFirstName write FFirstName;

    /// <summary>
    ///  The last name of the user
    /// </summary>
    property LastName: NullableString read FLastName write FLastName;

    /// <summary>
    ///  E-mail address of the user that was used for website registration
    /// </summary>
    property Email: NullableString read FEmail write FEmail;

    /// <summary>
    ///  A phone number of the user
    /// </summary>
    property PhoneNumber: NullableString read FPhoneNumber write FPhoneNumber;

    /// <summary>
    ///  Member ID
    /// </summary>
    property MemberId: NullableInteger read FMemberId write FMemberId;

    /// <summary>
    ///  a
    /// </summary>
    property AccountTypeId: NullableInteger read FAccountTypeId write FAccountTypeId;

    /// <summary>
    ///  a
    /// </summary>
    property MemberType: NullableString read FMemberType write FMemberType;

    /// <summary>
    ///  a
    /// </summary>
    property ReadonlyUser: NullableBoolean read FReadonlyUser write FReadonlyUser;

    /// <summary>
    ///  a
    /// </summary>
    property ShowSuperuserAddresses: NullableBoolean read FShowSuperuserAddresses write FShowSuperuserAddresses;
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

  Result := (MemberId = Other.MemberId) and
    (AccountTypeId = Other.AccountTypeId) and
    (Email = Other.Email) and
    (MemberId = Other.MemberId) and
    (ReadonlyUser = Other.ReadonlyUser) and
    (ShowSuperuserAddresses = Other.ShowSuperuserAddresses) and
    (FirstName = Other.FirstName) and
    (LastName = Other.LastName) and
    (PhoneNumber = Other.PhoneNumber);
end;

end.
