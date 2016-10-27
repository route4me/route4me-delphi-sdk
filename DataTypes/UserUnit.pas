unit UserUnit;

interface

uses
  SysUtils, System.Generics.Collections,
  REST.Json.Types, JSONNullableAttributeUnit,
  NullableBasicTypesUnit, GenericParametersUnit,
  JSONDictionaryInterceptorObjectUnit;

type

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
    FFirstName: String;

    [JSONName('member_last_name')]
    FLastName: String;

    [JSONName('member_email')]
    FEmail: String;

    [JSONName('phone_number')]
    FPhoneNumber: String;

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

    property FirstName: String read FFirstName write FFirstName;
    property LastName: String read FLastName write FLastName;
    property Email: String read FEmail write FEmail;
    property PhoneNumber: String read FPhoneNumber write FPhoneNumber;
    property MemberId: NullableInteger read FMemberId write FMemberId;
    property AccountTypeId: NullableInteger read FAccountTypeId write FAccountTypeId;
    property MemberType: NullableString read FMemberType write FMemberType;
    property ReadonlyUser: NullableBoolean read FReadonlyUser write FReadonlyUser;
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
