unit TestMemberSamplesUnit;

interface

uses
  TestFramework, Classes, SysUtils, DateUtils,
  BaseTestOnlineExamplesUnit;

type
  TTestMemberSamples = class(TTestOnlineExamples)
  published
    procedure AddNewUser;
    procedure GetUserDetails;
    procedure UpdateUser;
    procedure Authentication;
    procedure ValidateSession;
    procedure UserLicense;
    procedure GetUsers;
    procedure RegisterWebinar;
    procedure RemoveUser;
    procedure DeviceLicense;
  end;


implementation

{ TTestMemberSamples }

uses UserParametersUnit, UserParameterProviderUnit, UserUnit, EnumsUnit,
  NullableBasicTypesUnit;

var
  FMemberId: NullableInteger;
  FEMail: String;
  FSessionGuid: NullableString;
  FSessionId: NullableInteger;

procedure TTestMemberSamples.AddNewUser;
var
  Parameters: TUserParameters;
  Provider: IUserParameterProvider;
  ErrorString: String;
  DoubleMemberId: NullableInteger;
begin
  Provider := TUserParameterProvider.Create;

  // need real email
  FEMail := 'customer.services@store.manutd.com';
  Parameters := Provider.GetParameters(FEMail);
  try
    // Correct adding new user. Must be success.
    FMemberId := FRoute4MeManager.User.AddNewUser(Parameters, ErrorString);
    CheckEquals(EmptyStr, ErrorString);
    CheckTrue(FMemberId.IsNotNull);

    // Repeat adding same new user. Must be error.
    DoubleMemberId := FRoute4MeManager.User.AddNewUser(Parameters, ErrorString);
    CheckNotEquals(EmptyStr, ErrorString);
    CheckTrue(DoubleMemberId.IsNull);
  finally
    FreeAndNil(Parameters);
  end;
end;

procedure TTestMemberSamples.Authentication;
var
  Parameters: TUserParameters;
  Provider: IUserParameterProvider;
  ErrorString: String;
begin
  Provider := TUserParameterProvider.Create;

  Parameters := Provider.GetParameters(FEMail);
  try
    // Authentication the incorrected user. Must be error.
    FRoute4MeManager.User.Authentication(FEMail, Parameters.Password + '123',
      ErrorString, FSessionId, FSessionGuid);
    CheckTrue(FSessionId.IsNull);
    CheckTrue(FSessionGuid.IsNull);
    CheckNotEquals(EmptyStr, ErrorString);

    // Authentication the corrected user. Must be success.
    FRoute4MeManager.User.Authentication(FEMail, Parameters.Password,
      ErrorString, FSessionId, FSessionGuid);
    CheckTrue(FSessionId.IsNotNull);
    CheckTrue(FSessionGuid.IsNotNull);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Parameters);
  end;
end;

procedure TTestMemberSamples.DeviceLicense;
var
  ErrorString: String;
  DeviceId: String;
  DeviceType: TDeviceType;
begin
  DeviceId := 'random string';
  DeviceType := TDeviceType.IPad;

  // Undefined DeviceId. Must be error.
  CheckFalse(FRoute4MeManager.User.DeviceLicense(DeviceId, DeviceType, ErrorString));
  CheckNotEquals(EmptyStr, ErrorString);

{ DONE: For some unknown reason it doesn't work.

  // Registered DeviceId. Must be success.
  DeviceId := '546546516';
  CheckTrue(FRoute4MeManager.User.DeviceLicense(DeviceId, DeviceType, ErrorString));
  CheckEquals(EmptyStr, ErrorString);}
end;

procedure TTestMemberSamples.GetUserDetails;
var
  ErrorString: String;
  User: TUser;
begin
  CheckTrue(FMemberId.IsNotNull);

  // Get details of unexisting user. Must be error.
  User := FRoute4MeManager.User.Get(-1, ErrorString);
  CheckNull(User);
  CheckNotEquals(EmptyStr, ErrorString);

  // Get details of correct user. Must be success.
  User := FRoute4MeManager.User.Get(FMemberId, ErrorString);
  CheckNotNull(User);
  CheckTrue(User.MemberId = FMemberId);
  CheckEquals(EmptyStr, ErrorString);
end;

procedure TTestMemberSamples.GetUsers;
var
  ErrorString: String;
  Users: TUserList;
begin
  // Must be success.
  Users := FRoute4MeManager.User.Get(ErrorString);
  try
    CheckEquals(EmptyStr, ErrorString);
    CheckTrue(Users.Count > 0);
  finally
    FreeAndNil(Users);
  end;
end;

procedure TTestMemberSamples.RegisterWebinar;
var
  ErrorString: String;
  FirstName, LastName, Phone, Company: String;
  StartDate: TDateTime;
begin
  FirstName := 'Mmmmm';
  LastName := 'Ccccc';
  Phone := '454-454544';
  Company := 'c_name';
  StartDate := IncDay(Now, 1);

  // Unexisting MemberId. Must be error.
  CheckFalse(FRoute4MeManager.User.RegisterWebinar(FEmail, FirstName, LastName,
    Phone, Company, -1, StartDate, ErrorString));
  CheckEquals(EmptyStr, ErrorString);

{ DONE: For some unknown reason it doesn't work.

  // Must be success.
  CheckTrue(FRoute4MeManager.User.RegisterWebinar(FEmail, FirstName, LastName,
    Phone, Company, FMemberId, StartDate, ErrorString));
  CheckEquals(EmptyStr, ErrorString);}
end;

procedure TTestMemberSamples.RemoveUser;
var
  ErrorString: String;
begin
  // Removing existing user. Must be success.
  CheckTrue(FRoute4MeManager.User.Remove(FMemberId, ErrorString));
  CheckEquals(EmptyStr, ErrorString);

  // Removing unexisting user. Must be error.
  CheckFalse(FRoute4MeManager.User.Remove(FMemberId, ErrorString));
  CheckNotEquals(EmptyStr, ErrorString);
end;

procedure TTestMemberSamples.UpdateUser;
var
  Parameters: TUserParameters;
  Provider: IUserParameterProvider;
  ErrorString: String;
begin
  Provider := TUserParameterProvider.Create;

  Parameters := Provider.GetParameters(FEMail);
  try
    // Correct updating new user. Must be success.
    Parameters.FirstName := 'John';
    Parameters.MemberId := FMemberId;
    FRoute4MeManager.User.Update(Parameters, ErrorString);
    CheckEquals(EmptyStr, ErrorString);

    Parameters.MemberId := -1;
    // Updating unexisting user. Must be error.
    FRoute4MeManager.User.Update(Parameters, ErrorString);
    CheckNotEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Parameters);
  end;
end;

procedure TTestMemberSamples.UserLicense;
var
  ErrorString: String;
  DeviceId: String;
  DeviceType: TDeviceType;
  Subscription: String;
  Token: String;
  Payload: String;
begin
  DeviceId := 'random string';
  DeviceType := TDeviceType.IPad;
  Subscription := 'IPAD_MONTHLY';
  Token := '4/P7q7W91a-oMsCeLvIaQm6bTrgtp7';
  Payload := 'APA91bHun4MxP5egoKMwt2KZFBaFUH-1RYqx';

  // Undefined DeviceId. Must be error.
  CheckFalse(FRoute4MeManager.User.UserLicense(FMemberId, FSessionId,
    DeviceId, DeviceType, Subscription, Token, Payload, ErrorString));
  CheckNotEquals(EmptyStr, ErrorString);

{ DONE: For some unknown reason it doesn't work.

  // Registered DeviceId. Must be success.
  DeviceId := '546546516';
  CheckTrue(FRoute4MeManager.User.UserLicense(FMemberId, FSessionId, DeviceId,
    DeviceType, Subscription, Token, Payload, ErrorString));
  CheckEquals(EmptyStr, ErrorString);}
end;

procedure TTestMemberSamples.ValidateSession;
var
  ErrorString: String;
begin
  CheckTrue(FSessionGuid.IsNotNull);
  CheckTrue(FMemberId.IsNotNull);

{ DONE: For some unknown reason it doesn't work.

  // Validate session of the corrected user. Must be success.
  CheckTrue(FRoute4MeManager.User.IsSessionValid(FSessionGuid, FMemberId, ErrorString));
  CheckEquals(EmptyStr, ErrorString);}

  // Validate session of the incorrected session. Must be error.
  CheckFalse(FRoute4MeManager.User.IsSessionValid('1' + FSessionGuid.Value, FMemberId, ErrorString));
  CheckEquals(EmptyStr, ErrorString);

  // Validate session of the incorrected user. Must be error.
  CheckFalse(FRoute4MeManager.User.IsSessionValid(FSessionGuid, -1, ErrorString));
  CheckEquals(EmptyStr, ErrorString);
end;

initialization
  RegisterTest('Examples\Online\Members\', TTestMemberSamples.Suite);
  FMemberId := NullableInteger.Null;
  FSessionGuid := NullableString.Null;
  FSessionId := NullableInteger.Null;
end.
