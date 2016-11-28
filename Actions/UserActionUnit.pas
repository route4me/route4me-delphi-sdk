unit UserActionUnit;

interface

uses
  SysUtils, BaseActionUnit, System.Generics.Collections,
  DataObjectUnit, GenericParametersUnit, UserUnit, EnumsUnit,
  NullableBasicTypesUnit, UserParametersUnit;

type
  TUserActions = class(TBaseAction)
  public
    function Get(MemberId: integer; out ErrorString: String): TUser; overload;

    function Get(out ErrorString: String): TUserList; overload;

    /// <summary>
    ///  Authentication of a user with an email and password
    /// </summary>
    /// <returns>
    ///  SessionGuid
    /// </returns>
    function Authentication(Email, Password: String;
      out ErrorString: String): NullableInteger;

    /// <summary>
    ///  Check if a session is valid.
    /// </summary>
    function IsSessionValid(SessionId, MemberId: integer;
      out ErrorString: String): boolean;

    /// <summary>
    ///  Check if a session is valid.
    /// </summary>
    /// <returns>
    ///  Member Id of added user
    /// </returns>
    function RegisterAccount(Plan, Industry, FirstName, LastName, Email: String;
      Terms: boolean; DeviceType: TDeviceType; Password, PasswordConfirmation: String;
      out ErrorString: String): NullableInteger; deprecated;

    /// <summary>
    ///  Create new user
    /// </summary>
    /// <returns>
    ///  Member Id of added user
    /// </returns>
    function AddNewUser(Parameters: TUserParameters;
      out ErrorString: String): NullableInteger;

    /// <summary>
    ///  Update an existing user
    /// </summary>
    /// <returns>
    ///  Member Id of added user
    /// </returns>
    procedure Update(Parameters: TUserParameters; out ErrorString: String);

    /// <summary>
    ///  Remove existing user from a member�s account.
    /// </summary>
    function Remove(MemberId: integer; out ErrorString: String): boolean;

    function DeviceLicense(DeviceId: String; DeviceType: TDeviceType;
      out ErrorString: String): boolean;

    function UserLicense(MemberId, SessionId: integer;
      DeviceId: String; DeviceType: TDeviceType; Subscription: String;
      Token: String; Payload: String; out ErrorString: String): boolean;

    function RegisterWebinar(Email, FirstName, LastName, Phone, Company: String;
      MemberId: integer; StartDate: TDateTime; out ErrorString: String): boolean;
  end;

implementation

{ TUserActions }

uses SettingsUnit, ValidateSessionResponseUnit, AddNewUserResponseUnit,
  RegisterAccountResponseUnit, StatusResponseUnit, RemoveUserRequestUnit,
  AuthenticationResponseUnit, CommonTypesUnit, DeviceLicenseRequestUnit,
  UserLicenseRequestUnit, RegisterWebinarRequestUnit;

function TUserActions.RegisterAccount(Plan, Industry, FirstName, LastName, Email: String;
  Terms: boolean; DeviceType: TDeviceType; Password,
  PasswordConfirmation: String; out ErrorString: String): NullableInteger;
var
  Parameters: TGenericParameters;
  Response: TRegisterAccountResponse;
begin
  Result := NullableInteger.Null;

  Parameters := TGenericParameters.Create;
  try
    Parameters.AddParameter('plan', Plan);
    Parameters.AddBodyParameter('strIndustry', Industry);
    Parameters.AddBodyParameter('strFirstName', FirstName);
    Parameters.AddBodyParameter('strLastName', LastName);
    Parameters.AddBodyParameter('strEmail', Email);
    Parameters.AddBodyParameter('format', 'json');
    if Terms then
      Parameters.AddBodyParameter('chkTerms', '1')
    else
      Parameters.AddBodyParameter('chkTerms', '0');
    Parameters.AddBodyParameter('device_type', TDeviceTypeDescription[DeviceType]);
    Parameters.AddBodyParameter('strPassword_1', Password);
    Parameters.AddBodyParameter('strPassword_2', PasswordConfirmation);

    Response := FConnection.Post(TSettings.RegisterAccount, Parameters,
      TRegisterAccountResponse, ErrorString) as TRegisterAccountResponse;
    try
      if (Response <> nil) then
        if (Length(Response.Errors) > 0) then
          ErrorString := String.Join('; ', Response.Errors)
        else
          Result := Response.MemberId
      else
        if (ErrorString = EmptyStr) then
          ErrorString := 'New user not added';
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

function TUserActions.RegisterWebinar(Email, FirstName, LastName, Phone,
  Company: String; MemberId: integer; StartDate: TDateTime;
  out ErrorString: String): boolean;
var
  Request: TRegisterWebinarRequest;
  Response: TSimpleString;
begin
  Result := False;

  Request := TRegisterWebinarRequest.Create(Email, FirstName, LastName, Phone,
    Company, MemberId, StartDate);
  try
    Response := FConnection.Post(TSettings.RegisterWebinarHost,
      Request, TSimpleString, ErrorString) as TSimpleString;
    try
      if (Response = nil) and (ErrorString = EmptyStr) then
        ErrorString := 'RegisterWebinar fault';
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

function TUserActions.Remove(MemberId: integer; out ErrorString: String): boolean;
var
  Request: TRemoveUserRequest;
  Response: TStatusResponse;
begin
  Request := TRemoveUserRequest.Create(MemberId);
  try
    Response := FConnection.Delete(TSettings.UsersHost,
      Request, TStatusResponse, ErrorString) as TStatusResponse;
    try
      Result := (Response <> nil) and (Response.Status);

      if ((Response = nil) and (ErrorString = EmptyStr)) or
        ((Response <> nil) and (not Response.Status)) then
        ErrorString := 'User not removed';
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

procedure TUserActions.Update(Parameters: TUserParameters;
  out ErrorString: String);
var
  Response: TAddNewUserResponse;
begin
  Response := FConnection.Put(TSettings.UsersHost,
    Parameters, TAddNewUserResponse, ErrorString) as TAddNewUserResponse;
  try
    if (Response = nil) and (ErrorString = EmptyStr) then
      ErrorString := 'User not updated';
  finally
    FreeAndNil(Response);
  end;
end;

function TUserActions.UserLicense(MemberId, SessionId: integer;
  DeviceId: String; DeviceType: TDeviceType; Subscription: String;
  Token: String; Payload: String; out ErrorString: String): boolean;
var
  Request: TUserLicenseRequest;
  Response: TSimpleString;
begin
  Result := False;

  Request := TUserLicenseRequest.Create(MemberId, SessionId, DeviceId,
    DeviceType, Subscription, Token, Payload);
  try
    Response := FConnection.Post(TSettings.UserLicenseHost,
      Request, TSimpleString, ErrorString) as TSimpleString;
    try
      if (Response <> nil) and (ErrorString = EmptyStr) then
        ErrorString := Response.Value;

      if (Response = nil) and (ErrorString = EmptyStr) then
        ErrorString := 'UserLicense fault';
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

function TUserActions.AddNewUser(Parameters: TUserParameters;
  out ErrorString: String): NullableInteger;
var
  Response: TAddNewUserResponse;
  EMail: String;
begin
  Result := NullableInteger.Null;
  if (Parameters.Email.IsNotNull) then
  begin
    EMail := Parameters.Email;
    StringReplace(EMail, ' ', '+', [rfReplaceAll]);
    Parameters.Email := EMail;
  end;

  Response := FConnection.Post(TSettings.UsersHost,
    Parameters, TAddNewUserResponse, ErrorString) as TAddNewUserResponse;
  try
    if (Response <> nil) then
      Result := Response.MemberId
    else
    if (Response = nil) and (ErrorString = EmptyStr) then
      ErrorString := 'New user not added';
  finally
    FreeAndNil(Response);
  end;
end;

function TUserActions.Authentication(Email, Password: String;
  out ErrorString: String): NullableInteger;
var
  Parameters: TGenericParameters;
  Response: TObject;
  PossibleResponses: TClassArray;
begin
  Result := NullableInteger.Null;

  Parameters := TGenericParameters.Create;
  try
    Parameters.AddBodyParameter('strEmail', Email);
    Parameters.AddBodyParameter('strPassword', Password);
    Parameters.AddBodyParameter('format', 'json');

    SetLength(PossibleResponses, 2);
    PossibleResponses[0] := TGoodAuthenticationResponse;
    PossibleResponses[1] := TBadAuthenticationResponse;

    Response := FConnection.Post(TSettings.Authenticate,
      Parameters, PossibleResponses, ErrorString);

    if (Response <> nil) then
    begin
      if Response is TGoodAuthenticationResponse then
      begin
        if (TGoodAuthenticationResponse(Response).Status) then
          Result := TGoodAuthenticationResponse(Response).SessionId
        else
          ErrorString := 'User authentication error';
      end
      else
          ErrorString := (Response as TBadAuthenticationResponse).Error;
    end
    else
    if (Response = nil) and (ErrorString = EmptyStr) then
      ErrorString := 'User authentication error';
  finally
    FreeAndNil(Parameters);
  end;
end;

function TUserActions.DeviceLicense(DeviceId: String; DeviceType: TDeviceType;
  out ErrorString: String): boolean;
var
  Request: TDeviceLicenseRequest;
  Response: TSimpleString;
begin
  Result := False;

  Request := TDeviceLicenseRequest.Create(DeviceId, DeviceType, 'json');
  try
    // todo: ���������� ������ ������: "Missing or Invalid Device ID"
    Response := FConnection.Post(TSettings.VerifyDeviceLicenseHost,
      Request, TSimpleString, ErrorString) as TSimpleString;
    try
      if (Response <> nil) and (ErrorString = EmptyStr) then
        ErrorString := Response.Value;

      if (Response = nil) and (ErrorString = EmptyStr) then
        ErrorString := 'DeviceLicense fault';
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

function TUserActions.Get(MemberId: integer; out ErrorString: String): TUser;
var
  Parameters: TGenericParameters;
begin
  Parameters := TGenericParameters.Create;
  try
    Parameters.AddParameter('member_id', IntToStr(MemberId));

    Result := FConnection.Get(TSettings.UsersHost,
      Parameters, TUser, ErrorString) as TUser;

    if (Result = nil) and (ErrorString = EmptyStr) then
      ErrorString := 'User details not got';
  finally
    FreeAndNil(Parameters);
  end;
end;

function TUserActions.Get(out ErrorString: String): TUserList;
var
  Parameters: TGenericParameters;
begin
  Parameters := TGenericParameters.Create;
  try
    Result := FConnection.Get(TSettings.GetUsersHost,
      Parameters, TUserList, ErrorString) as TUserList;
    if (Result = nil) then
      Result := TUserList.Create;
    Result.OwnsObjects := True;
  finally
    FreeAndNil(Parameters);
  end;
end;

function TUserActions.IsSessionValid(SessionId, MemberId: integer;
  out ErrorString: String): boolean;
var
  Parameters: TGenericParameters;
  Response: TValidateSessionResponse;
begin
  Result := False;

  Parameters := TGenericParameters.Create;
  try
    Parameters.AddParameter('session_guid', IntToStr(SessionId));
    Parameters.AddParameter('member_id', IntToStr(MemberId));
    Parameters.AddParameter('format', 'json');

    Response := FConnection.Get(TSettings.ValidateSessionHost, Parameters,
      TValidateSessionResponse, ErrorString) as TValidateSessionResponse;
    try
      if (Response <> nil) then
        Result := Response.Authenticated
      else
        if (ErrorString = EmptyStr) then
          ErrorString := 'Session not validated';
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
