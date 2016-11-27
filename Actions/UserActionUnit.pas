unit UserActionUnit;

interface

uses
  SysUtils, BaseActionUnit,
  DataObjectUnit, GenericParametersUnit, UserUnit, EnumsUnit,
  NullableBasicTypesUnit, UserParametersUnit;

type
  TUserActions = class(TBaseAction)
  public
    function Get(MemberId: integer; out ErrorString: String): TUser; overload;

    function Get(out ErrorString: String): TArray<TUser>; overload;

    /// <summary>
    ///  Authentication of a user with an email and password
    /// </summary>
    /// <returns>
    ///  SessionGuid
    /// </returns>
    function Authentication(Email, Password: String;
      out ErrorString: String): NullableString;

    /// <summary>
    ///  Check if a session is valid.
    /// </summary>
    function IsSessionValid(SessionId: String; MemberId: integer;
      out ErrorString: String): boolean;

    /// <summary>
    ///  Check if a session is valid.
    /// </summary>
    /// <returns>
    ///  Member Id of added user
    /// </returns>
    function RegisterAccount(Plan, Industry, FirstName, LastName, Email: String;
      Terms: boolean; DeviceType: TDeviceType; Password, PasswordConfirmation: String;
      out ErrorString: String): NullableInteger;

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
    ///  Remove existing user from a member’s account.
    /// </summary>
    procedure Remove(MemberId: integer; out ErrorString: String);
  end;

implementation

{ TUserActions }

uses SettingsUnit, ValidateSessionResponseUnit, AddNewUserResponseUnit,
  RegisterAccountResponseUnit, StatusResponseUnit, RemoveUserRequestUnit,
  AuthenticationResponseUnit, CommonTypesUnit;

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

procedure TUserActions.Remove(MemberId: integer; out ErrorString: String);
var
  Request: TRemoveUserRequest;
  Response: TStatusResponse;
begin
  Request := TRemoveUserRequest.Create(MemberId);
  try
    Response := FConnection.Delete(TSettings.UsersHost,
      Request, TStatusResponse, ErrorString) as TStatusResponse;
    try
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
  out ErrorString: String): NullableString;
var
  Parameters: TGenericParameters;
  Response: TObject;
  PossibleResponses: TClassArray;
begin
  Result := NullableString.Null;

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
          Result := TGoodAuthenticationResponse(Response).SessionGuid
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

function TUserActions.Get(out ErrorString: String): TArray<TUser>;
var
  List: TUserList;
  Parameters: TGenericParameters;
begin
  Parameters := TGenericParameters.Create;
  try
    List := FConnection.Get(TSettings.GetUsersHost,
      Parameters, TUserList, ErrorString) as TUserList;
    try
      if (List <> nil) then
        Result := List.ToArray
      else
        SetLength(Result, 0);
    finally
      FreeAndNil(List);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

function TUserActions.IsSessionValid(SessionId: String; MemberId: integer;
  out ErrorString: String): boolean;
var
  Parameters: TGenericParameters;
  Response: TValidateSessionResponse;
begin
  Result := False;

  Parameters := TGenericParameters.Create;
  try
    Parameters.AddParameter('session_guid', SessionId);
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
