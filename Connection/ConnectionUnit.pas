unit ConnectionUnit;

interface

uses
  Classes, SysUtils, System.JSON, IdURI,
  REST.Client, REST.Types, IPPeerCommon, IPPeerClient,
  System.Net.HttpClient, System.NetConsts, System.Net.URLClient, System.Net.HttpClient.Win,
  IConnectionUnit, GenericParametersUnit, DataObjectUnit, CommonTypesUnit;

type
  TConnectionFacade = class
  strict private
    FClient2: THTTPClient;

    FClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;

    procedure RESTRequest(URL: String; Method: TRESTRequestMethod;
      Body: String; ContentType: TRESTContentType;
      out Success: boolean; out StatusCode: integer;
      out JSONValue: TJsonValue; out ResponseContent: String);
    procedure DeleteRequest(URL: String;
      Body: String; ContentType: TRESTContentType;
      out Success: boolean; out StatusCode: integer;
      out JSONValue: TJsonValue; out ResponseContent: String);
  public
    constructor Create();
    destructor Destroy; override;

    function ExecuteRequest(URL: String; Method: TRESTRequestMethod;
      Body: String; ContentType: TRESTContentType; out ErrorString: String): TJsonValue;

    procedure SetProxy(Host: String; Port: integer; Username, Password: String);
  end;

  TConnection = class(TInterfacedObject, IConnection)
  private
    FConnection: TConnectionFacade;
    FApiKey: String;

    function ExecuteRequest(Url: String; Data: TGenericParameters;
      Method: TRESTRequestMethod; ResultClassType: TClass;
      out ErrorString: String): TObject;

    function UrlParameters(Parameters: TListStringPair): String;
  protected
    function RunRequest(URL: String; Method: TRESTRequestMethod;
      Body: String; ContentType: TRESTContentType; out ErrorString: String): TJsonValue; virtual;
  public
    constructor Create(ApiKey: String);
    destructor Destroy; override;

    procedure SetProxy(Host: String; Port: integer; Username, Password: String);

    function Get(Url: String; Data: TGenericParameters;
      ResultClassType: TClass; out ErrorString: String): TObject;
    function Post(Url: String; Data: TGenericParameters;
      ResultClassType: TClass; out ErrorString: String): TObject;
    function Put(Url: String; Data: TGenericParameters;
      ResultClassType: TClass; out ErrorString: String): TObject;
    function Delete(Url: String; Data: TGenericParameters;
      ResultClassType: TClass; out ErrorString: String): TObject;
  end;

implementation

{ TConnection }

uses SettingsUnit, MarshalUnMarshalUnit, ErrorResponseUnit;

function TConnection.Post(Url: String; Data: TGenericParameters;
  ResultClassType: TClass; out ErrorString: String): TObject;
begin
  Result := ExecuteRequest(Url, Data, rmPOST, ResultClassType, ErrorString);
end;

function TConnection.Put(Url: String; Data: TGenericParameters;
  ResultClassType: TClass; out ErrorString: String): TObject;
begin
  Result := ExecuteRequest(Url, Data, rmPUT, ResultClassType, ErrorString);
end;

constructor TConnection.Create(ApiKey: String);
begin
  FApiKey := ApiKey;
  FConnection := TConnectionFacade.Create();
end;

function TConnection.Delete(Url: String; Data: TGenericParameters;
  ResultClassType: TClass; out ErrorString: String): TObject;
begin
  Result := ExecuteRequest(Url, Data, rmDELETE, ResultClassType, ErrorString);
end;

destructor TConnection.Destroy;
begin
  FreeAndNil(FConnection);

  inherited;
end;

function TConnection.Get(Url: String; Data: TGenericParameters;
  ResultClassType: TClass; out ErrorString: String): TObject;
begin
  Result := ExecuteRequest(Url, Data, rmGET, ResultClassType, ErrorString);
end;

function TConnection.ExecuteRequest(Url: String; Data: TGenericParameters;
  Method: TRESTRequestMethod; ResultClassType: TClass;
  out ErrorString: String): TObject;
var
  Responce: TJSONValue;
  Parameters: TListStringPair;
  st: TStringList;
  Body: String;
  Pair: TStringPair;
  ContentType: TRESTContentType;
begin
  Parameters := Data.Serialize(FApiKey);
  try
    Body := EmptyStr;

    ContentType := TRESTContentType.ctTEXT_PLAIN;

    if (Method <> rmGET) then
    begin
      if (Data.BodyParameters.Count > 0) then
      begin
        for Pair in Data.BodyParameters do
          Body := Body + Pair.Key + '=' + Pair.Value + '&';
        System.Delete(Body, Length(Body), 1);
        ContentType := TRESTContentType.ctAPPLICATION_X_WWW_FORM_URLENCODED;
      end
      else
        Body := Data.ToJsonValue.ToString;
    end;

    Responce := RunRequest(
      Url + UrlParameters(Parameters),
      Method, Body, ContentType, ErrorString);
  finally
    FreeAndNil(Parameters);
  end;

  if (Responce = nil) then
    Result := nil
  else
  begin
    st := TStringList.Create;
    st.Text := Responce.ToString;
//    st.SaveToFile('d:\post.json');
    FreeAndNil(st);
    Result := TMarshalUnMarshal.FromJson(ResultClassType, Responce);
  end;
end;

function TConnection.RunRequest(URL: String; Method: TRESTRequestMethod;
  Body: String; ContentType: TRESTContentType; out ErrorString: String): TJsonValue;
begin
  Result := FConnection.ExecuteRequest(URL, Method, Body, ContentType, ErrorString);
end;

procedure TConnection.SetProxy(Host: String; Port: integer; Username, Password: String);
begin
  FConnection.SetProxy(Host, Port, Username, Password);
end;

function TConnection.UrlParameters(Parameters: TListStringPair): String;
var
  Pair: TStringPair;
begin
  Result := EmptyStr;

  for Pair in Parameters do
    Result := Result + Pair.Key + '=' + EncodeURL(Pair.Value) + '&';

  if (Result <> EmptyStr) then
  begin
    Result := '?' + Result;
    System.Delete(Result, Length(Result), 1);
  end;
end;

{ TConnectionFacade }

constructor TConnectionFacade.Create;
begin
  FRESTResponse := TRESTResponse.Create(nil);
  FClient := TRESTClient.Create(nil);
  FRESTRequest := TRESTRequest.Create(FClient);
  FRESTRequest.Timeout := TSettings.DefaultTimeOutMinutes * 3600;
  FRESTRequest.Response := FRESTResponse;

  FClient.HandleRedirects := False;
  FRESTRequest.HandleRedirects := False;

  FClient2 := THttpClient.Create;
  FClient2.HandleRedirects := False;
end;

destructor TConnectionFacade.Destroy;
begin
  FreeAndNil(FRESTRequest);
  FreeAndNil(FClient);
  FreeAndNil(FRESTResponse);
  FreeAndNil(FClient2);

  inherited;
end;

procedure TConnectionFacade.DeleteRequest(URL, Body: String;
  ContentType: TRESTContentType; out Success: boolean; out StatusCode: integer;
  out JSONValue: TJsonValue; out ResponseContent: String);
var
  Request: IHTTPRequest;
  AResponseContent: TStream;
  ABodyStream: TStream;
  Response: IHTTPResponse;
  LBuffer: TBytes;
begin
  ABodyStream := TStringStream.Create;
  try
    LBuffer := TEncoding.UTF8.GetBytes(Body);
    TStringStream(ABodyStream).WriteData(LBuffer, Length(LBuffer));
    ABodyStream.Position := 0;

    Request := FClient2.GetRequest(sHTTPMethodDelete, URL);
    Request.SourceStream := ABodyStream;

    try
      AResponseContent := nil;
      Response := FClient2.Execute(Request, AResponseContent);

      StatusCode := Response.StatusCode;
      Success := (StatusCode >= 200) and (StatusCode < 300);

      ResponseContent := Response.ContentAsString();
      if (ResponseContent <> EmptyStr) then
        JSONValue := TJSONObject.ParseJSONValue(ResponseContent)
      else
        JSONValue := nil;
    except
      on e: Exception do
        raise Exception.Create('DeleteRequest exception: ' + e.Message);
    end;
  finally
    FreeAndNil(ABodyStream);
  end;
end;

function TConnectionFacade.ExecuteRequest(URL: String;
  Method: TRESTRequestMethod; Body: String; ContentType: TRESTContentType;
  out ErrorString: String): TJsonValue;
  function GetHeaderValue(Name: String): String;
  var
    s: String;
  begin
    Result := EmptyStr;

    for s in FRESTResponse.Headers do
      if s.StartsWith(Name, True) then
      begin
        Result := s;
        System.Delete(Result, 1, Length(Name) + 1);
        Break;
      end;
  end;
var
  ErrorResponse: TErrorResponse;
  Error: String;
  JsonResponce: TJsonValue;

  Success: boolean;
  StatusCode: integer;
  JSONValue: TJsonValue;
  ResponseContent: String;
begin
  Result := nil;
  ErrorString := EmptyStr;

  FClient.BaseURL := URL;
  if (Method = TRESTRequestMethod.rmDELETE) and (Body <> EmptyStr) then
    DeleteRequest(URL, Body, ContentType,
      Success, StatusCode, JSONValue, ResponseContent)
  else
    RESTRequest(URL, Method, Body, ContentType,
      Success, StatusCode, JSONValue, ResponseContent);

  if (Success) then
    Result := JSONValue
  else
  if (StatusCode = 303) then
    Result := ExecuteRequest(
      GetHeaderValue('Location'), TRESTRequestMethod.rmGET, EmptyStr, ContentType, ErrorString)
  else
  begin
    ErrorString := EmptyStr;
    JsonResponce := TJSONObject.ParseJSONValue(ResponseContent);
    if (JsonResponce <> nil) then
    begin
      ErrorResponse := TMarshalUnMarshal.FromJson(TErrorResponse, JsonResponce) as TErrorResponse;
      if (ErrorResponse <> nil) then
        for Error in ErrorResponse.Errors do
        begin
          if (Length(ErrorString) > 0) then
            ErrorString := ErrorString + '; ';
          ErrorString := ErrorString + Error;
        end;
    end
    else
      ErrorString := 'Response: ' + FRESTResponse.StatusText;
  end;
end;

procedure TConnectionFacade.RESTRequest(URL: String;
  Method: TRESTRequestMethod; Body: String; ContentType: TRESTContentType;
  out Success: boolean; out StatusCode: integer; out JSONValue: TJsonValue;
  out ResponseContent: String);
begin
  FRESTRequest.Client.BaseURL := URL;
  FRESTRequest.Method := Method;
  FRESTRequest.ClearBody;
  FRESTRequest.AddBody(Body, ContentType);
  if (ContentType = TRESTContentType.ctAPPLICATION_X_WWW_FORM_URLENCODED) then
    FRESTRequest.Params[FRESTRequest.Params.Count - 1].Options :=
      FRESTRequest.Params[FRESTRequest.Params.Count - 1].Options + [TRESTRequestParameterOption.poDoNotEncode];

  FRESTRequest.Execute;

  Success := FRESTResponse.Status.Success;
  StatusCode := FRESTResponse.StatusCode;
  JSONValue := FRESTResponse.JSONValue;
  ResponseContent := FRESTResponse.Content;
end;

procedure TConnectionFacade.SetProxy(Host: String; Port: integer; Username,
  Password: String);
begin
  FClient.ProxyServer := Host;
  FClient.ProxyPort := Port;
  FClient.ProxyUsername := Username;
  FClient.ProxyPassword := Password;

  FClient2.ProxySettings := TProxySettings.Create(Host, Port, Username, Password);
end;

end.
