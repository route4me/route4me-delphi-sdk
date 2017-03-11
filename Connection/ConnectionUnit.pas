unit ConnectionUnit;

interface

uses
  Classes, SysUtils, System.JSON, IdURI, System.Generics.Collections,
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

    FCollectionForDeleting: TObjectList<TJsonValue>;

    procedure RESTRequest(URL: String; Method: TRESTRequestMethod;
      Body: String; ContentType: TRESTContentType;
      out Success: boolean; out StatusCode: integer;
      out JSONValue: TJsonValue; out ResponseContent: String); overload;
    procedure RESTRequest(URL: String; Stream: TStream;
      out Success: boolean; out StatusCode: integer;
      out JSONValue: TJsonValue; out ResponseContent: String); overload;
    procedure DeleteRequest(URL: String;
      Body: String; ContentType: TRESTContentType;
      out Success: boolean; out StatusCode: integer;
      out JSONValue: TJsonValue; out ResponseContent: String);
  public
    constructor Create();
    destructor Destroy; override;

    function ExecuteRequest(URL: String; Method: TRESTRequestMethod;
      Body: String; ContentType: TRESTContentType; out ErrorString: String;
      out ResponseAsString: String): TJsonValue; overload;

    function ExecuteRequest(URL: String;  Stream: TStream; out ErrorString: String;
      out ResponseAsString: String): TJsonValue; overload;

    procedure SetProxy(Host: String; Port: integer; Username, Password: String);
  end;

  TConnection = class(TInterfacedObject, IConnection)
  private
    FConnection: TConnectionFacade;
    FApiKey: String;

    function ExecuteRequest(Url: String; Data: TGenericParameters;
      Method: TRESTRequestMethod; PossibleResultClassType: TClassArray;
      out ErrorString: String): TObject; overload;

    function ExecuteRequest(Url: String; Data: TGenericParameters;
      Stream: TStream; ResultClassType: TClass;
      out ErrorString: String): TObject; overload;

    function UrlParameters(Parameters: TListStringPair): String;
  protected
    function RunRequest(URL: String; Method: TRESTRequestMethod;
      Body: String; ContentType: TRESTContentType; out ErrorString: String;
      out ResponseAsString: String): TJsonValue; overload; virtual;
    function RunRequest(URL: String; Stream: TStream; out ErrorString: String;
      out ResponseAsString: String): TJsonValue; overload; virtual;
  public
    constructor Create(ApiKey: String);
    destructor Destroy; override;

    procedure SetProxy(Host: String; Port: integer; Username, Password: String);

    function Get(Url: String; Data: TGenericParameters;
      ResultClassType: TClass; out ErrorString: String): TObject; overload;
    function Get(Url: String; Data: TGenericParameters;
      PossibleResultClassType: TClassArray; out ErrorString: String): TObject; overload;
    function Post(Url: String; Data: TGenericParameters;
      ResultClassType: TClass; out ErrorString: String): TObject; overload;
    function Post(Url: String; Data: TGenericParameters;
      PossibleResultClassType: TClassArray; out ErrorString: String): TObject; overload;
    function Post(Url: String; Data: TGenericParameters; Stream: TStream;
      ResultClassType: TClass; out ErrorString: String): TObject; overload;
    function Put(Url: String; Data: TGenericParameters;
      ResultClassType: TClass; out ErrorString: String): TObject;
    function Delete(Url: String; Data: TGenericParameters;
      ResultClassType: TClass; out ErrorString: String): TObject;
  end;

implementation

{ TConnection }

uses SettingsUnit, MarshalUnMarshalUnit, ErrorResponseUnit, UtilsUnit;

function TConnection.Post(Url: String; Data: TGenericParameters;
  ResultClassType: TClass; out ErrorString: String): TObject;
var
  PossibleResultClassType: TClassArray;
begin
  PossibleResultClassType := [ResultClassType];
  Result := ExecuteRequest(Url, Data, rmPOST, PossibleResultClassType, ErrorString);
end;

function TConnection.Post(Url: String; Data: TGenericParameters;
  PossibleResultClassType: TClassArray; out ErrorString: String): TObject;
begin
  Result := ExecuteRequest(Url, Data, rmPOST, PossibleResultClassType, ErrorString);
end;

function TConnection.Post(Url: String; Data: TGenericParameters; Stream: TStream;
  ResultClassType: TClass; out ErrorString: String): TObject;
begin
  Result := ExecuteRequest(Url, Data, Stream, ResultClassType, ErrorString);
end;

function TConnection.Put(Url: String; Data: TGenericParameters;
  ResultClassType: TClass; out ErrorString: String): TObject;
var
  PossibleResultClassType: TClassArray;
begin
  PossibleResultClassType := [ResultClassType];
  Result := ExecuteRequest(Url, Data, rmPUT, PossibleResultClassType, ErrorString);
end;

function TConnection.RunRequest(URL: String; Stream: TStream; out ErrorString,
  ResponseAsString: String): TJsonValue;
begin
  Result := FConnection.ExecuteRequest(URL, Stream, ErrorString, ResponseAsString);
end;

constructor TConnection.Create(ApiKey: String);
begin
  FApiKey := ApiKey;
  FConnection := TConnectionFacade.Create();
end;

function TConnection.Delete(Url: String; Data: TGenericParameters;
  ResultClassType: TClass; out ErrorString: String): TObject;
var
  PossibleResultClassType: TClassArray;
begin
  PossibleResultClassType := [ResultClassType];
  Result := ExecuteRequest(Url, Data, rmDELETE, PossibleResultClassType, ErrorString);
end;

destructor TConnection.Destroy;
begin
  FreeAndNil(FConnection);

  inherited;
end;

function TConnection.ExecuteRequest(Url: String; Data: TGenericParameters;
  Stream: TStream; ResultClassType: TClass; out ErrorString: String): TObject;
var
  Response: TJSONValue;
  Parameters: TListStringPair;
  ResponseAsString: String;
begin
  Result := nil;

  Parameters := Data.Serialize(FApiKey);
  try
    Response := RunRequest(Url + UrlParameters(Parameters),
      Stream, ErrorString, ResponseAsString);

    if (Response <> nil) then
      Result := TMarshalUnMarshal.FromJson(ResultClassType, Response)
  finally
    FreeAndNil(Parameters);
  end;
end;

function TConnection.Get(Url: String; Data: TGenericParameters;
  PossibleResultClassType: TClassArray; out ErrorString: String): TObject;
begin
  Result := ExecuteRequest(Url, Data, rmGET, PossibleResultClassType, ErrorString);
end;

function TConnection.Get(Url: String; Data: TGenericParameters;
  ResultClassType: TClass; out ErrorString: String): TObject;
var
  PossibleResultClassType: TClassArray;
begin
  PossibleResultClassType := [ResultClassType];
  Result := ExecuteRequest(Url, Data, rmGET, PossibleResultClassType, ErrorString);
end;

function TConnection.ExecuteRequest(Url: String; Data: TGenericParameters;
  Method: TRESTRequestMethod; PossibleResultClassType: TClassArray;
  out ErrorString: String): TObject;
var
  Response: TJSONValue;
  Parameters: TListStringPair;
  Body: String;
  Pair: TStringPair;
  ContentType: TRESTContentType;
  ResultClassType: TClass;
  IsError: boolean;
  ResponseAsString: String;
  JsonValue: TJsonValue;
begin
  Result := nil;

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
      begin
        JsonValue := Data.ToJsonValue;
        try
          Body := JsonValue.ToString;
        finally
          FreeAndNil(JsonValue);
        end;
      end;
    end;

    Response := RunRequest(
      Url + UrlParameters(Parameters),
      Method, Body, ContentType, ErrorString, ResponseAsString);

    for ResultClassType in PossibleResultClassType do
    begin
      if (ResultClassType = TSimpleString) then
      begin
        Result := TSimpleString.Create(ResponseAsString);
        Break;
      end;

      IsError := False;
      try
        if (Response <> nil) then
          Result := TMarshalUnMarshal.FromJson(ResultClassType, Response)
      except
        IsError := True;
      end;
      if not IsError then
        Break;
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

function TConnection.RunRequest(URL: String; Method: TRESTRequestMethod;
  Body: String; ContentType: TRESTContentType; out ErrorString: String;
  out ResponseAsString: String): TJsonValue;
begin
  Result := FConnection.ExecuteRequest(URL, Method, Body, ContentType,
    ErrorString, ResponseAsString);
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
    Result := Result + Pair.Key + '=' + Pair.Value + '&';

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

  FCollectionForDeleting := TObjectList<TJsonValue>.Create;
end;

destructor TConnectionFacade.Destroy;
begin
  FreeAndNil(FCollectionForDeleting);
  FreeAndNil(FRESTRequest);
  FreeAndNil(FClient);
  FreeAndNil(FRESTResponse);
  FreeAndNil(FClient2);

  inherited;
end;

function TConnectionFacade.ExecuteRequest(URL: String; Stream: TStream;
  out ErrorString, ResponseAsString: String): TJsonValue;
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
  JsonResponse: TJsonValue;

  Success: boolean;
  StatusCode: integer;
  JSONValue: TJsonValue;
begin
  Result := nil;
  ErrorString := EmptyStr;

  FClient.BaseURL := URL;

  RESTRequest(URL, Stream, Success, StatusCode, JSONValue, ResponseAsString);

  if (Success) then
    Result := JSONValue
  else
  begin
    ErrorString := EmptyStr;
    JsonResponse := TJSONObject.ParseJSONValue(ResponseAsString);
    if (JsonResponse <> nil) then
    begin
      ErrorResponse := TMarshalUnMarshal.FromJson(TErrorResponse, JsonResponse) as TErrorResponse;
      try
        if (ErrorResponse <> nil) then
          for Error in ErrorResponse.Errors do
          begin
            if (Length(ErrorString) > 0) then
              ErrorString := ErrorString + '; ';
            ErrorString := ErrorString + Error;
          end;
      finally
        FreeAndNil(ErrorResponse);
      end;
    end
    else
      ErrorString := 'Response: ' + FRESTResponse.StatusText;
  end;
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
  out ErrorString: String; out ResponseAsString: String): TJsonValue;
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
  JsonResponse: TJsonValue;

  Success: boolean;
  StatusCode: integer;
  JSONValue: TJsonValue;
begin
  Result := nil;
  ErrorString := EmptyStr;

  FClient.BaseURL := URL;
  if (Method = TRESTRequestMethod.rmDELETE) and (Body <> EmptyStr) then
  begin
    DeleteRequest(URL, Body, ContentType,
      Success, StatusCode, JSONValue, ResponseAsString);
    FCollectionForDeleting.Add(JSONValue);
  end
  else
  begin
    RESTRequest(URL, Method, Body, ContentType,
      Success, StatusCode, JSONValue, ResponseAsString);
  end;

  if (Success) then
    Result := JSONValue
  else
  begin
    if (StatusCode = 303) then
      Result := ExecuteRequest(
        GetHeaderValue('Location'), TRESTRequestMethod.rmGET, EmptyStr,
          ContentType, ErrorString, ResponseAsString)
    else
    begin
      ErrorString := EmptyStr;
      JsonResponse := TJSONObject.ParseJSONValue(ResponseAsString);
      if (JsonResponse <> nil) then
      begin
        ErrorResponse := TMarshalUnMarshal.FromJson(TErrorResponse, JsonResponse) as TErrorResponse;
        try
          if (ErrorResponse <> nil) then
            for Error in ErrorResponse.Errors do
            begin
              if (Length(ErrorString) > 0) then
                ErrorString := ErrorString + '; ';
              ErrorString := ErrorString + Error;
            end;
        finally
          FreeAndNil(ErrorResponse);
        end;
      end
      else
        ErrorString := 'Response: ' + FRESTResponse.StatusText;
    end;
  end;
end;

procedure TConnectionFacade.RESTRequest(URL: String; Stream: TStream;
  out Success: boolean; out StatusCode: integer; out JSONValue: TJsonValue;
  out ResponseContent: String);
begin
  FRESTRequest.Client.BaseURL := URL;
  FRESTRequest.Method := rmPOST;
  FRESTRequest.ClearBody;
  FRESTRequest.AddBody(Stream, TRESTContentType.ctMULTIPART_FORM_DATA);

  FRESTRequest.Execute;

  Success := FRESTResponse.Status.Success;
  StatusCode := FRESTResponse.StatusCode;
  JSONValue := FRESTResponse.JSONValue;
  ResponseContent := FRESTResponse.Content;
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
