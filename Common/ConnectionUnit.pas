unit ConnectionUnit;

interface

uses
  Classes, SysUtils, System.JSON, IdURI,
  REST.Client, REST.Types, IPPeerCommon, IPPeerClient,
  IConnectionUnit, GenericParametersUnit, DataObjectUnit, CommonTypesUnit;

type
  TConnection = class(TInterfacedObject, IConnection)
  private
    FClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    FApiKey: String;

    function ExecuteRequest(Url: String; Data: TGenericParameters;
      Method: TRESTRequestMethod; ResultClassType: TClass;
      out ErrorString: String): TObject;
    function InternalRequest(URL: String; Method: TRESTRequestMethod;
      Body: String; ContentType: TRESTContentType; out ErrorString: String): TJsonValue;

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

uses SettingsUnit, MarshalUnMarshalUnit;

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

  FRESTResponse := TRESTResponse.Create(nil);
  FClient := TRESTClient.Create(nil);
  FRESTRequest := TRESTRequest.Create(FClient);
  FRESTRequest.Timeout := TSettings.DefaultTimeOutMinutes * 3600;
  FRESTRequest.Response := FRESTResponse;

  FClient.HandleRedirects := False;
  FRESTRequest.HandleRedirects := False;
end;

function TConnection.Delete(Url: String; Data: TGenericParameters;
  ResultClassType: TClass; out ErrorString: String): TObject;
begin
  Result := ExecuteRequest(Url, Data, rmDELETE, ResultClassType, ErrorString);
end;

destructor TConnection.Destroy;
begin
  FreeAndNil(FRESTRequest);
  FreeAndNil(FClient);
  FreeAndNil(FRESTResponse);

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
  FClient.BaseURL := Url;
  Parameters := Data.Serialize(FApiKey);
  try
    Body := EmptyStr;

    ContentType := TRESTContentType.ctTEXT_PLAIN;

    if (Method <> rmGET) then
    begin
      if (Data.BodyParameters.Count > 0) then
      begin
        for Pair in Data.BodyParameters do
          Body := Body + Pair.Key + '=' + {TIdURI.ParamsEncode}(Pair.Value) + '&';
        System.Delete(Body, Length(Body), 1);
        ContentType := TRESTContentType.ctAPPLICATION_X_WWW_FORM_URLENCODED;
      end
      else
        Body := Data.ToJsonValue.ToString;
    end;

    Responce := RunRequest(
      FRESTRequest.Client.BaseURL + UrlParameters(Parameters),
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
  Result := InternalRequest(URL, Method, Body, ContentType, ErrorString);
end;

function TConnection.InternalRequest(URL: String; Method: TRESTRequestMethod;
  Body: String; ContentType: TRESTContentType; out ErrorString: String): TJsonValue;
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
begin
  Result := nil;
  ErrorString := EmptyStr;

  FRESTRequest.Client.BaseURL := URL;
  FRESTRequest.Method := Method;
  FRESTRequest.ClearBody;
  FRESTRequest.AddBody(Body, ContentType);
  if (ContentType = TRESTContentType.ctAPPLICATION_X_WWW_FORM_URLENCODED) then
    FRESTRequest.Params[FRESTRequest.Params.Count - 1].Options :=
      FRESTRequest.Params[FRESTRequest.Params.Count - 1].Options + [TRESTRequestParameterOption.poDoNotEncode];
  FRESTRequest.Execute;
  if (FRESTResponse.StatusCode = 200) then
    Result := FRESTResponse.JSONValue
  else
  if (FRESTResponse.StatusCode = 303) then
  begin
    Result := InternalRequest(
      GetHeaderValue('Location'), TRESTRequestMethod.rmGET, EmptyStr, ContentType, ErrorString);
  end
  else
// todo:  ����� ���� �����: {"errors":["Point is not allowed for test account"],"timestamp":1477405518}

    ErrorString := FRESTResponse.StatusText;
end;

procedure TConnection.SetProxy(Host: String; Port: integer; Username, Password: String);
begin
  FClient.ProxyServer := Host;
  FClient.ProxyPort := Port;
  FClient.ProxyUsername := Username;
  FClient.ProxyPassword := Password;
end;

function TConnection.UrlParameters(Parameters: TListStringPair): String;
var
  Pair: TStringPair;
begin
  Result := EmptyStr;

  for Pair in Parameters do
    Result := Result + Pair.Key + '=' + TIdURI.ParamsEncode(Pair.Value) + '&';

  if (Result <> EmptyStr) then
  begin
    Result := '?' + Result;
    System.Delete(Result, Length(Result), 1);
  end;
end;

end.
