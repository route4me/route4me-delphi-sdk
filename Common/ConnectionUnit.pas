unit ConnectionUnit;

interface

uses
  Classes, SysUtils, System.JSON,
  //HTTPApp, IdHTTP,
  REST.Client, REST.Types, IPPeerCommon, IPPeerClient,
  IConnectionUnit, GenericParametersUnit, DataObjectUnit, CommonTypesUnit;

type
  TConnection = class(TInterfacedObject, IConnection)
  private
//    FHttp: TIdHTTP;
    FClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponce: TRESTResponse;
    FApiKey: String;

    function InternalRequest(URL: String; Method: TRESTRequestMethod;
      Body: String; out ErrorString: String): TJsonValue;

    function UrlParameters(Parameters: TListStringPair): String;
  public
    constructor Create(ApiKey: String);
    destructor Destroy; override;

    procedure SetProxy(Host: String; Port: integer; Username, Password: String);

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
var
  Responce: TJSONValue;
  Parameters: TListStringPair;
//  st: TStringList;
  JsonString: String;
begin
  FClient.BaseURL := Url;
  Parameters := Data.Serialize(FApiKey);
  JsonString := Data.ToJsonValue.ToString;

  Responce := InternalRequest(
    FRESTRequest.Client.BaseURL + UrlParameters(Parameters),
    TRESTRequestMethod.rmPOST, JsonString, ErrorString);

  if (Responce = nil) then
    Result := nil
  else
  begin
{    st := TStringList.Create;
    st.Text := Responce.ToString;
    st.SaveToFile('d:\post.json');
    st.Free;}
    Result := TMarshalUnMarshal.FromJson(ResultClassType, Responce);
  end;
end;

function TConnection.Put(Url: String; Data: TGenericParameters;
  ResultClassType: TClass; out ErrorString: String): TObject;
var
  Responce: TJSONValue;
  Parameters: TListStringPair;
//  st: TStringList;
  JsonString: String;
begin
  FClient.BaseURL := Url;
  Parameters := Data.Serialize(FApiKey);
  JsonString := Data.ToJsonValue.ToString;

  Responce := InternalRequest(
    FRESTRequest.Client.BaseURL + UrlParameters(Parameters),
    TRESTRequestMethod.rmPUT, JsonString, ErrorString);

  if (Responce = nil) then
    Result := nil
  else
  begin
{    st := TStringList.Create;
    st.Text := Responce.ToString;
    st.SaveToFile('d:\put.json');
    st.Free;}
    Result := TMarshalUnMarshal.FromJson(ResultClassType, Responce);
  end;
end;

constructor TConnection.Create(ApiKey: String);
begin
  FApiKey := ApiKey;

{  FHttp := TIdHTTP.Create;
  FHttp.ProxyParams.BasicAuthentication := False;

  FHttp.Request.CharSet := 'utf-8';
  FHttp.Request.ContentType := 'application/json';}

  FRESTResponce := TRESTResponse.Create(nil);
  FClient := TRESTClient.Create(nil);
  FRESTRequest := TRESTRequest.Create(FClient);
  FRESTRequest.Timeout := TSettings.DefaultTimeOutMinutes * 3600;
  FRESTRequest.Response := FRESTResponce;

  FClient.HandleRedirects := False;
  FRESTRequest.HandleRedirects := False;
end;

function TConnection.Delete(Url: String; Data: TGenericParameters;
  ResultClassType: TClass; out ErrorString: String): TObject;
var
  Responce: TJSONValue;
  Parameters: TListStringPair;
//  st: TStringList;
  JsonString: String;
begin
  FClient.BaseURL := Url;
  Parameters := Data.Serialize(FApiKey);
  JsonString := Data.ToJsonValue.ToString;

  Responce := InternalRequest(
    FRESTRequest.Client.BaseURL + UrlParameters(Parameters),
    TRESTRequestMethod.rmDELETE, JsonString, ErrorString);

  if (Responce = nil) then
    Result := nil
  else
  begin
{    st := TStringList.Create;
    st.Text := Responce.ToString;
    st.SaveToFile('d:\delete.json');
    st.Free;}
    Result := TMarshalUnMarshal.FromJson(ResultClassType, Responce);
  end;
end;

destructor TConnection.Destroy;
begin
  FRESTRequest.Free;
  FClient.Free;
  FRESTResponce.Free;
//  FHttp.Free;
  inherited;
end;

(*function TConnection.InternalPost(Request: TJSONValue; Parameters: TListStringPair;
  out ErrorString: String): TJsonValue;
begin
  Result := InternalRequest(
    FRESTRequest.Client.BaseURL + UrlParameters(Parameters),
    TRESTRequestMethod.rmPOST, Request.ToString, ErrorString);

{  Result := nil;
  ErrorString := EmptyStr;

  FRESTRequest.Client.BaseURL := FRESTRequest.Client.BaseURL + UrlParameters;
  FRESTRequest.Method := TRESTRequestMethod.rmPOST;
  FRESTRequest.AddBody(Request.ToString, TRESTContentType.ctTEXT_PLAIN);
  FRESTRequest.Execute;
  if (FRESTResponce.StatusCode = 200) then
    Result := FRESTResponce.JSONValue
  else
  if (FRESTResponce.StatusCode = 303) then
    Result := FRESTResponce.JSONValue
  else
    ErrorString := FRESTResponce.StatusText;}
end;
  *)

function TConnection.InternalRequest(URL: String; Method: TRESTRequestMethod;
  Body: String; out ErrorString: String): TJsonValue;
  function GetHeaderValue(Name: String): String;
  var
    s: String;
  begin
    Result := EmptyStr;

    for s in FRESTResponce.Headers do
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
  FRESTRequest.AddBody(Body, TRESTContentType.ctTEXT_PLAIN);
  FRESTRequest.Execute;
  if (FRESTResponce.StatusCode = 200) then
    Result := FRESTResponce.JSONValue
  else
  if (FRESTResponce.StatusCode = 303) then
  begin
    Result := InternalRequest(
      GetHeaderValue('Location'), TRESTRequestMethod.rmGET, EmptyStr, ErrorString);
  end
  else
    ErrorString := FRESTResponce.StatusText;
end;

{function TConnection.InternalPost(Url: String; Request: String;
  out ErrorString: String): TJsonValue;
var
  ARequest: TStringList;
begin
  Result := nil;
  ErrorString := EmptyStr;
  ARequest := TStringList.Create;
  try
    ARequest.Text := Request;
    try
      FRESTRequest.Method := TRESTRequestMethod.rmPOST;
      FRESTRequest.Execute;
      if (FRESTResponce.StatusCode = 200) then
        Result := FRESTResponce.JSONValue
      else
        ErrorString := FRESTResponce.StatusText;

//      Result := FHttp.Post(Url, ARequest);
    except
      on e: Exception do
        ErrorString := e.Message;
    end;
  finally
    ARequest.Free;
  end;
end;}

procedure TConnection.SetProxy(Host: String; Port: integer; Username, Password: String);
begin
{  FHttp.ProxyParams.BasicAuthentication := True;
  FHttp.ProxyParams.ProxyPassword := Password;
  FHttp.ProxyParams.ProxyPort := Port;
  FHttp.ProxyParams.ProxyServer := Host;
  FHttp.ProxyParams.ProxyUsername := Username;}

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
    Result := Result + Pair.Key + '=' + Pair.Value + '&';

  if (Result <> EmptyStr) then
  begin
    Result := '?' + Result;
    System.Delete(Result, Length(Result), 1);
  end;
end;

end.
