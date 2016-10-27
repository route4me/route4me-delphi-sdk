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

    function ExecuteRequest(Url: String; Data: TGenericParameters;
      Method: TRESTRequestMethod; ResultClassType: TClass;
      out ErrorString: String): TObject;
    function InternalRequest(URL: String; Method: TRESTRequestMethod;
      Body: String; out ErrorString: String): TJsonValue;

    function UrlParameters(Parameters: TListStringPair): String;
  protected
    function RunRequest(URL: String; Method: TRESTRequestMethod;
      Body: String; out ErrorString: String): TJsonValue; virtual;
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
begin
  Result := ExecuteRequest(Url, Data, rmDELETE, ResultClassType, ErrorString);
end;

destructor TConnection.Destroy;
begin
  FreeAndNil(FRESTRequest);
  FreeAndNil(FClient);
  FreeAndNil(FRESTResponce);
//  FreeAndNil(FHttp);
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
//  st: TStringList;
  JsonString: String;
begin
  FClient.BaseURL := Url;
  Parameters := Data.Serialize(FApiKey);
  try
    if (Method = rmGET) then
      JsonString := EmptyStr
    else
      JsonString := Data.ToJsonValue.ToString;

    Responce := RunRequest(
      FRESTRequest.Client.BaseURL + UrlParameters(Parameters),
      Method, JsonString, ErrorString);
  finally
    FreeAndNil(Parameters);
  end;

  if (Responce = nil) then
    Result := nil
  else
  begin
{    st := TStringList.Create;
    st.Text := Responce.ToString;
    st.SaveToFile('d:\post.json');
    FreeAndNil(st);}
    Result := TMarshalUnMarshal.FromJson(ResultClassType, Responce);
  end;
end;

function TConnection.RunRequest(URL: String; Method: TRESTRequestMethod;
  Body: String; out ErrorString: String): TJsonValue;
begin
  Result := InternalRequest(URL, Method, Body, ErrorString);
end;

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
// todo:  здесь есть ответ: {"errors":["Point is not allowed for test account"],"timestamp":1477405518}

    ErrorString := FRESTResponce.StatusText;
end;

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
