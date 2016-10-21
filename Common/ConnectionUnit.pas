unit ConnectionUnit;

interface

uses
  Classes, SysUtils, HTTPApp, IdHTTP,
  IConnectionUnit, GenericParametersUnit, DataObjectUnit;

type
  TConnection = class(TInterfacedObject, IConnection)
  private
    FHttp: TIdHTTP;
    FApiKey: String;

    function InternalPost(Url: String; Request: String; out ErrorString: String): String;
  public
    constructor Create(ApiKey: String);
    destructor Destroy; override;

    procedure SetProxy(Host: String; Port: integer; Username, Password: String);
//    function Post(Data: TGenericParameters; Url: String; out ErrorString: String): boolean; overload;
    function Post(Data: TGenericParameters; Url: String; ResultClassType: TClass; out ErrorString: String): TObject;
  end;

implementation

{ TConnection }

uses SettingsUnit, MarshalUnMarshalUnit;

function TConnection.Post(Data: TGenericParameters; Url: String;
  ResultClassType: TClass; out ErrorString: String): TObject;
var
  Responce: String;
begin
  Url := Url + Data.Serialize(FApiKey);
  Responce := InternalPost(Url, Data.ToJsonString, ErrorString);

  if (Responce = EmptyStr) then
    Result := nil
  else
    Result := TMarshalUnMarshal.FromJson(ResultClassType, Responce);
end;

constructor TConnection.Create(ApiKey: String);
begin
  FApiKey := ApiKey;

  FHttp := TIdHTTP.Create;
  FHttp.ProxyParams.BasicAuthentication := False;

//  FHttp.Timeout := TSettings.DefaultTimeOut;
  FHttp.Request.CharSet := 'utf-8';
  FHttp.Request.ContentType := 'application/json';
end;

destructor TConnection.Destroy;
begin
  FHttp.Free;
  inherited;
end;

function TConnection.InternalPost(Url: String; Request: String;
  out ErrorString: String): String;
var
  ARequest: TStringList;
begin
  ErrorString := EmptyStr;
  ARequest := TStringList.Create;
  try
    ARequest.Text := Request;
    try
      Result := FHttp.Post(Url, ARequest);
    except
      on e: Exception do
        ErrorString := e.Message;
    end;
  finally
    ARequest.Free;
  end;
end;

procedure TConnection.SetProxy(Host: String; Port: integer; Username, Password: String);
begin
  FHttp.ProxyParams.BasicAuthentication := True;
  FHttp.ProxyParams.ProxyPassword := Password;
  FHttp.ProxyParams.ProxyPort := Port;
  FHttp.ProxyParams.ProxyServer := Host;
  FHttp.ProxyParams.ProxyUsername := Username;
end;

end.
