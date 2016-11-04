unit ConnectionStubUnit;

interface

uses
  REST.Types, System.JSON,
  ConnectionUnit;

type
  TConnectionStub = class(TConnection)
  private
    const ApiKey = '11111111111111111111111111111111';
  var
    FUrl: String;
    FMethod: TRESTRequestMethod;
    FRequestBody: String;
    FContentType: TRESTContentType;
  protected
    function RunRequest(URL: String; Method: TRESTRequestMethod;
      RequestBody: String; ContentType: TRESTContentType; out ErrorString: String): TJsonValue; override;
  public
    constructor Create(); reintroduce;

    property Url: String read FUrl;
    property Method: TRESTRequestMethod read FMethod;
    property RequestBody: String read FRequestBody;
    property ContentType: TRESTContentType read FContentType;
  end;

implementation

{ TConnectionStub }

constructor TConnectionStub.Create;
begin
  Inherited Create(ApiKey);
end;

function TConnectionStub.RunRequest(URL: String;
  Method: TRESTRequestMethod; RequestBody: String; ContentType: TRESTContentType;
  out ErrorString: String): TJsonValue;
begin
  FUrl := URL;
  FMethod := Method;
  FRequestBody := RequestBody;
  FContentType := ContentType;

  Result := nil;
end;

end.
