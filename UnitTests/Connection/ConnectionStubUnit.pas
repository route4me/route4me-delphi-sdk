unit ConnectionStubUnit;

interface

uses
  REST.Types, System.JSON, Classes, ConnectionUnit;

type
  TConnectionStub = class(TConnection)
  private
    const ApiKey = '11111111111111111111111111111111';
  var
    FUrl: String;
    FMethod: TRESTRequestMethod;
    FRequestBody: String;
    FContentType: TRESTContentType;
    FStream: TStream;
  protected
    function RunRequest(URL: String; Method: TRESTRequestMethod;
      RequestBody: String; ContentType: TRESTContentType;
      out ErrorString: String; out ResponseAsString: String): TJsonValue; override;
    function RunRequest(URL: String; Stream: TStream; out ErrorString: String;
      out ResponseAsString: String): TJsonValue; overload; virtual;
  public
    constructor Create(); reintroduce;

    property Url: String read FUrl;
    property Method: TRESTRequestMethod read FMethod;
    property RequestBody: String read FRequestBody;
    property ContentType: TRESTContentType read FContentType;
    property Stream: TStream read FStream;
  end;

implementation

{ TConnectionStub }

constructor TConnectionStub.Create;
begin
  Inherited Create(ApiKey);
  FStream := nil;
end;

function TConnectionStub.RunRequest(URL: String;
  Method: TRESTRequestMethod; RequestBody: String; ContentType: TRESTContentType;
  out ErrorString: String; out ResponseAsString: String{; out NeedFreeResult: boolean}): TJsonValue;
begin
  FUrl := URL;
  FMethod := Method;
  FRequestBody := RequestBody;
  FContentType := ContentType;
  FStream := nil;

  Result := nil;
end;

function TConnectionStub.RunRequest(URL: String; Stream: TStream;
  out ErrorString, ResponseAsString: String): TJsonValue;
begin
  FUrl := URL;
  FMethod := rmPOST;
  FRequestBody := '';
  FContentType := TRESTContentType.ctMULTIPART_FORM_DATA;
  FStream := Stream;

  Result := nil;
end;

end.
