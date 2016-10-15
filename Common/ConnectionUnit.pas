unit ConnectionUnit;

interface

uses IConnectionUnit, GenericParametersUnit;

type
  TConnection = class(TInterfacedObject, IConnection)
  private
  public
    function Post(Data: TGenericParameters; Url: String; out ErrorString: String): boolean; overload;
    function Post<T>(Data: TGenericParameters; Url: String; out ErrorString: String): T; overload;
  end;

implementation

{ TConnection }

uses SettingsUnit;

function TConnection.Post(Data: TGenericParameters; Url: String;
  out ErrorString: String): boolean;
begin
  Url := TSettings.MainHost + Url;
end;

function TConnection.Post<T>(Data: TGenericParameters; Url: String;
  out ErrorString: String): T;
var
  JsonString: String;
begin

end;

end.
