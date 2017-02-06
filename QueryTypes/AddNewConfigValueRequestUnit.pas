unit AddNewConfigValueRequestUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit;

type
  TAddNewConfigValueRequest = class(TGenericParameters)
  private
    [JSONName('config_key')]
    FKey: String;

    [JSONName('config_value')]
    FValue: String;
  public
    constructor Create(Key, Value: String); reintroduce;

    property Key: String read FKey;
    property Value: String read FValue;
  end;

implementation

{ TAddNewConfigValueRequest }

constructor TAddNewConfigValueRequest.Create(Key, Value: String);
begin
  inherited Create;

  FKey := Key;
  FValue := Value;
end;

end.
