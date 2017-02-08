unit ConfigValueRequestUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit, NullableBasicTypesUnit, JSONNullableAttributeUnit;

type
  TConfigValueRequest = class(TGenericParameters)
  private
    [JSONName('config_key')]
    FKey: String;

    [JSONName('config_value')]
    [Nullable]
    FValue: NullableString;
  public
    constructor Create(Key: String); overload;
    constructor Create(Key, Value: String); overload;

    property Key: String read FKey;
    property Value: NullableString read FValue;
  end;

implementation

{ TConfigValueRequest }

constructor TConfigValueRequest.Create(Key, Value: String);
begin
  Create(Key);

  FValue := Value;
end;

constructor TConfigValueRequest.Create(Key: String);
begin
  inherited Create;

  FKey := Key;
  FValue := NullableString.Null;
end;

end.
