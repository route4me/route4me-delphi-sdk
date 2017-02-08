unit GetConfigValueResponseUnit;

interface

uses
  REST.Json.Types, SysUtils,
  NullableBasicTypesUnit, GenericParametersUnit, JSONNullableAttributeUnit, CommonTypesUnit;

type
  TConfigValue = class
  private
    [JSONName('member_id')]
    [Nullable]
    FMemberId: NullableInteger;

    [JSONName('config_key')]
    FConfigKey: String;

    [JSONName('config_value')]
    FConfigValue: String;
  public
    constructor Create;

    property MemberId: NullableInteger read FMemberId;
    property Key: String read FConfigKey;
    property Value: String read FConfigValue;

    function AsStringPair: TStringPair;
  end;
  TConfigValueArray = TArray<TConfigValue>;

  TGetConfigValueResponse = class(TGenericParameters)
  private
    [JSONName('result')]
    [Nullable]
    FResult: NullableString;

    [JSONName('data')]
    [NullableArray(TConfigValue)]
    FConfigValues: TConfigValueArray;
  public
    constructor Create;
    destructor Destroy; override;

    property Result: NullableString read FResult;
    property ConfigValues: TConfigValueArray read FConfigValues;
  end;

implementation

{ TGetConfigValueResponse }

constructor TGetConfigValueResponse.Create;
begin
  FResult := NullableString.Null;

  SetLength(FConfigValues, 0);
end;

destructor TGetConfigValueResponse.Destroy;
var
  i: integer;
begin
  for i := Length(FConfigValues) - 1 downto 0 do
    FreeAndNil(FConfigValues[i]);

  inherited;
end;

{ TConfigValue }

function TConfigValue.AsStringPair: TStringPair;
begin
  Result := TStringPair.Create(FConfigKey, FConfigValue);
end;

constructor TConfigValue.Create;
begin
  FMemberId := NullableInteger.Null;
end;

end.
