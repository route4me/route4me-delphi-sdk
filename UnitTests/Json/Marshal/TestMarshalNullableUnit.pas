unit TestMarshalNullableUnit;

interface

uses
  TestFramework, REST.Json.Types,
  JSONNullableAttributeUnit,
  GenericParametersUnit,
  NullableBasicTypesUnit, TestBaseJsonMarshalUnit;

type
  TTestNullableBooleanClass = class(TGenericParameters)
  private
    [JSONName('boolean_null')]
    [Nullable(True)]
    FTestNull: NullableBoolean;

    [JSONName('boolean_null_but_not_need_save')]
    [Nullable]
    FTestNullButNotNeedSave: NullableBoolean;

    [JSONName('boolean_not_null')]
    [Nullable]
    FTest: NullableBoolean;
  public
    function Etalon: String;

    property TestNull: NullableBoolean read FTestNull write FTestNull;
    property TestNullButNotNeedSave: NullableBoolean read FTestNullButNotNeedSave write FTestNullButNotNeedSave;
    property Test: NullableBoolean read FTest write FTest;
  end;

  TTestNullableStringClass = class(TGenericParameters)
  private
    [JSONName('string_null')]
    [Nullable(True)]
    FTestNull: NullableString;

    [JSONName('string_null_but_not_need_save')]
    [Nullable]
    FTestNullButNotNeedSave: NullableString;

    [JSONName('string_not_null')]
    [Nullable]
    FTest: NullableString;
  public
    function Etalon: String;

    property TestNull: NullableString read FTestNull write FTestNull;
    property TestNullButNotNeedSave: NullableString read FTestNullButNotNeedSave write FTestNullButNotNeedSave;
    property Test: NullableString read FTest write FTest;
  end;

  TTestNullableIntegerClass = class(TGenericParameters)
  private
    [JSONName('integer_null')]
    [Nullable(True)]
    FTestNull: NullableInteger;

    [JSONName('integer_null_but_not_need_save')]
    [Nullable]
    FTestNullButNotNeedSave: NullableInteger;

    [JSONName('integer_not_null')]
    [Nullable]
    FTest: NullableInteger;
  public
    function Etalon: String;

    property TestNull: NullableInteger read FTestNull write FTestNull;
    property TestNullButNotNeedSave: NullableInteger read FTestNullButNotNeedSave write FTestNullButNotNeedSave;
    property Test: NullableInteger read FTest write FTest;
  end;

  TTestNullableDoubleClass = class(TGenericParameters)
  private
    [JSONName('double_null')]
    [Nullable(True)]
    FTestNull: NullableDouble;

    [JSONName('double_null_but_not_need_save')]
    [Nullable]
    FTestNullButNotNeedSave: NullableDouble;

    [JSONName('double_not_null')]
    [Nullable]
    FTest: NullableDouble;
  public
    function Etalon: String;

    property TestNull: NullableDouble read FTestNull write FTestNull;
    property TestNullButNotNeedSave: NullableDouble read FTestNullButNotNeedSave write FTestNullButNotNeedSave;
    property Test: NullableDouble read FTest write FTest;
  end;

  TTestNullableObjectClass = class(TGenericParameters)
  private
  type
    TTestObject = class
      IntValue: integer;
      BoolValue: boolean;
      StringValue: String;
      DoubleValue: double;
      ArrayValue: array of integer;
    end;
  var
    [JSONName('object_null')]
    [NullableObject(TTestObject,True)]
    FTestNull: NullableObject;

    [JSONName('object_null_but_not_need_save')]
    [NullableObject(TTestObject)]
    FTestNullButNotNeedSave: NullableObject;

    [JSONName('object_not_null')]
    [NullableObject(TTestObject)]
    FTest: NullableObject;
  public
    destructor Destroy; override;

    function MakeTestObject(): TObject;
    function Etalon: String;

    property TestNull: NullableObject read FTestNull write FTestNull;
    property TestNullButNotNeedSave: NullableObject read FTestNullButNotNeedSave write FTestNullButNotNeedSave;
    property Test: NullableObject read FTest write FTest;
  end;

  TTestMarshalNullable = class(TTestCase)
  published
    procedure TestNullableBoolean();
    procedure TestNullableString();
    procedure TestNullableInteger();
    procedure TestNullableDouble();
    procedure TestNullableObject();
  end;

implementation

{ TTestNullableBooleanClass }

function TTestNullableBooleanClass.Etalon: String;
begin
  Result := '{"boolean_null":null,"boolean_not_null":true}';
end;

{ TTestNullableStringClass }

function TTestNullableStringClass.Etalon: String;
begin
  Result := '{"string_null":null,"string_not_null":"123"}';
end;

{ TTestMarshalNullable }

procedure TTestMarshalNullable.TestNullableBoolean;
var
  op: TTestNullableBooleanClass;
begin
  op := TTestNullableBooleanClass.Create;
  try
    op.TestNull := NullableBoolean.Null;
    op.TestNullButNotNeedSave := NullableBoolean.Null;
    op.Test := True;

    CheckEquals(op.Etalon, op.ToJsonString);
  finally
    op.Free;
  end;
end;

procedure TTestMarshalNullable.TestNullableDouble;
var
  op: TTestNullableDoubleClass;
begin
  op := TTestNullableDoubleClass.Create;
  try
    op.TestNull := NullableDouble.Null;
    op.TestNullButNotNeedSave := NullableDouble.Null;
    op.Test := 123.456;

    CheckEquals(op.Etalon, op.ToJsonString);
  finally
    op.Free;
  end;
end;

procedure TTestMarshalNullable.TestNullableInteger;
var
  op: TTestNullableIntegerClass;
begin
  op := TTestNullableIntegerClass.Create;
  try
    op.TestNull := NullableInteger.Null;
    op.TestNullButNotNeedSave := NullableInteger.Null;
    op.Test := 123;

    CheckEquals(op.Etalon, op.ToJsonString);
  finally
    op.Free;
  end;
end;

procedure TTestMarshalNullable.TestNullableObject;
var
  op: TTestNullableObjectClass;
begin
  op := TTestNullableObjectClass.Create;
  try
    op.TestNull := NullableObject.Null;
    op.TestNullButNotNeedSave := NullableObject.Null;
    op.Test := op.MakeTestObject;

    CheckEquals(op.Etalon, op.ToJsonString);
  finally
    op.Free;
  end;
end;

procedure TTestMarshalNullable.TestNullableString;
var
  op: TTestNullableStringClass;
begin
  op := TTestNullableStringClass.Create;
  try
    op.TestNull := NullableString.Null;
    op.TestNullButNotNeedSave := NullableString.Null;
    op.Test := '123';

    CheckEquals(op.Etalon, op.ToJsonString);
  finally
    op.Free;
  end;
end;

{ TTestNullableIntegerClass }

function TTestNullableIntegerClass.Etalon: String;
begin
  Result := '{"integer_null":null,"integer_not_null":123}';
end;

{ TTestNullableDoubleClass }

function TTestNullableDoubleClass.Etalon: String;
begin
  Result := '{"double_null":null,"double_not_null":123.456}';
end;

{ TTestNullableObjectClass }

destructor TTestNullableObjectClass.Destroy;
begin
  FTest.Free;
  inherited;
end;

function TTestNullableObjectClass.Etalon: String;
begin
  Result := '{"object_null":null,"object_not_null":{"intValue":123,"boolValue":true,"stringValue":"321","doubleValue":123.456,"arrayValue":[3,4,5]}}';
end;

function TTestNullableObjectClass.MakeTestObject: TObject;
var
  Res: TTestObject;
begin
  Res := TTestObject.Create;
  Res.IntValue := 123;
  Res.BoolValue := True;
  Res.StringValue := '321';
  Res.DoubleValue := 123.456;
  SetLength(Res.ArrayValue, 3);
  Res.ArrayValue[0] := 3;
  Res.ArrayValue[1] := 4;
  Res.ArrayValue[2] := 5;

  Result := Res;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('JSON\Marshal\', TTestMarshalNullable.Suite);
end.
