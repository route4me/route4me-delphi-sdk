unit TestMarshalNullableUnit;

interface

uses
  TestFramework, REST.Json.Types, SysUtils,
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
    private
      FIntValue: integer;
      FBoolValue: boolean;
      FStringValue: String;
      FDoubleValue: double;
      FArrayValue: array of integer;

      [JSONName('nullableobject_null_optional')]
      [NullableObject(TTestObject)]
      FOptionalNullObject: NullableObject;

      [JSONName('nullableobject_null')]
      [NullableObject(TTestObject,True)]
      FNullObject: NullableObject;

      [JSONName('nullableobject_not_null')]
      [NullableObject(TTestObject,True)]
      FNotNullObject: NullableObject;
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
    FreeAndNil(op);
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
    FreeAndNil(op);
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
    FreeAndNil(op);
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
    FreeAndNil(op);
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
    FreeAndNil(op);
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
  FreeAndNil(FTest);

  inherited;
end;

function TTestNullableObjectClass.Etalon: String;
begin
  Result := '{"object_null":null,' +
    '"object_not_null":{"intValue":123,"boolValue":true,"stringValue":"321",' +
    '"doubleValue":123.456,"arrayValue":[3,4,5],"nullableobject_null":null,' +
    '"nullableobject_not_null":{"intValue":111111,"boolValue":false,' +
    '"stringValue":"22222","doubleValue":789.123,"arrayValue":[8],' +
    '"nullableobject_null":null,"nullableobject_not_null":null}}}';
end;

function TTestNullableObjectClass.MakeTestObject: TObject;
var
  Res: TTestObject;
  SubObject: TTestObject;
begin
  Res := TTestObject.Create;
  Res.FIntValue := 123;
  Res.FBoolValue := True;
  Res.FStringValue := '321';
  Res.FDoubleValue := 123.456;
  SetLength(Res.FArrayValue, 3);
  Res.FArrayValue[0] := 3;
  Res.FArrayValue[1] := 4;
  Res.FArrayValue[2] := 5;

  Res.FOptionalNullObject := NullableObject.Null;
  Res.FNullObject := NullableObject.Null;
  SubObject := TTestObject.Create;
  SubObject.FIntValue := 111111;
  SubObject.FBoolValue := False;
  SubObject.FStringValue := '22222';
  SubObject.FDoubleValue := 789.123;
  SetLength(SubObject.FArrayValue, 1);
  SubObject.FArrayValue[0] := 8;
  SubObject.FOptionalNullObject := NullableObject.Null;
  SubObject.FNullObject := NullableObject.Null;
  SubObject.FNotNullObject := NullableObject.Null;
  Res.FNotNullObject := SubObject;

  Result := Res;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('JSON\Marshal\', TTestMarshalNullable.Suite);
end.
