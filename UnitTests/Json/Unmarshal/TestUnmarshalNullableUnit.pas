unit TestUnmarshalNullableUnit;

interface

uses
  TestFramework, REST.Json.Types,
  JSONNullableAttributeUnit,
  GenericParametersUnit,
  NullableBasicTypesUnit;

type
  TTestNullableBooleanClass = class(TGenericParameters)
  private
    [JSONName('boolean_null')]
    [JSONNullable(True)]
    FTestNull: NullableBoolean;

    [JSONName('boolean_null_but_not_need_save')]
    [JSONNullable]
    FTestNullButNotNeedSave: NullableBoolean;

    [JSONName('boolean_not_null')]
    [JSONNullable]
    FTest: NullableBoolean;
  public
    class function AsJson: String;

    function Equals(Obj: TObject): Boolean; override;

    property TestNull: NullableBoolean read FTestNull write FTestNull;
    property TestNullButNotNeedSave: NullableBoolean read FTestNullButNotNeedSave write FTestNullButNotNeedSave;
    property Test: NullableBoolean read FTest write FTest;
  end;

  TTestUnmarshalNullableStringClass = class(TGenericParameters)
  private
    [JSONName('string_null')]
    [JSONNullable(True)]
    FTestNull: NullableString;

    [JSONName('string_null_but_not_need_save')]
    [JSONNullable]
    FTestNullButNotNeedSave: NullableString;

    [JSONName('string_not_null')]
    [JSONNullable]
    FTest: NullableString;

    FDebug: String;
  public
    constructor Create;

    class function AsJson: String;

    property TestNull: NullableString read FTestNull write FTestNull;
    property TestNullButNotNeedSave: NullableString read FTestNullButNotNeedSave write FTestNullButNotNeedSave;
    property Test: NullableString read FTest write FTest;
  end;

  TTestNullableIntegerClass = class(TGenericParameters)
  private
    [JSONName('integer_null')]
    [JSONNullable(True)]
    FTestNull: NullableInteger;

    [JSONName('integer_null_but_not_need_save')]
    [JSONNullable]
    FTestNullButNotNeedSave: NullableInteger;

    [JSONName('integer_not_null')]
    [JSONNullable]
    FTest: NullableInteger;
  public
    class function AsJson: String;

    property TestNull: NullableInteger read FTestNull write FTestNull;
    property TestNullButNotNeedSave: NullableInteger read FTestNullButNotNeedSave write FTestNullButNotNeedSave;
    property Test: NullableInteger read FTest write FTest;
  end;

  TTestNullableInt64Class = class(TGenericParameters)
  private
    [JSONName('int64_null')]
    [JSONNullable(True)]
    FTestNull: NullableInt64;

    [JSONName('int64_null_but_not_need_save')]
    [JSONNullable]
    FTestNullButNotNeedSave: NullableInt64;

    [JSONName('int64_not_null')]
    [JSONNullable]
    FTest: NullableInt64;
  public
    class function AsJson: String;

    property TestNull: NullableInt64 read FTestNull write FTestNull;
    property TestNullButNotNeedSave: NullableInt64 read FTestNullButNotNeedSave write FTestNullButNotNeedSave;
    property Test: NullableInt64 read FTest write FTest;
  end;

  TTestNullableDoubleClass = class(TGenericParameters)
  private
    [JSONName('double_null')]
    [JSONNullable(True)]
    FTestNull: NullableDouble;

    [JSONName('double_null_but_not_need_save')]
    [JSONNullable]
    FTestNullButNotNeedSave: NullableDouble;

    [JSONName('double_not_null')]
    [JSONNullable]
    FTest: NullableDouble;
  public
    class function AsJson: String;

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
    [JSONNullable(True)]
    FTestNull: NullableObject;

    [JSONName('object_null_but_not_need_save')]
    [JSONNullable]
    FTestNullButNotNeedSave: NullableObject;

    [JSONName('object_not_null')]
    [JSONNullable]
    FTest: NullableObject;
  public
    destructor Destroy; override;

    function MakeTestObject(): TObject;
    class function AsJson: String;

    property TestNull: NullableObject read FTestNull write FTestNull;
    property TestNullButNotNeedSave: NullableObject read FTestNullButNotNeedSave write FTestNullButNotNeedSave;
    property Test: NullableObject read FTest write FTest;
  end;

  TTestUnmarshalNullable = class(TTestCase)
  published
    procedure TestNullableBoolean();
    procedure TestNullableString();
    procedure TestNullableInteger();
    procedure TestNullableInt64();
    procedure TestNullableDouble();
    procedure TestNullableObject();
  end;

implementation

{ TTestNullableBooleanClass }

uses MarshalUnMarshalUnit;

function TTestNullableBooleanClass.Equals(Obj: TObject): Boolean;
var
  Other: TTestNullableBooleanClass;
  Res: boolean;
begin
  Result := False;

  if not (Obj is TTestNullableBooleanClass) then
    Exit;

  Other := TTestNullableBooleanClass(Obj);

  Result :=
    (TestNull = Other.TestNull) and
    (TestNullButNotNeedSave = Other.TestNullButNotNeedSave) and
    (Test = Other.Test);
end;

class function TTestNullableBooleanClass.AsJson: String;
begin
  Result := '{"boolean_null":null,"boolean_not_null":true}';
end;

{ TTestNullableStringClass }

class function TTestUnmarshalNullableStringClass.AsJson: String;
begin
  Result := '{"string_null":null,"string_not_null":"123"}';
end;

constructor TTestUnmarshalNullableStringClass.Create;
begin
    FTestNull := NullableString.Null;
    FTestNullButNotNeedSave := NullableString.Null;
    FTest := NullableString.Null;
//    FTest := 'asd';
end;

{ TTestUnmarshalNullable }

procedure TTestUnmarshalNullable.TestNullableBoolean;
var
  Etalon: TTestNullableBooleanClass;
  Actual: TTestNullableBooleanClass;
  Obj: TObject;
begin
  Obj := TMarshalUnMarshal.FromJson(TTestNullableBooleanClass, TTestNullableBooleanClass.AsJson);
  CheckIs(Obj, TTestNullableBooleanClass);

  Actual := Obj as TTestNullableBooleanClass;

  Etalon := TTestNullableBooleanClass.Create;
  try
    Etalon.TestNull := NullableBoolean.Null;
    Etalon.TestNullButNotNeedSave := NullableBoolean.Null;
    Etalon.Test := True;

    CheckTrue(Etalon.Equals(Actual));
  finally
    Etalon.Free;
  end;
end;

procedure TTestUnmarshalNullable.TestNullableDouble;
var
  Etalon: TTestNullableDoubleClass;
  Actual: TTestNullableDoubleClass;
  Obj: TObject;
begin
  Obj := TMarshalUnMarshal.FromJson(TTestNullableDoubleClass, TTestNullableDoubleClass.AsJson);
  CheckIs(Obj, TTestNullableDoubleClass);

  Actual := Obj as TTestNullableDoubleClass;

  Etalon := TTestNullableDoubleClass.Create;
  try
    Etalon.TestNull := NullableDouble.Null;
    Etalon.TestNullButNotNeedSave := NullableDouble.Null;
    Etalon.Test := 123.456;

    CheckTrue(Etalon.Equals(Actual));
  finally
    Etalon.Free;
  end;
end;

procedure TTestUnmarshalNullable.TestNullableInt64;
var
  Etalon: TTestNullableInt64Class;
  Actual: TTestNullableInt64Class;
  Obj: TObject;
begin
  Obj := TMarshalUnMarshal.FromJson(TTestNullableInt64Class, TTestNullableInt64Class.AsJson);
  CheckIs(Obj, TTestNullableInt64Class);

  Actual := Obj as TTestNullableInt64Class;

  Etalon := TTestNullableInt64Class.Create;
  try
    Etalon.TestNull := NullableInt64.Null;
    Etalon.TestNullButNotNeedSave := NullableInt64.Null;
    Etalon.Test := 123;

    CheckTrue(Etalon.Equals(Actual));
  finally
    Etalon.Free;
  end;
end;

procedure TTestUnmarshalNullable.TestNullableInteger;
var
  Etalon: TTestNullableIntegerClass;
  Actual: TTestNullableIntegerClass;
  Obj: TObject;
begin
  Obj := TMarshalUnMarshal.FromJson(TTestNullableIntegerClass, TTestNullableIntegerClass.AsJson);
  CheckIs(Obj, TTestNullableIntegerClass);

  Actual := Obj as TTestNullableIntegerClass;

  Etalon := TTestNullableIntegerClass.Create;
  try
    Etalon.TestNull := NullableInteger.Null;
    Etalon.TestNullButNotNeedSave := NullableInteger.Null;
    Etalon.Test := 123;

    CheckTrue(Etalon.Equals(Actual));
  finally
    Etalon.Free;
  end;
end;

procedure TTestUnmarshalNullable.TestNullableObject;
var
  op: TTestNullableObjectClass;
begin
  op := TTestNullableObjectClass.Create;
  try
    op.TestNull := NullableObject.Null;
    op.TestNullButNotNeedSave := NullableObject.Null;
    op.Test := op.MakeTestObject;

    CheckEquals(op.AsJson, op.ToJsonString);
  finally
    op.Free;
  end;
end;

procedure TTestUnmarshalNullable.TestNullableString;
var
  Etalon: TTestUnmarshalNullableStringClass;
  Actual: TTestUnmarshalNullableStringClass;
  Obj: TObject;
begin
  Obj := TMarshalUnMarshal.FromJson(TTestUnmarshalNullableStringClass, TTestUnmarshalNullableStringClass.AsJson);
  CheckIs(Obj, TTestUnmarshalNullableStringClass);

  Actual := Obj as TTestUnmarshalNullableStringClass;

  Etalon := TTestUnmarshalNullableStringClass.Create;
  try
    Etalon.TestNull := NullableString.Null;
    Etalon.TestNullButNotNeedSave := NullableString.Null;
    Etalon.Test := '123';

    CheckTrue(Etalon.Equals(Actual));
  finally
    Etalon.Free;
  end;
end;

{ TTestNullableIntegerClass }

class function TTestNullableIntegerClass.AsJson: String;
begin
  Result := '{"integer_null":null,"integer_not_null":123}';
end;

{ TTestNullableInt64Class }

class function TTestNullableInt64Class.AsJson: String;
begin
  Result := '{"int64_null":null,"int64_not_null":123}';
end;

{ TTestNullableDoubleClass }

class function TTestNullableDoubleClass.AsJson: String;
begin
  Result := '{"double_null":null,"double_not_null":123.456}';
end;

{ TTestNullableObjectClass }

destructor TTestNullableObjectClass.Destroy;
begin
  FTest.Free;
  inherited;
end;

class function TTestNullableObjectClass.AsJson: String;
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
  RegisterTest('JSON\Unmarshal\', TTestUnmarshalNullable.Suite);
end.
