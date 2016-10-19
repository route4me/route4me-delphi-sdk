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
    [JSONNullableBoolean(True)]
    FTestNull: NullableBoolean;

    [JSONName('boolean_null_but_not_need_save')]
    [JSONNullableBoolean]
    FTestNullButNotNeedSave: NullableBoolean;

    [JSONName('boolean_not_null')]
    [JSONNullableBoolean]
    FTest: NullableBoolean;
  public
    constructor Create;

    function Equals(Obj: TObject): Boolean; override;

    class function AsJson: String;

    property TestNull: NullableBoolean read FTestNull write FTestNull;
    property TestNullButNotNeedSave: NullableBoolean read FTestNullButNotNeedSave write FTestNullButNotNeedSave;
    property Test: NullableBoolean read FTest write FTest;
  end;

  TTestUnmarshalNullableStringClass = class(TGenericParameters)
  private
    [JSONName('string_null')]
    [JSONNullableString(True)]
    FTestNull: NullableString;

    [JSONName('string_null_but_not_need_save')]
    [JSONNullableString]
    FTestNullButNotNeedSave: NullableString;

    [JSONName('string_not_null')]
    [JSONNullableString]
    FTest: NullableString;
  public
    constructor Create;

    function Equals(Obj: TObject): Boolean; override;

    class function AsJson: String;

    property TestNull: NullableString read FTestNull write FTestNull;
    property TestNullButNotNeedSave: NullableString read FTestNullButNotNeedSave write FTestNullButNotNeedSave;
    property Test: NullableString read FTest write FTest;
  end;

  TTestNullableIntegerClass = class(TGenericParameters)
  private
    [JSONName('integer_null')]
    [JSONNullableNumber(True)]
    FTestNull: NullableInteger;

    [JSONName('integer_null_but_not_need_save')]
    [JSONNullableNumber]
    FTestNullButNotNeedSave: NullableInteger;

    [JSONName('integer_not_null')]
    [JSONNullableNumber]
    FTest: NullableInteger;
  public
    constructor Create;

    function Equals(Obj: TObject): Boolean; override;

    class function AsJson: String;

    property TestNull: NullableInteger read FTestNull write FTestNull;
    property TestNullButNotNeedSave: NullableInteger read FTestNullButNotNeedSave write FTestNullButNotNeedSave;
    property Test: NullableInteger read FTest write FTest;
  end;

  TTestNullableDoubleClass = class(TGenericParameters)
  private
    [JSONName('double_null')]
    [JSONNullableNumber(True)]
    FTestNull: NullableDouble;

    [JSONName('double_null_but_not_need_save')]
    [JSONNullableNumber]
    FTestNullButNotNeedSave: NullableDouble;

    [JSONName('double_not_null')]
    [JSONNullableNumber]
    FTest: NullableDouble;
  public
    constructor Create;

    function Equals(Obj: TObject): Boolean; override;

    class function AsJson: String;

    property TestNull: NullableDouble read FTestNull write FTestNull;
    property TestNullButNotNeedSave: NullableDouble read FTestNullButNotNeedSave write FTestNullButNotNeedSave;
    property Test: NullableDouble read FTest write FTest;
  end;

  TTestObject = class
    IntValue: integer;
    BoolValue: boolean;
    StringValue: String;
    DoubleValue: double;
    ArrayValue: array of integer;

    function Equals(Obj: TObject): Boolean; override;
  end;

  TTestNullableObjectClass = class(TGenericParameters)
    [JSONName('object_null')]
    [JSONNullableObject(TTestObject,True)]
    FTestNull: NullableObject;

    [JSONName('object_null_but_not_need_save')]
    [JSONNullableObject(TTestObject)]
    FTestNullButNotNeedSave: NullableObject;

    [JSONName('object_not_null')]
    [JSONNullableObject(TTestObject)]
    FTest: NullableObject;
  public
    constructor Create;
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; override;
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
    procedure TestNullableDouble();
    procedure TestNullableObject();
  end;

implementation

{ TTestNullableBooleanClass }

uses MarshalUnMarshalUnit;

constructor TTestNullableBooleanClass.Create;
begin
    FTestNull := NullableBoolean.Null;
    FTestNullButNotNeedSave := NullableBoolean.Null;
    FTest := NullableBoolean.Null;
end;

function TTestNullableBooleanClass.Equals(Obj: TObject): Boolean;
var
  Other: TTestNullableBooleanClass;
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
end;

function TTestUnmarshalNullableStringClass.Equals(Obj: TObject): Boolean;
var
  Other: TTestUnmarshalNullableStringClass;
begin
  Result := False;

  if not (Obj is TTestUnmarshalNullableStringClass) then
    Exit;

  Other := TTestUnmarshalNullableStringClass(Obj);

  Result :=
    (TestNull = Other.TestNull) and
    (TestNullButNotNeedSave = Other.TestNullButNotNeedSave) and
    (Test = Other.Test);
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
  Etalon: TTestNullableObjectClass;
  Actual: TTestNullableObjectClass;
  TestObj: TTestObject;
  Obj: TObject;
begin
  Obj := TMarshalUnMarshal.FromJson(TTestNullableObjectClass, TTestNullableObjectClass.AsJson);
  CheckIs(Obj, TTestNullableObjectClass);

  Actual := Obj as TTestNullableObjectClass;

  Etalon := TTestNullableObjectClass.Create;
  try
    Etalon.TestNull := NullableObject.Null;
    Etalon.TestNullButNotNeedSave := NullableObject.Null;

    TestObj := TTestObject.Create;
    TestObj.IntValue := 123;
    TestObj.BoolValue := True;
    TestObj.StringValue := '321';
    TestObj.DoubleValue := 123.456;
    SetLength(TestObj.ArrayValue, 3);
    TestObj.ArrayValue[0] := 3;
    TestObj.ArrayValue[1] := 4;
    TestObj.ArrayValue[2] := 5;
    Etalon.Test := TestObj;

    CheckTrue(Etalon.Equals(Actual));
  finally
    Etalon.Free;
  end;
end;

{var
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
end;  }

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

constructor TTestNullableIntegerClass.Create;
begin
    FTestNull := NullableInteger.Null;
    FTestNullButNotNeedSave := NullableInteger.Null;
    FTest := NullableInteger.Null;
end;

function TTestNullableIntegerClass.Equals(Obj: TObject): Boolean;
var
  Other: TTestNullableIntegerClass;
begin
  Result := False;

  if not (Obj is TTestNullableIntegerClass) then
    Exit;

  Other := TTestNullableIntegerClass(Obj);

  Result :=
    (TestNull = Other.TestNull) and
    (TestNullButNotNeedSave = Other.TestNullButNotNeedSave) and
    (Test = Other.Test);
end;

{ TTestNullableDoubleClass }

class function TTestNullableDoubleClass.AsJson: String;
begin
  Result := '{"double_null":null,"double_not_null":123.456}';
end;

constructor TTestNullableDoubleClass.Create;
begin
    FTestNull := NullableDouble.Null;
    FTestNullButNotNeedSave := NullableDouble.Null;
    FTest := NullableDouble.Null;
end;

function TTestNullableDoubleClass.Equals(Obj: TObject): Boolean;
var
  Other: TTestNullableDoubleClass;
begin
  Result := False;

  if not (Obj is TTestNullableDoubleClass) then
    Exit;

  Other := TTestNullableDoubleClass(Obj);

  Result :=
    (TestNull = Other.TestNull) and
    (TestNullButNotNeedSave = Other.TestNullButNotNeedSave) and
    (Test = Other.Test);
end;

{ TTestNullableObjectClass }

constructor TTestNullableObjectClass.Create;
begin
    FTestNull := NullableObject.Null;
    FTestNullButNotNeedSave := NullableObject.Null;
    FTest := NullableObject.Null;
end;

destructor TTestNullableObjectClass.Destroy;
begin
  FTest.Free;
  inherited;
end;

function TTestNullableObjectClass.Equals(Obj: TObject): Boolean;
var
  Other: TTestNullableObjectClass;
begin
  Result := False;

  if not (Obj is TTestNullableObjectClass) then
    Exit;

  Other := TTestNullableObjectClass(Obj);

  Result :=
    (TestNull = Other.TestNull) and
    (TestNullButNotNeedSave = Other.TestNullButNotNeedSave) and
    (Test = Other.Test);
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

{ TTestObject }

function TTestObject.Equals(Obj: TObject): Boolean;
var
  Other: TTestObject;
  i: integer;
begin
  Result := False;

  if not (Obj is TTestObject) then
    Exit;

  Other := TTestObject(Obj);

  Result :=
    (IntValue = Other.IntValue) and
    (BoolValue = Other.BoolValue) and
    (StringValue = Other.StringValue) and
    (DoubleValue = Other.DoubleValue);

  if Result then
  begin
    if (Length(ArrayValue) <> Length(Other.ArrayValue)) then
      Exit(False);
    for i := 0 to High(ArrayValue) do
      Result := Result and (ArrayValue[i] = Other.ArrayValue[i]);
  end;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('JSON\Unmarshal\', TTestUnmarshalNullable.Suite);
end.
