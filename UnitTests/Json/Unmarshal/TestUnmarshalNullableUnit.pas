unit TestUnmarshalNullableUnit;

interface

uses
  TestFramework, REST.Json.Types, System.JSON, Types, SysUtils, Classes,
  JSONNullableAttributeUnit, System.Generics.Collections, REST.JsonReflect,
  GenericParametersUnit,
  NullableBasicTypesUnit;

type
  TTestNullableBooleanClass = class(TGenericParameters)
  private
    [JSONName('boolean_null')]
    [Nullable(True)]
    FTestNull: NullableBoolean;

    [JSONName('boolean_null_but_not_need_save')]
    [Nullable]
    FTestNullButNotNeedSave: NullableBoolean;

    [JSONName('boolean_not_null_true')]
    [Nullable]
    FTestTrue: NullableBoolean;

    [JSONName('boolean_not_null_false')]
    [Nullable]
    FTestFalse: NullableBoolean;
  public
    constructor Create; override;

    function Equals(Obj: TObject): Boolean; override;

    class function AsJson: TJSONValue;

    property TestNull: NullableBoolean read FTestNull write FTestNull;
    property TestNullButNotNeedSave: NullableBoolean read FTestNullButNotNeedSave write FTestNullButNotNeedSave;
    property TestTrue: NullableBoolean read FTestTrue write FTestTrue;
    property TestFalse: NullableBoolean read FTestFalse write FTestFalse;
  end;

  TTestUnmarshalNullableStringClass = class(TGenericParameters)
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
    constructor Create; override;

    function Equals(Obj: TObject): Boolean; override;

    class function AsJson: TJSONValue;

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
    constructor Create; override;

    function Equals(Obj: TObject): Boolean; override;

    class function AsJson: TJSONValue;

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
    constructor Create; override;

    function Equals(Obj: TObject): Boolean; override;

    class function AsJson: TJSONValue;

    property TestNull: NullableDouble read FTestNull write FTestNull;
    property TestNullButNotNeedSave: NullableDouble read FTestNullButNotNeedSave write FTestNullButNotNeedSave;
    property Test: NullableDouble read FTest write FTest;
  end;

  TTestObject = class
  private
    FIntValue: integer;
    FBoolValue: boolean;
    FStringValue: String;
    FDoubleValue: double;
    FArrayValue: TIntegerDynArray;

    [JSONName('nullableobject_null_optional')]
    [NullableObject(TTestObject)]
    FOptionalNullObject: NullableObject;

    [JSONName('nullableobject_null')]
    [NullableObject(TTestObject,True)]
    FNullObject: NullableObject;

    [JSONName('nullableobject_not_null')]
    [NullableObject(TTestObject,True)]
    FNotNullObject: NullableObject;
  public
    constructor Create();

    function Equals(Obj: TObject): Boolean; override;

    property IntValue: integer read FIntValue;
    property BoolValue: boolean read FBoolValue;
    property StringValue: String read FStringValue;
    property DoubleValue: double read FDoubleValue;
    property ArrayValue: TIntegerDynArray read FArrayValue;
    property OptionalNullObject: NullableObject read FOptionalNullObject;
    property NullObject: NullableObject read FNullObject;
    property NotNullObject: NullableObject read FNotNullObject;
  end;

  TTestNullableObjectClass = class(TGenericParameters)
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
    constructor Create; override;
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; override;
    class function MakeTestObject(): TObject;

    class function AsJson: TJSONValue;

    property TestNull: NullableObject read FTestNull write FTestNull;
    property TestNullButNotNeedSave: NullableObject read FTestNullButNotNeedSave write FTestNullButNotNeedSave;
    property Test: NullableObject read FTest write FTest;
  end;

//  TTestArrayObjectArray = TArray<TTestObject>;
  TTestArrayObjectArray = array of TTestObject;

  TTestNullableArrayObjectClass = class(TGenericParameters)
  private
    [JSONName('array_empty')]
    [NullableArray(TTestObject,True)]
    FTestNull: TTestArrayObjectArray;

    [JSONName('array_empty_but_not_need_save')]
    [NullableArray(TTestObject)]
    FTestNullButNotNeedSave: TTestArrayObjectArray;

    [JSONName('array_not_empty')]
    [NullableArray(TTestObject)]
    FTest: TTestArrayObjectArray;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; override;
    class function MakeTestArray: TTestArrayObjectArray;

    class function AsJson: TJSONValue;

    property TestNull: TTestArrayObjectArray read FTestNull;
    property TestNullButNotNeedSave: TTestArrayObjectArray read FTestNullButNotNeedSave;
    property Test: TTestArrayObjectArray read FTest;
  end;

  TTestIntegerObject = class
  private
    [JSONName('value')]
    FValue: integer;
  public
    constructor Create(Value: integer);
  end;
  TTestIntegerObjectArray = array of TTestIntegerObject;
  TTestIntegerObjectList = TList<TTestIntegerObject>;

  TTestArrayObjectClass = class(TGenericParameters)
  private
    [JSONName('array_empty')]
    FEmptyArray: TTestIntegerObjectArray;

    [JSONName('array_not_empty')]
    FNotEmptyArray: TTestIntegerObjectArray;

    [JSONName('nullable_array_empty')]
    [NullableArray(TTestIntegerObject,True)]
    FNullableEmptyArray: TTestIntegerObjectArray;

    [JSONName('nullable_array_empty_but_not_need_save')]
    [NullableArray(TTestIntegerObject)]
    FNullableEmptyArrayNotForSaving: TTestIntegerObjectArray;

    [JSONName('nullable_array_not_empty')]
    [NullableArray(TTestIntegerObject)]
    FNullableNotEmptyArray: TTestIntegerObjectArray;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; override;

    class function AsJson: TJSONValue;
  end;

  TTestSimpleObjectClass = class(TGenericParameters)
  private
    FIntValue: integer;
    FBoolValue: boolean;
    FStringValue: String;
    FDoubleValue: double;
    FObjectValue: TTestSimpleObjectClass;
  public
    class function AsJson: TJSONValue;
    function Equals(Obj: TObject): Boolean; override;
  end;

  TTestUnmarshalNullable = class(TTestCase)
  published
    procedure TestNullableBoolean();
    procedure TestNullableString();
    procedure TestNullableInteger();
    procedure TestNullableDouble();
    procedure TestNullableObject();
    procedure TestSimpleObject();
    procedure TestNullableArrayObject();
    procedure TestArrayObject();
    procedure TestSimpleArray();
    procedure TestSimpleList();
  end;

implementation

{ TTestNullableBooleanClass }

uses MarshalUnMarshalUnit, ErrorResponseUnit;

constructor TTestNullableBooleanClass.Create;
begin
  Inherited;

  FTestNull := NullableBoolean.Null;
  FTestNullButNotNeedSave := NullableBoolean.Null;
  FTestTrue := NullableBoolean.Null;
  FTestFalse := NullableBoolean.Null;
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
    (TestTrue = Other.TestTrue) and
    (TestFalse = Other.TestFalse);
end;

class function TTestNullableBooleanClass.AsJson: TJSONValue;
begin
  Result := TJSONObject.ParseJSONValue(
    '{"boolean_null":null,"boolean_not_null_true":true,"boolean_not_null_false":false}');
end;

{ TTestNullableStringClass }

class function TTestUnmarshalNullableStringClass.AsJson: TJSONValue;
begin
  Result := TJSONObject.ParseJSONValue(
    '{"string_null":null,"string_not_null":"123"}');
end;

constructor TTestUnmarshalNullableStringClass.Create;
begin
  Inherited;

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

procedure TTestUnmarshalNullable.TestArrayObject;
var
  Etalon: TTestArrayObjectClass;
  Actual: TTestArrayObjectClass;
  Obj: TObject;
  JsonValue: TJSONValue;
begin
  JsonValue := TTestArrayObjectClass.AsJson;
  try
    Obj := TMarshalUnMarshal.FromJson(TTestArrayObjectClass, JsonValue);
    try
      CheckIs(Obj, TTestArrayObjectClass);

      Actual := Obj as TTestArrayObjectClass;

      Etalon := TTestArrayObjectClass.Create;
      try
        SetLength(Etalon.FNotEmptyArray, 3);
        Etalon.FNotEmptyArray[0] := TTestIntegerObject.Create(1);
        Etalon.FNotEmptyArray[1] := TTestIntegerObject.Create(2);
        Etalon.FNotEmptyArray[2] := TTestIntegerObject.Create(3);

        SetLength(Etalon.FNullableNotEmptyArray, 4);
        Etalon.FNullableNotEmptyArray[0] := TTestIntegerObject.Create(8);
        Etalon.FNullableNotEmptyArray[1] := TTestIntegerObject.Create(7);
        Etalon.FNullableNotEmptyArray[2] := TTestIntegerObject.Create(6);
        Etalon.FNullableNotEmptyArray[3] := TTestIntegerObject.Create(5);

        CheckTrue(Etalon.Equals(Actual));
      finally
        FreeAndNil(Etalon);
      end;
    finally
      FreeAndNil(Obj);
    end;
  finally
    FreeAndNil(JsonValue);
  end;
end;

procedure TTestUnmarshalNullable.TestNullableArrayObject;
var
  Etalon: TTestNullableArrayObjectClass;
  Actual: TTestNullableArrayObjectClass;
  Obj: TObject;
  JsonValue: TJSONValue;
begin
  JsonValue := TTestNullableArrayObjectClass.AsJson;
  try
    Obj := TMarshalUnMarshal.FromJson(TTestNullableArrayObjectClass, JsonValue);
    try
      CheckIs(Obj, TTestNullableArrayObjectClass);

      Actual := Obj as TTestNullableArrayObjectClass;

      Etalon := TTestNullableArrayObjectClass.Create;
      try
        Etalon.FTest := TTestNullableArrayObjectClass.MakeTestArray;

        CheckTrue(Etalon.Equals(Actual));
      finally
        FreeAndNil(Etalon);
      end;
    finally
      FreeAndNil(Obj);
    end;
  finally
    FreeAndNil(JsonValue);
  end;
end;

procedure TTestUnmarshalNullable.TestNullableBoolean;
var
  Etalon: TTestNullableBooleanClass;
  Actual: TTestNullableBooleanClass;
  Obj: TObject;
  JsonValue: TJSONValue;
begin
  JsonValue := TTestNullableBooleanClass.AsJson;
  try
    Obj := TMarshalUnMarshal.FromJson(TTestNullableBooleanClass, JsonValue);
    try
      CheckIs(Obj, TTestNullableBooleanClass);

      Actual := Obj as TTestNullableBooleanClass;

      Etalon := TTestNullableBooleanClass.Create;
      try
        Etalon.TestNull := NullableBoolean.Null;
        Etalon.TestNullButNotNeedSave := NullableBoolean.Null;
        Etalon.TestTrue := True;
        Etalon.TestFalse := False;

        CheckTrue(Etalon.Equals(Actual));
      finally
        FreeAndNil(Etalon);
      end;
    finally
      FreeAndNil(Obj);
    end;
  finally
    FreeAndNil(JsonValue);
  end;
end;

procedure TTestUnmarshalNullable.TestNullableDouble;
var
  Etalon: TTestNullableDoubleClass;
  Actual: TTestNullableDoubleClass;
  Obj: TObject;
  JsonValue: TJSONValue;
begin
  JsonValue := TTestNullableDoubleClass.AsJson;
  try
    Obj := TMarshalUnMarshal.FromJson(TTestNullableDoubleClass, JsonValue);
    try
      CheckIs(Obj, TTestNullableDoubleClass);

      Actual := Obj as TTestNullableDoubleClass;

      Etalon := TTestNullableDoubleClass.Create;
      try
        Etalon.TestNull := NullableDouble.Null;
        Etalon.TestNullButNotNeedSave := NullableDouble.Null;
        Etalon.Test := 123.456;

        CheckTrue(Etalon.Equals(Actual));
      finally
        FreeAndNil(Etalon);
      end;
    finally
      FreeAndNil(Obj);
    end;
  finally
    FreeAndNil(JsonValue);
  end;
end;

procedure TTestUnmarshalNullable.TestNullableInteger;
var
  Etalon: TTestNullableIntegerClass;
  Actual: TTestNullableIntegerClass;
  Obj: TObject;
  JsonValue: TJSONValue;
begin
  JsonValue := TTestNullableIntegerClass.AsJson;
  try
    Obj := TMarshalUnMarshal.FromJson(TTestNullableIntegerClass, JsonValue);
    try
      CheckIs(Obj, TTestNullableIntegerClass);

      Actual := Obj as TTestNullableIntegerClass;

      Etalon := TTestNullableIntegerClass.Create;
      try
        Etalon.TestNull := NullableInteger.Null;
        Etalon.TestNullButNotNeedSave := NullableInteger.Null;
        Etalon.Test := 123;

        CheckTrue(Etalon.Equals(Actual));
      finally
        FreeAndNil(Etalon);
      end;
    finally
      FreeAndNil(Obj);
    end;
  finally
    FreeAndNil(JsonValue);
  end;
end;

procedure TTestUnmarshalNullable.TestNullableObject;
var
  Etalon: TTestNullableObjectClass;
  Actual: TTestNullableObjectClass;
  Obj: TObject;
  JsonValue: TJSONValue;
begin
  JsonValue := TTestNullableObjectClass.AsJson;
  try
    Obj := TMarshalUnMarshal.FromJson(TTestNullableObjectClass, JsonValue);
    try
      CheckIs(Obj, TTestNullableObjectClass);

      Actual := Obj as TTestNullableObjectClass;

      Etalon := TTestNullableObjectClass.Create;
      try
        Etalon.TestNull := NullableObject.Null;
        Etalon.TestNullButNotNeedSave := NullableObject.Null;

        Etalon.Test := TTestNullableObjectClass.MakeTestObject;

        CheckTrue(Etalon.Equals(Actual));
      finally
        FreeAndNil(Etalon);
      end;
    finally
      FreeAndNil(Obj);
    end;
  finally
    FreeAndNil(JsonValue);
  end;
end;

procedure TTestUnmarshalNullable.TestNullableString;
var
  Etalon: TTestUnmarshalNullableStringClass;
  Actual: TTestUnmarshalNullableStringClass;
  Obj: TObject;
  JsonValue: TJSONValue;
begin
  JsonValue := TTestUnmarshalNullableStringClass.AsJson;
  try
    Obj := TMarshalUnMarshal.FromJson(TTestUnmarshalNullableStringClass, JsonValue);
    try
      CheckIs(Obj, TTestUnmarshalNullableStringClass);

      Actual := Obj as TTestUnmarshalNullableStringClass;

      Etalon := TTestUnmarshalNullableStringClass.Create;
      try
        Etalon.TestNull := NullableString.Null;
        Etalon.TestNullButNotNeedSave := NullableString.Null;
        Etalon.Test := '123';

        CheckTrue(Etalon.Equals(Actual));
      finally
        FreeAndNil(Etalon);
      end;
    finally
      FreeAndNil(Obj);
    end;
  finally
    FreeAndNil(JsonValue);
  end;
end;

procedure TTestUnmarshalNullable.TestSimpleArray;
var
  Actual: TTestIntegerObjectList;
  Obj: TObject;
  JsonValue: TJSONValue;
  i: integer;
begin
  JsonValue := TJSONObject.ParseJSONValue('[{"value":1},{"value":2},{"value":3}]');
  try
    Obj := TMarshalUnMarshal.FromJson(TTestIntegerObjectList, JsonValue);
    try
      CheckIs(Obj, TTestIntegerObjectList);

      Actual := Obj as TTestIntegerObjectList;

      CheckEquals(3, Actual.Count);
      CheckEquals(1, Actual[0].FValue);
      CheckEquals(2, Actual[1].FValue);
      CheckEquals(3, Actual[2].FValue);
    finally
      for i := Actual.Count - 1 downto 0 do
        Actual[i].Free;

      FreeAndNil(Obj);
    end;
  finally
    FreeAndNil(JsonValue);
  end;
end;

procedure TTestUnmarshalNullable.TestSimpleList;
var
  Actual: TErrorResponse;
  Obj: TObject;
  JsonValue: TJSONValue;
begin
  JsonValue := TJSONObject.ParseJSONValue('{"errors":["Error1", "Error2"]}');
  try
    Obj := TMarshalUnMarshal.FromJson(TErrorResponse, JsonValue);
    try
      CheckIs(Obj, TErrorResponse);

      Actual := Obj as TErrorResponse;

      CheckEquals(2, Length(Actual.Errors));
      CheckEquals('Error1', Actual.Errors[0]);
      CheckEquals('Error2', Actual.Errors[1]);
    finally
      FreeAndNil(Obj);
    end;
  finally
    FreeAndNil(JsonValue);
  end;
end;

procedure TTestUnmarshalNullable.TestSimpleObject;
var
  Etalon: TTestSimpleObjectClass;
  Actual: TTestSimpleObjectClass;
  Obj: TObject;
  JsonValue: TJSONValue;
begin
  JsonValue := TTestSimpleObjectClass.AsJson;
  try
    Obj := TMarshalUnMarshal.FromJson(TTestSimpleObjectClass, JsonValue);
    try
      CheckIs(Obj, TTestSimpleObjectClass);

      Actual := Obj as TTestSimpleObjectClass;

      Etalon := TTestSimpleObjectClass.Create;
      try
        Etalon.FIntValue := 123;
        Etalon.FBoolValue := True;
        Etalon.FStringValue := '321';
        Etalon.FDoubleValue := 123.456;
        Etalon.FObjectValue := TTestSimpleObjectClass.Create;
        Etalon.FObjectValue.FIntValue := 111111;
        Etalon.FObjectValue.FBoolValue := False;
        Etalon.FObjectValue.FStringValue := '22222';
        Etalon.FObjectValue.FDoubleValue := 789.123;
        Etalon.FObjectValue.FObjectValue := nil;

        CheckTrue(Etalon.Equals(Actual));
      finally
        FreeAndNil(Etalon);
      end;
    finally
      FreeAndNil(Obj);
    end;
  finally
    FreeAndNil(JsonValue);
  end;
end;

{ TTestNullableIntegerClass }

class function TTestNullableIntegerClass.AsJson: TJSONValue;
begin
  Result := TJSONObject.ParseJSONValue(
    '{"integer_null":null,"integer_not_null":123}');
end;

constructor TTestNullableIntegerClass.Create;
begin
  Inherited;
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

class function TTestNullableDoubleClass.AsJson: TJSONValue;
begin
  Result := TJSONObject.ParseJSONValue(
    '{"double_null":null,"double_not_null":123.456}');
end;

constructor TTestNullableDoubleClass.Create;
begin
  Inherited;
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
  Inherited;
  FTestNull := NullableObject.Null;
  FTestNullButNotNeedSave := NullableObject.Null;
  FTest := NullableObject.Null;
end;

destructor TTestNullableObjectClass.Destroy;
begin
  FreeAndNil(FTest);

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

class function TTestNullableObjectClass.AsJson: TJSONValue;
begin
//  Result := TJSONObject.ParseJSONValue(
//    '{"object_null":null,"object_not_null":{"intValue":123,"boolValue":true,"stringValue":"321","doubleValue":123.456,"arrayValue":[3,4,5]}}');
  Result := TJSONObject.ParseJSONValue(
    '{"object_null":null,' +
    '"object_not_null":{"intValue":123,"boolValue":true,"stringValue":"321",' +
    '"doubleValue":123.456,"arrayValue":[3,4,5],"nullableobject_null":null,' +
    '"nullableobject_not_null":{"intValue":111111,"boolValue":false,' +
    '"stringValue":"22222","doubleValue":789.123,"arrayValue":[8],' +
    '"nullableobject_null":null,"nullableobject_not_null":null}}}');
end;

class function TTestNullableObjectClass.MakeTestObject: TObject;
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

{ TTestObject }

constructor TTestObject.Create;
begin
  FOptionalNullObject := NullableObject.Null;
  FNullObject := NullableObject.Null;
  FNotNullObject := NullableObject.Null;
end;

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
    (FIntValue = Other.FIntValue) and
    (FBoolValue = Other.FBoolValue) and
    (FStringValue = Other.FStringValue) and
    (FDoubleValue = Other.FDoubleValue) and
    (FNullObject = Other.FNullObject) and
    (FNotNullObject = Other.FNotNullObject) and
    (FOptionalNullObject = Other.FOptionalNullObject);

  if Result then
  begin
    if (Length(ArrayValue) <> Length(Other.ArrayValue)) then
      Exit(False);
    for i := 0 to High(ArrayValue) do
      Result := Result and (ArrayValue[i] = Other.ArrayValue[i]);
  end;
end;

{ TTestSimpleObjectClass }

class function TTestSimpleObjectClass.AsJson: TJSONValue;
begin
  Result := TJSONObject.ParseJSONValue(
    '{"intValue":123,"boolValue":true,"stringValue":"321",' +
    '"doubleValue":123.456,"objectValue":{"intValue":111111,"boolValue":false,' +
    '"stringValue":"22222","doubleValue":789.123,"objectValue":null}}');
end;

function TTestSimpleObjectClass.Equals(Obj: TObject): Boolean;
var
  Other: TTestSimpleObjectClass;
begin
  Result := False;

  if not (Obj is TTestSimpleObjectClass) then
    Exit;

  Other := TTestSimpleObjectClass(Obj);

  Result :=
    (FIntValue = Other.FIntValue) and
    (FBoolValue = Other.FBoolValue) and
    (FStringValue = Other.FStringValue) and
    (FDoubleValue = Other.FDoubleValue) and
    (FObjectValue.FIntValue = Other.FObjectValue.FIntValue) and
    (FObjectValue.FBoolValue = Other.FObjectValue.FBoolValue) and
    (FObjectValue.FStringValue = Other.FObjectValue.FStringValue) and
    (FObjectValue.FDoubleValue = Other.FObjectValue.FDoubleValue);
end;

{ TTestNullableArrayObjectClass }

class function TTestNullableArrayObjectClass.AsJson: TJSONValue;
begin
  Result := TJSONObject.ParseJSONValue(
    '{"array_empty":[],' +
    '"array_not_empty":[' +
    '{"intValue":123,"boolValue":true,"stringValue":"321",' +
    '"doubleValue":123.456,"arrayValue":[3,4,5],"nullableobject_null":null,' +
    '"nullableobject_not_null":{"intValue":111111,"boolValue":false,' +
    '"stringValue":"22222","doubleValue":789.123,"arrayValue":[8],' +
    '"nullableobject_null":null,"nullableobject_not_null":null}}' +
    ',' +
    '{"intValue":756,"boolValue":true,"stringValue":"qwe",' +
    '"doubleValue":573.5,"arrayValue":[2,1],"nullableobject_null":null,' +
    '"nullableobject_not_null":{"intValue":85,"boolValue":true,' +
    '"stringValue":"asd","doubleValue":147.16,"arrayValue":[1,5,7],' +
    '"nullableobject_null":null,"nullableobject_not_null":null}}' +
    ']}');
end;

constructor TTestNullableArrayObjectClass.Create;
begin
  Inherited;
  SetLength(FTestNull, 0);
  SetLength(FTestNullButNotNeedSave, 0);
  SetLength(FTest, 0);
end;

destructor TTestNullableArrayObjectClass.Destroy;
var
  i: integer;
begin
  for i := Length(FTestNull) - 1 downto 0 do
    FreeAndNil(FTestNull[i]);
  for i := Length(FTestNullButNotNeedSave) - 1 downto 0 do
    FreeAndNil(FTestNullButNotNeedSave[i]);
  for i := Length(FTest) - 1 downto 0 do
    FreeAndNil(FTest[i]);

  inherited;
end;

function TTestNullableArrayObjectClass.Equals(Obj: TObject): Boolean;
var
  Other: TTestNullableArrayObjectClass;
  i: integer;
begin
  Result := False;

  if not (Obj is TTestNullableArrayObjectClass) then
    Exit;

  Other := TTestNullableArrayObjectClass(Obj);

  Result :=
    (Length(FTestNull) = Length(Other.FTestNull)) and
    (Length(FTestNullButNotNeedSave) = Length(Other.FTestNullButNotNeedSave)) and
    (Length(FTest) = Length(Other.FTest));

  if not Result then
    Exit;

  Result := False;

  for i := 0 to Length(FTestNull) - 1 do
    if not FTestNull[i].Equals(Other.FTestNull[i]) then
      Exit;

  for i := 0 to Length(FTestNullButNotNeedSave) - 1 do
    if not FTestNullButNotNeedSave[i].Equals(Other.FTestNullButNotNeedSave[i]) then
      Exit;

  for i := 0 to Length(FTest) - 1 do
    if not FTest[i].Equals(Other.FTest[i]) then
      Exit;

  Result := True;
end;

class function TTestNullableArrayObjectClass.MakeTestArray(): TTestArrayObjectArray;
var
  Res: TTestObject;
  SubObject: TTestObject;
begin
  SetLength(Result, 2);

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

  Result[0] := Res;

  Res := TTestObject.Create;
  Res.FIntValue := 756;
  Res.FBoolValue := True;
  Res.FStringValue := 'qwe';
  Res.FDoubleValue := 573.5;
  SetLength(Res.FArrayValue, 2);
  Res.FArrayValue[0] := 2;
  Res.FArrayValue[1] := 1;

  Res.FOptionalNullObject := NullableObject.Null;
  Res.FNullObject := NullableObject.Null;
  SubObject := TTestObject.Create;
  SubObject.FIntValue := 85;
  SubObject.FBoolValue := True;
  SubObject.FStringValue := 'asd';
  SubObject.FDoubleValue := 147.16;
  SetLength(SubObject.FArrayValue, 3);
  SubObject.FArrayValue[0] := 1;
  SubObject.FArrayValue[1] := 5;
  SubObject.FArrayValue[2] := 7;
  SubObject.FOptionalNullObject := NullableObject.Null;
  SubObject.FNullObject := NullableObject.Null;
  SubObject.FNotNullObject := NullableObject.Null;
  Res.FNotNullObject := SubObject;

  Result[1] := Res;

end;

{ TTestArrayObjectClass }

class function TTestArrayObjectClass.AsJson: TJSONValue;
begin
  Result := TJSONObject.ParseJSONValue(
    '{"array_empty":[],' +
    '"array_not_empty":[{"value":1},{"value":2},{"value":3}],' +
    '"nullable_array_empty":[],' +
    '"nullable_array_not_empty":[{"value":8},{"value":7},{"value":6},{"value":5}]}');
end;

constructor TTestArrayObjectClass.Create;
begin
  Inherited;

  SetLength(FEmptyArray, 0);
  SetLength(FNotEmptyArray, 0);
  SetLength(FNullableEmptyArray, 0);
  SetLength(FNullableEmptyArrayNotForSaving, 0);
  SetLength(FNullableNotEmptyArray, 0);
end;

destructor TTestArrayObjectClass.Destroy;
  procedure Destroy(Arr: TTestIntegerObjectArray);
  var
    i: integer;
  begin
    for i := Length(Arr) - 1 downto 0 do
      FreeAndNil(Arr[i]);
  end;
begin
    Destroy(FEmptyArray);
    Destroy(FNotEmptyArray);
    Destroy(FNullableEmptyArray);
    Destroy(FNullableEmptyArrayNotForSaving);
    Destroy(FNullableNotEmptyArray);

  inherited;
end;

function TTestArrayObjectClass.Equals(Obj: TObject): Boolean;
var
  Other: TTestArrayObjectClass;
  i: integer;
begin
  Result := False;

  if not (Obj is TTestArrayObjectClass) then
    Exit;

  Other := TTestArrayObjectClass(Obj);

  Result :=
    (Length(FEmptyArray) = Length(Other.FEmptyArray)) and
    (Length(FNotEmptyArray) = Length(Other.FNotEmptyArray)) and
    (Length(FNullableEmptyArray) = Length(Other.FNullableEmptyArray)) and
    (Length(FNullableEmptyArrayNotForSaving) = Length(Other.FNullableEmptyArrayNotForSaving)) and
    (Length(FNullableNotEmptyArray) = Length(Other.FNullableNotEmptyArray));

  if not Result then
    Exit;

  Result := False;

  for i := 0 to Length(FEmptyArray) - 1 do
    if (FEmptyArray[i].FValue <> Other.FEmptyArray[i].FValue) then
      Exit;

  for i := 0 to Length(FNotEmptyArray) - 1 do
    if (FNotEmptyArray[i].FValue <> Other.FNotEmptyArray[i].FValue) then
      Exit;

  for i := 0 to Length(FNullableEmptyArray) - 1 do
    if (FNullableEmptyArray[i].FValue <> Other.FNullableEmptyArray[i].FValue) then
      Exit;

  for i := 0 to Length(FNullableEmptyArrayNotForSaving) - 1 do
    if (FNullableEmptyArrayNotForSaving[i].FValue <> Other.FNullableEmptyArrayNotForSaving[i].FValue) then
      Exit;

        for i := 0 to Length(FNullableNotEmptyArray) - 1 do
    if (FNullableNotEmptyArray[i].FValue <> Other.FNullableNotEmptyArray[i].FValue) then
      Exit;

  Result := True;
end;

{ TTestIntegerObject }

constructor TTestIntegerObject.Create(Value: integer);
begin
  FValue := Value;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('JSON\Unmarshal\', TTestUnmarshalNullable.Suite);
end.
