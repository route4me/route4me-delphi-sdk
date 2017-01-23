unit NullableArrayInterceptorUnit;

interface

uses
  {$IF CompilerVersion < 27.0}
  Data.DBXJSONReflect,
  Data.DBXJSON,
  {$ELSE}
  REST.JsonReflect,
  System.JSON,
  {$IFEND}

  Windows, Rtti, SysUtils, System.TypInfo, System.Generics.Collections;

type
  TNullableArrayIntermediateObjectA = class
  private
    FIsRequired: boolean;
    FValue: TArray<TObject>;
  public
    property IsRequired: boolean read FIsRequired write FIsRequired;
  end;

  TNullableArrayIntermediateObject = class(TInterfacedObject, IUnknown)
  private
    FNullableArrayIntermediateObject: TNullableArrayIntermediateObjectA;
  public
    constructor Create(Value: TValue);
    destructor Destroy; override;
  end;

  TNullableArrayInterceptor = class(TJSONInterceptor)
  private
  type
    TInternalJSONUnMarshal = class(TJSONUnMarshal);
  const
    IsNullFieldCaption = 'FIsNull';
    ValueFieldCaption = 'FValue';
  var
    FIntermediateObjects: TObjectList<TObject>;

    function GetObjectValue(s: String; Clazz: TClass): TValue;
  public
    destructor Destroy; override;

    /// <summary>Converters that transforms a field value into an
    /// intermediate object</summary>
    /// <param name="Data">Current object instance being serialized</param>
    /// <param name="Field">Field name</param>
    /// <result> a serializable object </result>
    function ObjectConverter(Data: TObject; Field: string): TObject; override;
    /// <summary>Reverter that sets an object field to a value based on
    /// an array of strings</summary>
    /// <param name="Data">Current object instance being populated</param>
    /// <param name="Field">Field name</param>
    /// <param name="Args"> an array of strings </param>
    procedure StringsReverter(Data: TObject; Field: string; Args: TListOfStrings); override;
  end;

implementation

{ TNullableArrayInterceptor }

uses JSONNullableAttributeUnit, MarshalUnMarshalUnit;

destructor TNullableArrayInterceptor.Destroy;
begin
  FreeAndNil(FIntermediateObjects);

  inherited;
end;

function TNullableArrayInterceptor.GetObjectValue(s: String; Clazz: TClass): TValue;
var
  Obj: TObject;
  JsonValue: TJsonValue;
begin
  JsonValue := TJsonObject.ParseJSONValue(s);
  try
    Obj := TMarshalUnMarshal.FromJson(Clazz, JsonValue);
    Result := TValue.From(Obj);
  finally
    FreeAndNil(JsonValue)
  end;
end;

function TNullableArrayInterceptor.ObjectConverter(Data: TObject;
  Field: string): TObject;
var
  ctx: TRttiContext;
  RttiType: TRttiType;
  RttiField: TRttiField;
  Value: TValue;
begin
  Result := nil;

  ctx := TRttiContext.Create;

  try
    rttiType := ctx.GetType(Data.ClassType);
    RttiField := rttiType.GetField(Field);

    if (RttiField.FieldType.TypeKind <> tkDynArray) then
        raise Exception.Create('The field marked attribute "NullableArray" must be an array.');

    Value := RttiField.GetValue(Data);
    Result := TNullableArrayIntermediateObject.Create(Value);
  finally
    ctx.Free;
  end;

  if (Result = nil) then
    raise Exception.Create('The result can not be undefinded!');

  if (FIntermediateObjects = nil) then
    FIntermediateObjects := TObjectList<TObject>.Create;
  FIntermediateObjects.Add(Result);
end;

procedure TNullableArrayInterceptor.StringsReverter(Data: TObject;
  Field: string; Args: TListOfStrings);
  function GetClass(RttiField: TRttiField): TClass;
  var
    NullableAttribute: NullableArrayAttribute;
    Attr: TCustomAttribute;
  begin
    NullableAttribute := nil;
    for Attr in RttiField.GetAttributes do
      if Attr is NullableArrayAttribute then
        NullableAttribute := NullableArrayAttribute(Attr);
    if (NullableAttribute = nil) then
      raise Exception.Create('This intercepter (TNullableArrayInterceptor) was created not in NullableArrayAttribute.');
    Result := NullableArrayAttribute(NullableAttribute).Clazz;
  end;
var
  ctx: TRttiContext;
  RttiType: TRttiType;
  RttiField: TRttiField;
  Clazz: TClass;
  ArrayValue, NewArrayValue: TValue;
  i: integer;
  Values: array of TValue;
begin
  inherited;

  if (Length(Args) = 0) then
    Exit;

  ctx := TRttiContext.Create;
  try
    RttiType := ctx.GetType(Data.ClassType);
    RttiField := RttiType.GetField(Field);
    ArrayValue := RttiField.GetValue(Data);

    Clazz := GetClass(RttiField);
    SetLength(Values, Length(Args));
    for i := 0 to Length(Args) - 1 do
      Values[i] := GetObjectValue(Args[i], Clazz);

    NewArrayValue := TValue.FromArray(ArrayValue.TypeInfo, Values);
    RttiField.SetValue(Data, NewArrayValue);
  finally
    ctx.Free;
  end;
end;

{ TNullableArrayIntermediateObject }

constructor TNullableArrayIntermediateObject.Create(Value: TValue);
var
  i: integer;
  Len: integer;
begin
  FNullableArrayIntermediateObject := TNullableArrayIntermediateObjectA.Create;

  Len := Value.GetArrayLength;
  SetLength(FNullableArrayIntermediateObject.FValue, Len);

  for i := 0 to Len - 1 do
    FNullableArrayIntermediateObject.FValue[i] := Value.GetArrayElement(i).AsObject;
end;

destructor TNullableArrayIntermediateObject.Destroy;
begin
  FreeAndNil(FNullableArrayIntermediateObject);

  inherited;
end;

end.
