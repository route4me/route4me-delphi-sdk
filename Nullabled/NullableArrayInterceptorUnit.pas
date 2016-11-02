unit NullableArrayInterceptorUnit;

interface

uses
  Windows, REST.JsonReflect, Rtti, SysUtils, System.JSON, System.TypInfo,
  System.Generics.Collections;

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
  end;

  TNullableArrayInterceptor = class(TJSONInterceptor)
  private
  type
    TInternalJSONUnMarshal = class(TJSONUnMarshal);
  const
    IsNullFieldCaption = 'FIsNull';
    ValueFieldCaption = 'FValue';

    function GetObjectValue(s: String; Clazz: TClass): TValue;
  public
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
  RttiField, IsNullField, ValueField: TRttiField;
  RttiRecord: TRttiRecordType;
  Attr: TCustomAttribute;
  Ptr: Pointer;

  IsNull: boolean;
  Value: TValue;
  IsRequired: boolean;
  IsRequiredFound: boolean;
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
  Ptr: Pointer;
  ArrayValue, NewArrayValue: TValue;
  Value: TValue;
  i: integer;
  Values: array of TValue;
  ArrLength: LongInt;
begin
  inherited;
{Нужно:
1. Чтобы в метод TJSONUnMarshal.PopulateFields приходил в поле Addresses не массив объектов, а массив строк.
2. В данном методе нужно из массива строк загружать объект. Использовать для этого метод TMarshalUnMarshal.FromJson. Пример смотреть в TNullableInterceptor.StringReverter}

  if (Length(Args) = 0) then
    Exit;

  ctx := TRttiContext.Create;
  try
    RttiType := ctx.GetType(Data.ClassType);
    RttiField := RttiType.GetField(Field);
    ArrayValue := RttiField.GetValue(Data);
    Ptr := ArrayValue.GetReferenceToRawData;
//    TValue.Make(@Ptr, RttiField.FieldType.Handle, ArrayValue);

    Clazz := GetClass(RttiField);
    SetLength(Values, Length(Args));
    for i := 0 to Length(Args) - 1 do
      Values[i] := GetObjectValue(Args[i], Clazz);

//    ArrLength := Length(Args);
//    DynArraySetLength(Ptr, ArrayValue.TypeInfo, 1, @ArrLength);
    NewArrayValue := TValue.FromArray(ArrayValue.TypeInfo, Values);
//    RttiField.SetValue(Ptr, NewArrayValue);
//    ArrayValue.SetArrayElement(0, Values[0]);
    RttiField.SetValue(Data, NewArrayValue);

//    RttiField.SetValue(val.AsObject, arrValue);
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

end.
