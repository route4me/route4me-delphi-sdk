unit NullableInterceptorUnit;

interface

uses
  Windows, REST.JsonReflect, Rtti, SysUtils, System.JSON, System.TypInfo;

type
  TBaseNullableIntermediateObject = class abstract
  protected
    FIsNull: boolean;
    FIsRequired: boolean;
  public
    property IsNull: boolean read FIsNull write FIsNull;
    property IsRequired: boolean read FIsRequired write FIsRequired;
  end;

  TNullableObjectIntermediateObject = class(TBaseNullableIntermediateObject)
  private
    FValue: TObject;
  public
    constructor Create(Value: TObject);
  end;

  TNullableIntegerIntermediateObject = class(TBaseNullableIntermediateObject)
  private
    FValue: integer;
  public
    constructor Create(Value: integer);
  end;

  TNullableBooleanIntermediateObject = class(TBaseNullableIntermediateObject)
  private
    FValue: boolean;
  public
    constructor Create(Value: boolean);
  end;

  TNullableDoubleIntermediateObject = class(TBaseNullableIntermediateObject)
  private
    FValue: double;
  public
    constructor Create(Value: double);
  end;

  TNullableStringIntermediateObject = class(TBaseNullableIntermediateObject)
  private
    FValue: String;
  public
    constructor Create(Value: String);
  end;

  TNullableIntermediateObject = class(TInterfacedObject, IUnknown)
  private
    FNullableIntermediateObject: TBaseNullableIntermediateObject;
  public
    constructor Create(IntermediateObject: TBaseNullableIntermediateObject);
    destructor Destroy; override;
  end;

  TNullableInterceptor = class(TJSONInterceptor)
  private
  type
    TInternalJSONUnMarshal = class(TJSONUnMarshal);
  const
    IsNullFieldCaption = 'FIsNull';
    ValueFieldCaption = 'FValue';
  var
    function GetObjectValue(s: String; Clazz: TClass; UnMarshal: TInternalJSONUnMarshal): TValue;
  public
    /// <summary>Converters that transforms a field value into an
    /// intermediate object</summary>
    /// <param name="Data">Current object instance being serialized</param>
    /// <param name="Field">Field name</param>
    /// <result> a serializable object </result>
    function ObjectConverter(Data: TObject; Field: string): TObject; override;
    /// <summary>Field reverter that sets an object field to a value based on
    /// an array of intermediate objects</summary>
    /// <param name="Data">Current object instance being populated</param>
    /// <param name="Field">Field name</param>
    /// <param name="Args"> an array of objects </param>
    procedure ObjectsReverter(Data: TObject; Field: string; Args: TListOfObjects); override;
    /// <summary>Reverter that sets an object field to a value from
    /// a string</summary>
    /// <param name="Data">Current object instance being populated</param>
    /// <param name="Field">Field name</param>
    /// <param name="Arg">serialized value as a string </param>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

implementation

uses
  JSONNullableAttributeUnit, MarshalUnMarshalUnit;

var
  LocaleFormatSettings: TFormatSettings;

{ TNullableInterceptor }

function TNullableInterceptor.ObjectConverter(Data: TObject; Field: string): TObject;
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

  ValueIntermediateObject: TBaseNullableIntermediateObject;
begin
  Result := nil;

  ctx := TRttiContext.Create;

  try
    rttiType := ctx.GetType(Data.ClassType);
    RttiField := rttiType.GetField(Field);
    Ptr := RttiField.GetValue(Data).GetReferenceToRawData;

    if (not RttiField.FieldType.IsRecord) then
        raise Exception.Create('The field marked attribute "Nullable" must be a record.');

    IsRequired := False;
    IsRequiredFound := False;
    for Attr in RttiField.GetAttributes do
    begin
      if Attr is BaseJSONNullableAttribute then
      begin
        IsRequired := BaseJSONNullableAttribute(attr).IsRequired;
        IsRequiredFound := True;
      end;
    end;
    if not IsRequiredFound then
      raise Exception.Create('This intercepter (TNullableInterceptor) was created not in JSONNullableAttribute.');

    RttiRecord := RttiField.FieldType.AsRecord;

    IsNullField := RttiRecord.GetField(IsNullFieldCaption);

    if (IsNullField <> nil) and (IsNullField.FieldType.TypeKind = tkEnumeration) then
      IsNull := IsNullField.GetValue(Ptr).AsBoolean
    else
      raise Exception.Create(Format(
        'The field marked attribute "JSONNullableAttribute" must have a field "%s: boolean"', [IsNullFieldCaption]));

    ValueField := RttiRecord.GetField(ValueFieldCaption);
    if (ValueField <> nil) then
    begin
      Value := ValueField.GetValue(Ptr);
      case ValueField.FieldType.TypeKind of
        tkEnumeration:
          ValueIntermediateObject := TNullableBooleanIntermediateObject.Create(Value.AsBoolean);
        tkInteger:
          ValueIntermediateObject := TNullableIntegerIntermediateObject.Create(Value.AsInteger);
        tkFloat:
          ValueIntermediateObject := TNullableDoubleIntermediateObject.Create(Value.AsExtended);
        TTypeKind.tkString, TTypeKind.tkLString, TTypeKind.tkWString, TTypeKind.tkUString:
          ValueIntermediateObject := TNullableStringIntermediateObject.Create(Value.AsString);
        tkClass:
          ValueIntermediateObject := TNullableObjectIntermediateObject.Create(Value.AsObject);
      else
        raise Exception.Create(Format(
          'Unsupported type (%d) of the field marked attribute "JSONNullableAttribute"', [Integer(IsNullField.FieldType.TypeKind)]));
      end;

      ValueIntermediateObject.IsNull := IsNull;
      ValueIntermediateObject.IsRequired := IsRequired;

      Result := TNullableIntermediateObject.Create(ValueIntermediateObject);
    end
    else
      raise Exception.Create(Format(
        'The field marked attribute "JSONNullableAttribute" must have a field "%s"', [ValueFieldCaption]));
  finally
    ctx.Free;
  end;

  if (Result = nil) then
    raise Exception.Create('The result can not be undefinded!');
end;

{ TNullableBooleanIntermediateObject }

constructor TNullableBooleanIntermediateObject.Create(Value: boolean);
begin
  FValue := Value;
end;

{ TNullableIntegerIntermediateObject }

constructor TNullableIntegerIntermediateObject.Create(Value: integer);
begin
  FValue := Value;
end;

{ TNullableDoubleIntermediateObject }

constructor TNullableDoubleIntermediateObject.Create(Value: double);
begin
  FValue := Value;
end;

{ TNullableStringIntermediateObject }

constructor TNullableStringIntermediateObject.Create(Value: String);
begin
  FValue := Value;
end;

{ TNullableIntermediateObject }

constructor TNullableIntermediateObject.Create(
  IntermediateObject: TBaseNullableIntermediateObject);
begin
  FNullableIntermediateObject := IntermediateObject;
end;

destructor TNullableIntermediateObject.Destroy;
begin
  FreeAndNil(FNullableIntermediateObject);
  inherited;
end;

{ TNullableObjectIntermediateObject }

constructor TNullableObjectIntermediateObject.Create(Value: TObject);
begin
  FValue := Value;
end;

{ TNullableNumberAndStringInterceptor }

function TNullableInterceptor.GetObjectValue(s: String;
  Clazz: TClass; UnMarshal: TInternalJSONUnMarshal): TValue;
var
  Obj: TObject;
  JsonValue: TJsonValue;
begin
  JsonValue := TJsonObject.ParseJSONValue(s);
  try
    //Obj := UnMarshal.CreateObject(Clazz, JsonValue as TJsonObject);
    Obj := TMarshalUnMarshal.FromJson(Clazz, JsonValue);
    Result := TValue.From(Obj);
  finally
    FreeAndNil(JsonValue)
  end;
end;

procedure TNullableInterceptor.ObjectsReverter(Data: TObject;
  Field: string; Args: TListOfObjects);
var
  ctx: TRttiContext;
  RttiType: TRttiType;
  RttiField, IsNullField: TRttiField;
  RttiRecord: TRttiRecordType;
  Attr: TCustomAttribute;
  IsRequiredFound: boolean;
  Ptr: Pointer;
begin
  if (Args <> nil) then
    Exit;

  ctx := TRttiContext.Create;
  try
    RttiType := ctx.GetType(Data.ClassType);
    RttiField := RttiType.GetField(Field);

    if (not RttiField.FieldType.IsRecord) then
        raise Exception.Create('The field marked attribute "JSONNullable" must be a record.');

    IsRequiredFound := False;
    for Attr in RttiField.GetAttributes do
    begin
      if Attr is BaseJSONNullableAttribute then
        IsRequiredFound := True;
    end;
    if not IsRequiredFound then
      raise Exception.Create('This intercepter (TNullableInterceptor) was created not in JSONNullableAttribute.');

    RttiRecord := RttiField.FieldType.AsRecord;

    IsNullField := RttiRecord.GetField(IsNullFieldCaption);
    Ptr := RttiField.GetValue(Data).GetReferenceToRawData;
    IsNullField.SetValue(Ptr, TValue.From(True));

  finally
    ctx.Free;
  end;
end;

procedure TNullableInterceptor.StringReverter(Data: TObject; Field, Arg: String);
var
  ctx: TRttiContext;
  RttiType, RttiTypeObject: TRttiType;
  RttiRecordField, IsNullField, ValueField: TRttiField;
  RttiMethodObject: TRttiMethod;
  RttiRecord: TRttiRecordType;
  RecordValue: TValue;
  Attr: TCustomAttribute;
  Ptr: Pointer;
  Value: TValue;
  InternalUnMarshal: TInternalJSONUnMarshal;
  Clazz: TClass;
  ObjectType: TRttiType;
  NullableAttribute: BaseJSONNullableAttribute;
begin
  ctx := TRttiContext.Create;
  InternalUnMarshal := TInternalJSONUnMarshal.Create;
  try
    RttiType := ctx.GetType(Data.ClassType);
    RttiRecordField := RttiType.GetField(Field);

    if (not RttiRecordField.FieldType.IsRecord) then
        raise Exception.Create('The field marked attribute "JSONNullable" must be a record.');

    RttiRecord := RttiRecordField.FieldType.AsRecord;
    RecordValue := RttiRecordField.GetValue(Data);
    Ptr := RecordValue.GetReferenceToRawData;

    NullableAttribute := nil;
    for Attr in RttiRecordField.GetAttributes do
      if Attr is BaseJSONNullableAttribute then
        NullableAttribute := BaseJSONNullableAttribute(Attr);
    if (NullableAttribute = nil) then
      raise Exception.Create('This intercepter (TNullableInterceptor) was created not in JSONNullableAttribute.');

    IsNullField := RttiRecord.GetField(IsNullFieldCaption);
    if (IsNullField <> nil) then
      IsNullField.SetValue(Ptr, TValue.From(False))
    else
      raise Exception.Create(Format(
        'The field marked attribute "JSONNullableAttribute" must have a field "%s: boolean"', [IsNullFieldCaption]));

    ValueField := RttiRecord.GetField(ValueFieldCaption);
    if (ValueField <> nil) then
    begin
      ObjectType := ValueField.FieldType;
      if (ObjectType.TypeKind = tkClass) then
      begin
        if not (NullableAttribute is NullableObjectAttribute) then
          raise Exception.Create(
            'The field class of "NullableObject" must be marked as "NullableObject" Attribute');

        Clazz := NullableObjectAttribute(NullableAttribute).Clazz;
        RttiTypeObject := ctx.GetType(Clazz);
        RttiMethodObject := RttiTypeObject.GetMethod('FromJsonString');
        if (RttiMethodObject <> nil) then
        begin
          Value := RttiMethodObject.Invoke(Clazz, [Arg]);
        end
        else
          Value := GetObjectValue(Arg, Clazz, InternalUnMarshal);
      end
      else
      begin
        if (ObjectType.TypeKind = tkFloat) then
          Arg := FloatToJson(StrToFloat(Arg, LocaleFormatSettings));
        Value := InternalUnMarshal.StringToTValue(Arg, ValueField.FieldType.Handle);
      end;

      ValueField.SetValue(Ptr, Value);
    end
    else
      raise Exception.Create(Format(
        'The field marked attribute "JSONNullableAttribute" must have a field "%s"', [ValueFieldCaption]));

    RttiRecordField.SetValue(Data, RecordValue);
  finally
    FreeAndNil(InternalUnMarshal);
    ctx.Free;
  end;
end;

initialization
  LocaleFormatSettings := TFormatSettings.Create(LOCALE_USER_DEFAULT);
end.
