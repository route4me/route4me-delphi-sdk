unit NullableInterceptorUnit;

interface

uses
  REST.JsonReflect, Rtti, SysUtils;

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

  TNullableInt64IntermediateObject = class(TBaseNullableIntermediateObject)
  private
    FValue: int64;
  public
    constructor Create(Value: int64);
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

  TNullableIntermediateObject = class
  private
    FNullableIntermediateObject: TBaseNullableIntermediateObject;
  public
    constructor Create(IntermediateObject: TBaseNullableIntermediateObject);
  end;

  TNullableInterceptor = class(TJSONInterceptor)
  private
  const
    IsNullFieldCaption = 'FIsNull';
    ValueFieldCaption = 'FValue';
  public
    /// <summary>Converters that transforms a field value into an
    /// intermediate object</summary>
    /// <param name="Data">Current object instance being serialized</param>
    /// <param name="Field">Field name</param>
    /// <result> a serializable object </result>
    function ObjectConverter(Data: TObject; Field: string): TObject; override;
    /// <summary>Reverter that sets an object field to a value based on
    /// an intermediate object</summary>
    /// <param name="Data">Current object instance being populated</param>
    /// <param name="Field">Field name</param>
    /// <param name="Arg"> intermediate object </param>
//    procedure ObjectReverter(Data: TObject; Field: string; Arg: TObject); override;
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
  JSONNullableAttributeUnit;

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
        raise Exception.Create('The field marked attribute "JSONNullable" must be a record.');

    IsRequired := False;
    IsRequiredFound := False;
    for Attr in RttiField.GetAttributes do
    begin
      if Attr is JSONNullableAttribute then
      begin
        IsRequired := JSONNullableAttribute(attr).IsRequired;
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
        tkInt64:
          ValueIntermediateObject := TNullableInt64IntermediateObject.Create(Value.AsInt64);
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

{procedure TNullableInterceptor.ObjectReverter(Data: TObject; Field: string;
  Arg: TObject);
var
  ctx: TRttiContext;
  RttiType: TRttiType;
  RttiField, IsNullField, ValueField: TRttiField;
  RttiRecord: TRttiRecordType;
  Attr: TCustomAttribute;
  IsRequired: boolean;
  IsRequiredFound: boolean;
  Ptr: Pointer;
begin
  ctx := TRttiContext.Create;
  try
    RttiType := ctx.GetType(Data.ClassType);
    RttiField := RttiType.GetField(Field);
    Ptr := RttiField.GetValue(Data).GetReferenceToRawData;

    if (not RttiField.FieldType.IsRecord) then
        raise Exception.Create('The field marked attribute "JSONNullable" must be a record.');

    IsRequired := False;
    IsRequiredFound := False;
    for Attr in RttiField.GetAttributes do
    begin
      if Attr is JSONNullableAttribute then
      begin
        IsRequired := JSONNullableAttribute(attr).IsRequired;
        IsRequiredFound := True;
      end;
    end;
    if not IsRequiredFound then
      raise Exception.Create('This intercepter (TNullableInterceptor) was created not in JSONNullableAttribute.');

    RttiRecord := RttiField.FieldType.AsRecord;

    IsNullField := RttiRecord.GetField(IsNullFieldCaption);
    ValueField := RttiRecord.GetField(ValueFieldCaption);

    if (Arg = nil) then
      IsNullField.SetValue(Ptr, TValue.From(True));

  finally
    ctx.Free;
  end;
end;
 }
procedure TNullableInterceptor.ObjectsReverter(Data: TObject; Field: string;
  Args: TListOfObjects);
var
  ctx: TRttiContext;
  RttiType: TRttiType;
  RttiField, IsNullField, ValueField: TRttiField;
  RttiRecord: TRttiRecordType;
  Attr: TCustomAttribute;
  IsRequired: boolean;
  IsRequiredFound: boolean;
  Ptr: Pointer;
begin
  if (Args <> nil) then
    Exit;

  ctx := TRttiContext.Create;
  try
    RttiType := ctx.GetType(Data.ClassType);
    RttiField := RttiType.GetField(Field);
    Ptr := RttiField.GetValue(Data).GetReferenceToRawData;

    if (not RttiField.FieldType.IsRecord) then
        raise Exception.Create('The field marked attribute "JSONNullable" must be a record.');

    IsRequired := False;
    IsRequiredFound := False;
    for Attr in RttiField.GetAttributes do
    begin
      if Attr is JSONNullableAttribute then
      begin
        IsRequired := JSONNullableAttribute(attr).IsRequired;
        IsRequiredFound := True;
      end;
    end;
    if not IsRequiredFound then
      raise Exception.Create('This intercepter (TNullableInterceptor) was created not in JSONNullableAttribute.');

    RttiRecord := RttiField.FieldType.AsRecord;

    IsNullField := RttiRecord.GetField(IsNullFieldCaption);
    ValueField := RttiRecord.GetField(ValueFieldCaption);

    IsNullField.SetValue(Ptr, TValue.From(True));

  finally
    ctx.Free;
  end;
end;

procedure TNullableInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
var
  ctx: TRttiContext;
  RttiType: TRttiType;
  RttiField, IsNullField, ValueField: TRttiField;
  RttiRecord: TRttiRecordType;
  Attr: TCustomAttribute;
  IsRequired: boolean;
  IsRequiredFound: boolean;
  Ptr: Pointer;

  TempS: String;
begin
  ctx := TRttiContext.Create;
  try
    RttiType := ctx.GetType(Data.ClassType);
    RttiField := RttiType.GetField(Field);
    Ptr := RttiField.GetValue(Data).GetReferenceToRawData;

    if (not RttiField.FieldType.IsRecord) then
        raise Exception.Create('The field marked attribute "JSONNullable" must be a record.');

    IsRequired := False;
    IsRequiredFound := False;
    for Attr in RttiField.GetAttributes do
    begin
      if Attr is JSONNullableAttribute then
      begin
        IsRequired := JSONNullableAttribute(attr).IsRequired;
        IsRequiredFound := True;
      end;
    end;
    if not IsRequiredFound then
      raise Exception.Create('This intercepter (TNullableInterceptor) was created not in JSONNullableAttribute.');

    RttiRecord := RttiField.FieldType.AsRecord;

    IsNullField := RttiRecord.GetField(IsNullFieldCaption);
    IsNullField.SetValue(Ptr, TValue.From(False));

    ValueField := RttiRecord.GetField(ValueFieldCaption);
    TempS := 'qwe';
    TempS := ValueField.GetValue(Ptr).AsString;
    ValueField.SetValue(Ptr, TValue.From(Arg));
    TempS := ValueField.GetValue(Ptr).AsString;


{    RttiField := RttiType.GetField('FDebug');
    Ptr := Pointer(Data);
    RttiField.SetValue(Ptr, TValue.From(Arg));}
  finally
    ctx.Free;
  end;
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

{ TNullableInt64IntermediateObject }

constructor TNullableInt64IntermediateObject.Create(Value: int64);
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

{ TNullableObjectIntermediateObject }

constructor TNullableObjectIntermediateObject.Create(Value: TObject);
begin
  FValue := Value;
end;

end.
