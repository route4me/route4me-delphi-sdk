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
    /// <summary>Reverter that sets an object field to a value based on
    /// an array of strings</summary>
    /// <param name="Data">Current object instance being populated</param>
    /// <param name="Field">Field name</param>
    /// <param name="Args"> an array of strings </param>
    procedure StringsReverter(Data: TObject; Field: string; Args: TListOfStrings); override;
  end;

implementation

{ TNullableArrayInterceptor }

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

procedure TNullableArrayInterceptor.ObjectsReverter(Data: TObject;
  Field: string; Args: TListOfObjects);
begin
  inherited;

end;

procedure TNullableArrayInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  inherited;

end;

procedure TNullableArrayInterceptor.StringsReverter(Data: TObject;
  Field: string; Args: TListOfStrings);
begin
  inherited;
{Нужно:
1. Чтобы в метод TJSONUnMarshal.PopulateFields приходил в поле Addresses не массив объектов, а массив строк.
2. В данном методе нужно из массива строк загружать объект. Использовать для этого метод TMarshalUnMarshal.FromJson. Пример смотреть в TNullableInterceptor.StringReverter}
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
