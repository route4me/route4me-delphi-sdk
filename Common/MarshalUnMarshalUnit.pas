unit MarshalUnMarshalUnit;

interface

uses
  GenericParametersUnit, System.Rtti, Classes, SysUtils,
  REST.JsonReflect, System.JSON, REST.Json.Types;

type
  TMarshalUnMarshal = class
  private
    class var ctx: TRttiContext;

    class procedure InitIntermediateObject(TypeInfo: Pointer; JsonObject: TJSONObject);
  public
    class function ToJson(GenericParameters: TGenericParameters): String;
    class function ToJsonValue(GenericParameters: TGenericParameters): TJSONValue;

    class function FromJson(AClass: TClass; JsonValue: TJsonValue): TObject;
  end;

implementation

{ TMarshalUnMarshal }

uses JSONNullableConverterUnit, JSONNullableAttributeUnit,
  NullableInterceptorUnit;

class function TMarshalUnMarshal.FromJson(AClass: TClass;
  JsonValue: TJsonValue): TObject;
var
  Marshal: TJSONMarshal;
  Unmarshal: TJSONUnMarshal;
  JsonObject: TJSONObject;
begin
  Unmarshal := TJSONUnMarshal.Create();
  try
    JsonObject := JsonValue as TJSONObject;

    ctx := TRttiContext.Create;
    Marshal := TJSONMarshal.Create;
    try
      InitIntermediateObject(AClass.ClassInfo, JsonObject);
    finally
      FreeAndNil(Marshal);
      ctx.Free;
    end;

    Result := Unmarshal.CreateObject(AClass, JsonObject);
  finally
    FreeAndNil(Unmarshal);
  end;
end;

class procedure TMarshalUnMarshal.InitIntermediateObject(
  TypeInfo: Pointer; JsonObject: TJSONObject);
var
  RttiType: TRttiType;

  function IsNullabledAttribute(Name: String; out Field: TRttiField): boolean;
  var
    RttiField: TRttiField;
    Attr, Attr1: TCustomAttribute;
  begin
    Field := nil;
    Result := False;
    for RttiField in RttiType.GetFields do
      for Attr in RttiField.GetAttributes do
        if (Attr is JSONNameAttribute) and (JSONNameAttribute(Attr).Name = Name) then
        begin
          Field := RttiField;
          for Attr1 in RttiField.GetAttributes do
            if (Attr1 is BaseJSONNullableAttribute) then
            begin
              Exit(True);
            end;
        end;
  end;
var
  i, j: integer;
  Name: String;
  Value: TJSONValue;
  RttiField: TRttiField;
  ObjectAsString: String;
  JSONValue: TJSONValue;
  ArrayItemObject: TJSONObject;
  ItemTypeInfo: Pointer;
  elementType: TRttiType;
begin
  RttiType := ctx.GetType(TypeInfo);
  for i := JsonObject.Count - 1 downto 0 do
  begin
    Name := JsonObject.Pairs[i].JsonString.Value;
    try
      if IsNullabledAttribute(Name, RttiField) then
      begin
        if (not RttiField.FieldType.IsRecord) then
          raise Exception.Create('The field marked attribute "JSONNullable" must be a record.');

        Value := JsonObject.Pairs[i].JsonValue;

        if (Value is TJSONTrue) then
        begin
          JsonObject.RemovePair(Name);
          JsonObject.AddPair(Name, 'true');
          Continue;
        end;
        if (Value is TJSONFalse) then
        begin
          JsonObject.RemovePair(Name);
          JsonObject.AddPair(Name, 'false');
          Continue;
        end;

        if (Value is TJSONObject) then
        begin
          ObjectAsString := TJSONObject(Value).ToString;
          JsonObject.RemovePair(Name);
          JsonObject.AddPair(Name, ObjectAsString);
          Continue;
        end;
      end;

      if (RttiField = nil) then
        Continue;
    
      JSONValue := JsonObject.Pairs[i].JsonValue;
      if (JSONValue is TJSONArray) then
      begin
        if RttiField.FieldType is TRttiArrayType then
          elementType := TRttiArrayType(RttiField.FieldType).elementType
        else
          elementType := TRttiDynamicArrayType(RttiField.FieldType).elementType;
        ItemTypeInfo := elementType.Handle;

        for j := 0 to TJSONArray(JSONValue).Count - 1 do
          if TJSONArray(JSONValue).Items[j] is TJSONObject then
          begin
            ArrayItemObject := TJSONObject(TJSONArray(JSONValue).Items[j]);
            InitIntermediateObject(ItemTypeInfo, ArrayItemObject);
          end;
      end;
      if (JSONValue is TJSONObject) then
      begin
        ItemTypeInfo := RttiField.FieldType.Handle;
        InitIntermediateObject(ItemTypeInfo, TJSONObject(JSONValue));
      end;
    except
      on e: Exception do
        raise Exception.Create('Error of "' + Name + '" field name');
    end;
  end;
end;

class function TMarshalUnMarshal.ToJson(
  GenericParameters: TGenericParameters): String;
var
  Marshal: TJSONMarshal;
  Value: TJSONValue;
begin
  Marshal := TJSONMarshal.Create(TJSONNullableConverter.Create);
  try
    Value := Marshal.Marshal(GenericParameters);
    try
      Result := Value.ToString;
    finally
      FreeAndNil(Value);
    end;
  finally
    FreeAndNil(Marshal);
  end;
end;

class function TMarshalUnMarshal.ToJsonValue(
  GenericParameters: TGenericParameters): TJSONValue;
var
  Marshal: TJSONMarshal;
begin
  Marshal := TJSONMarshal.Create(TJSONNullableConverter.Create);
  try
    Result := Marshal.Marshal(GenericParameters);
  finally
    FreeAndNil(Marshal);
  end;
end;

end.
