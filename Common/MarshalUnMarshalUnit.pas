unit MarshalUnMarshalUnit;

interface

uses
  GenericParametersUnit, System.Rtti, Classes, SysUtils,
  REST.JsonReflect, System.JSON, REST.Json.Types, System.Generics.Collections;

type
  TMarshalUnMarshal = class
  private
    class var ctx: TRttiContext;

    class procedure InitIntermediateObject(TypeInfo: Pointer; JsonObject: TJSONObject);
    class function MakeJsonObject(JsonValue: TJsonValue; out IsMakedObject: boolean): TJSONObject;
  public
    class function ToJson(GenericParameters: TGenericParameters): String;
    class function ToJsonValue(GenericParameters: TGenericParameters): TJSONValue;

    class function FromJson(AClass: TClass; JsonValue: TJsonValue): TObject;
  end;

implementation

{ TMarshalUnMarshal }

uses JSONNullableMarshalConverterUnit, JSONNullableAttributeUnit,
  NullableInterceptorUnit;

class function TMarshalUnMarshal.FromJson(AClass: TClass;
  JsonValue: TJsonValue): TObject;
var
  Marshal: TJSONMarshal;
  Unmarshal: TJSONUnMarshal;
  JsonObject: TJSONObject;
  IsMakedObject: boolean;
begin
  Unmarshal := TJSONUnMarshal.Create();
  try
    JsonObject := MakeJsonObject(JsonValue, IsMakedObject);
    if (JsonObject = nil) then
      Exit(nil);

    ctx := TRttiContext.Create;
    Marshal := TJSONMarshal.Create;
    try
      InitIntermediateObject(AClass.ClassInfo, JsonObject);
    finally
      FreeAndNil(Marshal);
      ctx.Free;
    end;

    Result := Unmarshal.CreateObject(AClass, JsonObject);

    if IsMakedObject then
      FreeAndNil(JsonObject);
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
  function IsNullabledArrayAttribute(Name: String): boolean;
  var
    RttiField: TRttiField;
    Attr, Attr1: TCustomAttribute;
  begin
    Result := False;
    for RttiField in RttiType.GetFields do
      for Attr in RttiField.GetAttributes do
        if (Attr is JSONNameAttribute) and (JSONNameAttribute(Attr).Name = Name) then
          for Attr1 in RttiField.GetAttributes do
            if (Attr1 is NullableArrayAttribute) then
              Exit(True);
  end;
var
  i, j, k: integer;
  Name: String;
  Value: TJSONValue;
  RttiField: TRttiField;
  ObjectAsString: String;
  JSONValue: TJSONValue;
  ArrayItemObject: TJSONObject;
  ItemTypeInfo: Pointer;
  elementType: TRttiType;
  ArraysItems: TArray<TJSONString>;
  JSONArray: TJSONArray;
  IsNullabledArrayAttr: boolean;
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
      begin
        if (Name = 'listHelper') then
          for j := 0 to JsonObject.Count - 1 do
            if (JsonObject.Pairs[j].JsonString.Value = 'items') then
            begin
              elementType := nil;
              for RttiField in RttiType.GetFields do
                if (RttiField.Name = 'FItems') then
                begin
                  elementType := (RttiField.FieldType as TRttiDynamicArrayType).ElementType;
                  Break;
                end;
              if (elementType = nil) then
                raise Exception.Create('Invalid collection initialization');

              ItemTypeInfo := elementType.Handle;

              JSONArray := JsonObject.Pairs[j].JsonValue as TJSONArray;
              for k := 0 to JSONArray.Count - 1 do
                InitIntermediateObject(ItemTypeInfo, JSONArray.Items[k] as TJSONObject);

              Break;
            end;

        Continue;
      end;

      JSONValue := JsonObject.Pairs[i].JsonValue;
      if (JSONValue is TJSONArray) then
      begin
        JSONArray := TJSONArray(JSONValue);
        if RttiField.FieldType is TRttiArrayType then
          elementType := TRttiArrayType(RttiField.FieldType).elementType
        else
          elementType := TRttiDynamicArrayType(RttiField.FieldType).elementType;
        if (elementType <> nil) then
        begin
          ItemTypeInfo := elementType.Handle;

          SetLength(ArraysItems, 0);
          IsNullabledArrayAttr := IsNullabledArrayAttribute(Name);
          for j := JSONArray.Count - 1 downto 0 do
            if JSONArray.Items[j] is TJSONObject then
            begin
              ArrayItemObject := TJSONObject(JSONArray.Items[j]);

              if IsNullabledArrayAttr then
              begin
                ObjectAsString := ArrayItemObject.ToString;
                JSONArray.Remove(j);

                SetLength(ArraysItems, Length(ArraysItems) + 1);
                ArraysItems[High(ArraysItems)] := TJSONString.Create(ObjectAsString);
              end
              else
                InitIntermediateObject(ItemTypeInfo, ArrayItemObject);
            end;

          if IsNullabledArrayAttr then
            for j := Length(ArraysItems) - 1 downto 0 do
              JSONArray.AddElement(ArraysItems[j]);

        end
        else
        begin
          JsonObject.RemovePair(Name);
          Continue;
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

class function TMarshalUnMarshal.MakeJsonObject(
  JsonValue: TJsonValue; out IsMakedObject: boolean): TJSONObject;
var
  i: integer;
  Arr: TJSONArray;
  ListHelperJSONValue: TJSONArray;
  ItemsJSONValue: TJSONArray;
  Item: TJSONObject;
begin
  IsMakedObject := False;
  if JsonValue is TJsonObject then
    Exit(TJSONObject(JsonValue));

  if JsonValue is TJSONArray then
  begin
    IsMakedObject := True;

    Arr := TJSONArray(JsonValue);
    Result := TJSONObject.Create();
    ListHelperJSONValue := TJSONArray.Create;
    ListHelperJSONValue.AddElement(TJSONNumber.Create(Arr.Count));
    Result.AddPair('listHelper', ListHelperJSONValue);

    ItemsJSONValue := TJSONArray.Create;
    for i := 0 to Arr.Count - 1 do
      ItemsJSONValue.AddElement(Arr.Items[i].Clone as TJSONValue);
    Result.AddPair('items', ItemsJSONValue);
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
