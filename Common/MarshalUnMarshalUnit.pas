unit MarshalUnMarshalUnit;

interface

uses
  GenericParametersUnit, System.Rtti, Classes, SysUtils,
  REST.JsonReflect, System.JSON, REST.Json.Types;

type
  TMarshalUnMarshal = class
  private
  const
    IsNullFieldCaption = 'FIsNull';
    ValueFieldCaption = 'FValue';


    class var ctx: TRttiContext;
    class var Marshal: TJSONMarshal;

    class procedure InitIntermediateObject({AClass: TClass; }TypeInfo: Pointer; JsonObject: TJSONObject);
  public
    class function ToJson(GenericParameters: TGenericParameters): String;
//    class function FromJson(JsonString: String): TGenericParameters; overload;
    class function FromJson(AClass: TClass; JsonString: String): TObject; overload;
  end;

implementation

{ TMarshalUnMarshal }

uses JSONNullableConverterUnit, JSONNullableAttributeUnit,
  NullableInterceptorUnit;

{class function TMarshalUnMarshal.FromJson(
  JsonString: String): TGenericParameters;
var
  Unmarshal: TJSONUnMarshal;
  JsonObject: TJSONObject;
  ResultObject: TObject;
begin
  Unmarshal := TJSONUnMarshal.Create();
  try
    JsonObject := TJSONObject.ParseJSONValue(JSONString) as TJSONObject;
    try
      ResultObject := Unmarshal.CreateObject(TGenericParameters, JsonObject);
      Result := ResultObject as TGenericParameters;
    finally
      JsonObject.Free;
    end;
  finally
    Unmarshal.Free;
  end;
end;}

class function TMarshalUnMarshal.FromJson(AClass: TClass; JsonString: String): TObject;
var
  Unmarshal: TJSONUnMarshal;
  JsonObject: TJSONObject;
begin
  Unmarshal := TJSONUnMarshal.Create();
  try
    JsonObject := TJSONObject.ParseJSONValue(JSONString) as TJSONObject;
    try

      ctx := TRttiContext.Create;
      Marshal := TJSONMarshal.Create;
      try
        InitIntermediateObject(AClass.ClassInfo, JsonObject);
      finally
        Marshal.Free;
        ctx.Free;
      end;

      Result := Unmarshal.CreateObject(AClass, JsonObject);
    finally
      JsonObject.Free;
    end;
  finally
    Unmarshal.Free;
  end;
end;

class procedure TMarshalUnMarshal.InitIntermediateObject({AClass: TClass;}TypeInfo: Pointer;
  JsonObject: TJSONObject);
var
  RttiType: TRttiType;

  function IsNullabledAttribute(Name: String; out Field: TRttiField): boolean;
  var
    RttiField: TRttiField;
    Attr, Attr1: TCustomAttribute;
  begin
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
  RttiRecordField: TRttiField;
  ObjectAsString: String;
  JSONValue: TJSONValue;
  ArrayItemObject: TJSONObject;
//  ItemClass: TClass;
  ItemTypeInfo: Pointer;
  elementType: TRttiType;
begin
  RttiType := ctx.GetType(TypeInfo{AClass});

  for i := JsonObject.Count - 1 downto 0 do
  begin
    Name := JsonObject.Pairs[i].JsonString.Value;
    if IsNullabledAttribute(Name, RttiRecordField) then
    begin
      if (not RttiRecordField.FieldType.IsRecord) then
        raise Exception.Create('The field marked attribute "JSONNullable" must be a record.');

      Value := JsonObject.Pairs[i].JsonValue;

      if (Value is TJSONTrue) then
      begin
        JsonObject.RemovePair(Name);
        JsonObject.AddPair(Name, 'true');
      end;
      if (Value is TJSONFalse) then
      begin
        JsonObject.RemovePair(Name);
        JsonObject.AddPair(Name, 'false');
      end;

      if (Value is TJSONObject) then
      begin
        ObjectAsString := TJSONObject(Value).ToString;
        JsonObject.RemovePair(Name);
        JsonObject.AddPair(Name, ObjectAsString);
      end;
    end;

    JSONValue := JsonObject.Pairs[i].JsonValue;
    if (JSONValue is TJSONArray) then
    begin
      if RttiRecordField.FieldType is TRttiArrayType then
        elementType := TRttiArrayType(RttiRecordField.FieldType).elementType
      else
        elementType := TRttiDynamicArrayType(RttiRecordField.FieldType).elementType;
      ItemTypeInfo := elementType.Handle;

      for j := 0 to TJSONArray(JSONValue).Count - 1 do
        if TJSONArray(JSONValue).Items[j] is TJSONObject then
        begin
          ArrayItemObject := TJSONObject(TJSONArray(JSONValue).Items[j]);
          InitIntermediateObject(ItemTypeInfo, ArrayItemObject);
        end;
    end;
  end;
end;

class function TMarshalUnMarshal.ToJson(
  GenericParameters: TGenericParameters): String;
var
  Marshal: TJSONMarshal;
begin
  Marshal := TJSONMarshal.Create(TJSONNullableConverter.Create);
  try
    Result := Marshal.Marshal(GenericParameters).ToString;
  finally
    Marshal.Free;
  end;
end;

end.
