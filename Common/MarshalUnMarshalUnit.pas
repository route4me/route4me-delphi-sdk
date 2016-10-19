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

//    class procedure InitIntermediateObject(AClass: TClass; JsonObject: TJSONObject); static;
  public
    class function ToJson(GenericParameters: TGenericParameters): String; static;
//    class function FromJson(JsonString: String): TGenericParameters; overload; static;
    class function FromJson(AClass: TClass; JsonString: String): TObject; overload; static;
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
//      InitIntermediateObject(AClass, JsonObject);
      Result := Unmarshal.CreateObject(AClass, JsonObject);
    finally
      JsonObject.Free;
    end;
  finally
    Unmarshal.Free;
  end;
end;

(*class procedure TMarshalUnMarshal.InitIntermediateObject(AClass: TClass;
  JsonObject: TJSONObject);
var
  ctx: TRttiContext;
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
          for Attr1 in RttiField.GetAttributes do
            if (Attr1 is JSONNullableAttribute) then
            begin
              Field := RttiField;
              Exit(True);
            end;
        end;
  end;
var
  i: integer;
  Name: String;
  Value: TJSONValue;
  RttiRecordField: TRttiField;
  ConcreteNullableIntermediateObject: TBaseNullableIntermediateObject;
  NullableIntermediateObject: TNullableIntermediateObject;
  RttiRecord: TRttiRecordType;
  IsNullField, ValueField: TRttiField;
  JsonIntermediateObject: TJSONValue;
  Marshal: TJSONMarshal;
begin
{
	"integer_null": {
		"nullableIntermediateObject": {
			"value": 6471586,
			"isNull": true,
			"isRequired": true
		}
	},
	"integer_null_but_not_need_save": {
		"nullableIntermediateObject": {
			"value": 6471586,
			"isNull": true,
			"isRequired": false
		}
	},
	"integer_not_null": {
		"nullableIntermediateObject": {
			"value": 123,
			"isNull": false,
			"isRequired": false
		}
	}
}

  ctx := TRttiContext.Create;
  Marshal := TJSONMarshal.Create();
  try
    RttiType := ctx.GetType(AClass);

    for i := JsonObject.Count - 1 downto 0 do
    begin
      Name := JsonObject.Pairs[i].JsonString.Value;
      if IsNullabledAttribute(Name, RttiRecordField) then
      begin
        if (not RttiRecordField.FieldType.IsRecord) then
          raise Exception.Create('The field marked attribute "JSONNullable" must be a record.');

        ConcreteNullableIntermediateObject := nil;
        Value := JsonObject.Pairs[i].JsonValue;
{        if (Value is TJSONObject) then
          ConcreteNullableIntermediateObject := TNullableObjectIntermediateObject.Create(TJSONObject(Value).Value);}
        if (Value is TJSONString) then
          ConcreteNullableIntermediateObject := TNullableStringIntermediateObject.Create(TJSONString(Value).Value);
        if (Value is TJSONNumber) then
        begin
          RttiRecord := RttiRecordField.FieldType.AsRecord;
          ValueField := RttiRecord.GetField(ValueFieldCaption);

          if (ValueField.FieldType.TypeKind = tkInteger) then
            ConcreteNullableIntermediateObject := TNullableIntegerIntermediateObject.Create(TJSONNumber(Value).AsInt);
          if (ValueField.FieldType.TypeKind = tkFloat) then
            ConcreteNullableIntermediateObject := TNullableDoubleIntermediateObject.Create(TJSONNumber(Value).AsDouble);
        end;
        if (Value is TJSONTrue) then
            ConcreteNullableIntermediateObject := TNullableBooleanIntermediateObject.Create(True);
        if (Value is TJSONFalse) then
            ConcreteNullableIntermediateObject := TNullableBooleanIntermediateObject.Create(False);

        if (ConcreteNullableIntermediateObject <> nil) then
        begin
          ConcreteNullableIntermediateObject.IsNull := False;
          NullableIntermediateObject := TNullableIntermediateObject.Create(ConcreteNullableIntermediateObject);
          JsonIntermediateObject := Marshal.Marshal(NullableIntermediateObject);

          JsonObject.RemovePair(Name);
          JsonObject.AddPair(Name, JsonIntermediateObject);
        end;
      end;
    end;
  finally
    Marshal.Free;
    ctx.Free;
  end;
end;
*)
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
