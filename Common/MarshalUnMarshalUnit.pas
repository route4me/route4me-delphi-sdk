unit MarshalUnMarshalUnit;

interface

uses
  GenericParametersUnit, System.Rtti,
  REST.JsonReflect, System.JSON;

type
  TMarshalUnMarshal = class
  public
    class function ToJson(GenericParameters: TGenericParameters): String; static;
    class function FromJson(JsonString: String): TGenericParameters; overload; static;
    class function FromJson(AClass: TClass; JsonString: String): TObject; overload; static;
  end;

implementation

{ TMarshalUnMarshal }

uses JSONNullableConverterUnit;

class function TMarshalUnMarshal.FromJson(
  JsonString: String): TGenericParameters;
var
  Unmarshal: TJSONUnMarshal;
  JsonObject: TJSONObject;
  ResultObject: TObject;
begin
  Unmarshal := TJSONUnMarshal.Create({TJSONNullableConverter.Create});
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
end;

class function TMarshalUnMarshal.FromJson(AClass: TClass; JsonString: String): TObject;
var
  Unmarshal: TJSONUnMarshal;
  JsonObject: TJSONObject;
begin
  Unmarshal := TJSONUnMarshal.Create({TJSONNullableConverter.Create});
  try
    JsonObject := TJSONObject.ParseJSONValue(JSONString) as TJSONObject;
    try
      Result := Unmarshal.CreateObject(AClass, JsonObject);
    finally
      JsonObject.Free;
    end;
  finally
    Unmarshal.Free;
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
