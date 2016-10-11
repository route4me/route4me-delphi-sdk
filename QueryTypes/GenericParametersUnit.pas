unit GenericParametersUnit;

interface

uses
  System.JSON, System.JSON.Writers, REST.JsonReflect, JsonSerializerUnit,
  JSONNullableAttributeUnit;

type

  TGenericParameters = class

  public
    function ToJsonString: String;
  end;

implementation

{ TGenericParameters }

uses JSONNullableConverterUnit;

function TGenericParameters.ToJsonString: String;
var
  Mar: TJSONMarshal;  //Serializer
begin
//  Result := TJsonSerializer.ObjectToJsonString(Self);

  Mar := TJSONMarshal.Create(TJSONNullableConverter.Create);
//  Mar.RegisterConverter(Self, );
  Result := Mar.Marshal(Self).ToString;
end;

end.
