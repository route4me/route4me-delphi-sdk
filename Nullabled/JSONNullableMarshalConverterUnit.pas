unit JSONNullableMarshalConverterUnit;

interface

uses
  System.JSON, REST.JsonReflect, System.RTTI, SysUtils,
  System.Generics.Collections;

type
  TJSONNullableConverter = class(TJSONConverter)
  private
    procedure RemoveNullableFields(Root: TJSONAncestor);
    procedure PrepareDictionaryFields(Root: TJSONAncestor);

    function IsNullableObject(JSONAncestor: TJSONAncestor;
      out IsRequired: boolean; out Value: TValue; out IsNull: boolean): boolean;
    function IsNullableArray(JSONAncestor: TJSONAncestor;
      out IsRequired: boolean; out Value: TJSONValue): boolean;
    function IsDictionaryIntermediateObject(JSONAncestor: TJSONAncestor;
      out Pairs: TArray<TJSONPair>): boolean;
  protected
    function GetSerializedData: TJSONValue; override;
  end;

implementation

{ TJSONNullableConverter }

uses
  NullableInterceptorUnit, NullableArrayInterceptorUnit;

function TJSONNullableConverter.GetSerializedData: TJSONValue;
begin
  Result := Inherited GetSerializedData;

  RemoveNullableFields(Result);
  PrepareDictionaryFields(Result);
end;

function TJSONNullableConverter.IsDictionaryIntermediateObject(
  JSONAncestor: TJSONAncestor; out Pairs: TArray<TJSONPair>): boolean;
var
  JSONObject: TJSONObject;
  JSONPair: TJSONPair;
  Value: TJSONValue;
  DictName: String;
  DictValue: TJSONValue;
begin
  Result := False;
  Pairs := nil;

  if (JSONAncestor is TJSONObject) then
  begin
    JSONObject := TJSONObject(JSONAncestor);
    if (JSONObject.Count = 1) and (JSONObject.Pairs[0].JsonString <> nil) then
    begin
      JSONPair := JSONObject.Pairs[0];
      if (JSONPair.JsonString.Value.ToUpper = 'DictionaryIntermediateObject'.ToUpper) then
      begin
        SetLength(Pairs, 0);

        for Value in (JSONPair.JsonValue as TJSONArray) do
        begin
          DictName := ((Value as TJSONArray).Items[0] as TJSONString).Value;
          DictValue := (Value as TJSONArray).Items[1];

          SetLength(Pairs, Length(Pairs) + 1);
          Pairs[High(Pairs)] := TJSONPair.Create(DictName, DictValue);
        end;

        Exit(True);
      end;
    end;
  end;
end;

function TJSONNullableConverter.IsNullableArray(JSONAncestor: TJSONAncestor;
  out IsRequired: boolean; out Value: TJSONValue): boolean;
var
  JSONObject: TJSONObject;
  JSONPair: TJSONPair;
begin
  Result := False;

  if (JSONAncestor is TJSONObject) then
  begin
    JSONObject := TJSONObject(JSONAncestor);
    if (JSONObject.Count = 1) and (JSONObject.Pairs[0].JsonString <> nil) then
    begin
      JSONPair := JSONObject.Pairs[0];
      Result := ('T' + JSONPair.JsonString.Value.ToUpper = TNullableArrayIntermediateObject.ClassName.ToUpper);
      if not Result then
        Exit;

      JSONObject := TJSONObject(JSONPair.JsonValue);
      Value := JSONObject.GetValue('value');
      IsRequired := (JSONObject.GetValue('isRequired') as TJSONBool).AsBoolean;
    end;
  end;
end;

function TJSONNullableConverter.IsNullableObject(JSONAncestor: TJSONAncestor;
  out IsRequired: boolean; out Value: TValue; out IsNull: boolean): boolean;
var
  JSONObject: TJSONObject;
  JSONPair: TJSONPair;
begin
  Result := False;

  if (JSONAncestor is TJSONObject) then
  begin
    JSONObject := TJSONObject(JSONAncestor);
    if (JSONObject.Count = 1) and (JSONObject.Pairs[0].JsonString <> nil) then
    begin
      JSONPair := JSONObject.Pairs[0];
      Result := ('T' + JSONPair.JsonString.Value.ToUpper = TNullableIntermediateObject.ClassName.ToUpper);
      if not Result then
        Exit;

      IsNull := (TJSONObject(JSONPair.JsonValue).GetValue('isNull') as TJSONBool).AsBoolean;
      IsRequired := (TJSONObject(JSONPair.JsonValue).GetValue('isRequired') as TJSONBool).AsBoolean;
      Value := TJSONObject(JSONPair.JsonValue).GetValue('value');
    end;
  end;
end;

procedure TJSONNullableConverter.PrepareDictionaryFields(Root: TJSONAncestor);
var
  i: integer;
  JSONObject: TJSONObject;
  JSONValue: TJSONValue;
  Pairs: TArray<TJSONPair>;
  Pair: TJSONPair;
  Name: String;
  NewJSONValue: TJSONObject;
begin
  if (Root is TJSONArray) then
    for JSONValue in TJSONArray(Root) do
      PrepareDictionaryFields(JSONValue);

  if (Root is TJSONObject) then
  begin
    JSONObject := TJSONObject(Root);
    i := 0;
    while (i < JSONObject.Count) do
    begin
      if IsDictionaryIntermediateObject(JSONObject.Pairs[i].JsonValue, Pairs) then
      begin
        Name := JSONObject.Pairs[i].JsonString.Value;
        JSONObject.RemovePair(JSONObject.Pairs[i].JsonString.Value);

        NewJSONValue := TJSONObject.Create;
        for Pair in Pairs do
          NewJSONValue.AddPair(Pair);
        JSONObject.AddPair(Name, NewJSONValue);
      end
      else
      begin
        PrepareDictionaryFields(JSONObject.Pairs[i].JsonValue);
        inc(i);
      end;
    end;
  end;
end;

procedure TJSONNullableConverter.RemoveNullableFields(Root: TJSONAncestor);
var
  i: integer;
  JSONObject: TJSONObject;
  IsRequired: boolean;
  IsNull: boolean;
  Value: TValue;
  Name: String;
  JSONValue: TJSONValue;
begin
  if (Root is TJSONArray) then
    for JSONValue in TJSONArray(Root) do
      RemoveNullableFields(JSONValue);

  if (Root is TJSONObject) then
  begin
    JSONObject := TJSONObject(Root);
    i := 0;
    while (i < JSONObject.Count) do
    begin
      if IsNullableObject(JSONObject.Pairs[i].JsonValue, IsRequired, Value, IsNull) then
      begin
        Name := JSONObject.Pairs[i].JsonString.Value;
        JSONValue := (Value.AsObject as TJsonValue).Clone as TJsonValue;
        JSONObject.RemovePair(JSONObject.Pairs[i].JsonString.Value);

        if (not IsNull) then
          JSONObject.AddPair(Name, JSONValue)
        else
        if IsRequired then
          JSONObject.AddPair(Name, TJSONNull.Create);
      end
      else
      if IsNullableArray(JSONObject.Pairs[i].JsonValue, IsRequired, JSONValue) then
      begin
        Name := JSONObject.Pairs[i].JsonString.Value;
        JSONValue := JSONValue.Clone as TJsonValue;
        JSONObject.RemovePair(JSONObject.Pairs[i].JsonString.Value);

        IsNull := (JSONValue is TJSONNull) or (
          (JSONValue is TJSONArray) and (TJSONArray(JSONValue).Count = 0));
        if (not IsNull) then
          JSONObject.AddPair(Name, JSONValue)
        else
        if IsRequired then
          JSONObject.AddPair(Name, TJSONNull.Create);
      end
      else
      begin
        RemoveNullableFields(JSONObject.Pairs[i].JsonValue);
        inc(i);
      end;
    end;
  end;
end;

end.
