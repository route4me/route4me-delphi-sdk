unit JSONNullableMarshalConverterUnit;

interface

uses
  {$IF CompilerVersion < 27.0}
  Data.DBXJSONReflect,
  Data.DBXJSON,
  {$ELSE}
  REST.JsonReflect,
  System.JSON,
  {$IFEND}

  System.RTTI, SysUtils,
  System.Generics.Collections;

type
  TJSONNullableConverter = class(TJSONConverter)
  private
    FRemovedPairs: TObjectList<TObject>;

    function GetJsonObjectPairsCount(JSONObject: TJSONObject): integer;
    function GetJsonArrayLength(JSONArray: TJSONArray): integer;
    function GetJsonPair(JSONObject: TJSONObject; index: integer): TJSONPair;
    function GetJsonPairValue(JSONObject: TJSONObject; Name: String): TJSONValue;

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
  public
    destructor Destroy; override;
  end;

implementation

{ TJSONNullableConverter }

uses
  NullableInterceptorUnit, NullableArrayInterceptorUnit;

destructor TJSONNullableConverter.Destroy;
begin

  inherited;
end;

function TJSONNullableConverter.GetJsonArrayLength(
  JSONArray: TJSONArray): integer;
begin
  {$IF CompilerVersion < 27.0}
  Result := JSONArray.Size;
  {$ELSE}
  Result := JSONArray.Count;
  {$IFEND}
end;

function TJSONNullableConverter.GetJsonObjectPairsCount(
    JSONObject: TJSONObject): integer;
begin
  {$IF CompilerVersion < 27.0}
  Result := JSONObject.Size;
  {$ELSE}
  Result := JSONObject.Count;
  {$IFEND}
end;

function TJSONNullableConverter.GetJsonPair(JSONObject: TJSONObject;
  index: integer): TJSONPair;
begin
  {$IF CompilerVersion < 27.0}
  Result := JSONObject.Get(0);
  {$ELSE}
  Result := JSONObject.Pairs[0];
  {$IFEND}
end;

function TJSONNullableConverter.GetJsonPairValue(JSONObject: TJSONObject;
  Name: String): TJSONValue;
begin
  {$IF CompilerVersion < 27.0}
  Result := JSONObject.Get(Name).JsonValue;
  {$ELSE}
  Result := JSONObject.GetValue(Name);
  {$IFEND}
end;

function TJSONNullableConverter.GetSerializedData: TJSONValue;
begin
  Result := Inherited GetSerializedData;

  FRemovedPairs := TObjectList<TObject>.Create;
  RemoveNullableFields(Result);
  FreeAndNil(FRemovedPairs);

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

    if (GetJsonObjectPairsCount(JSONObject) = 1) then
    begin
      JSONPair := GetJsonPair(JSONObject, 0);

      if (JSONPair.JsonString <> nil) and
        (JSONPair.JsonString.Value.ToUpper = 'DictionaryIntermediateObject'.ToUpper) then
      begin
        SetLength(Pairs, 0);

        for Value in (JSONPair.JsonValue as TJSONArray) do
        begin
          {$IF CompilerVersion < 27.0}
          DictName := ((Value as TJSONArray).Get(0) as TJSONString).Value;
          DictValue := (Value as TJSONArray).Get(1);
          {$ELSE}
          DictName := ((Value as TJSONArray).Items[0] as TJSONString).Value;
          DictValue := (Value as TJSONArray).Items[1];
          {$IFEND}

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
  {$IF CompilerVersion < 27.0}
  JSONValue: TJSONValue;
  {$IFEND}
begin
  Result := False;

  if (JSONAncestor is TJSONObject) then
  begin
    JSONObject := TJSONObject(JSONAncestor);
    if (GetJsonObjectPairsCount(JSONObject) = 1) then
    begin
      JSONPair := GetJsonPair(JSONObject, 0);
      if (JSONPair.JsonString = nil) then
        Exit;

      Result := ('T' + JSONPair.JsonString.Value.ToUpper = TNullableArrayIntermediateObject.ClassName.ToUpper);
      if not Result then
        Exit;

      JSONObject := TJSONObject(JSONPair.JsonValue);
      Value := GetJsonPairValue(JSONObject, 'value');

      {$IF CompilerVersion < 27.0}
      JSONValue := GetJsonPairValue(JSONObject, 'isRequired');
      if JSONValue is TJSONFalse then
        IsRequired := False
      else
      if JSONValue is TJSONTrue then
        IsRequired := True
      else
        raise Exception.Create('Unexpected datatype of "isRequired"');
      {$ELSE}
        IsRequired := (JSONObject.GetValue('isRequired') as TJSONBool).AsBoolean;
      {$IFEND}
    end;
  end;
end;

function TJSONNullableConverter.IsNullableObject(JSONAncestor: TJSONAncestor;
  out IsRequired: boolean; out Value: TValue; out IsNull: boolean): boolean;
var
  JSONObject: TJSONObject;
  JSONPair: TJSONPair;
  {$IF CompilerVersion < 27.0}
  JSONValue: TJSONValue;
  {$IFEND}
begin
  Result := False;

  if (JSONAncestor is TJSONObject) then
  begin
    JSONObject := TJSONObject(JSONAncestor);
    if (GetJsonObjectPairsCount(JSONObject) = 1) then
    begin
      JSONPair := GetJsonPair(JSONObject, 0);
      if (JSONPair.JsonString <> nil) then
        Exit;

      Result := ('T' + JSONPair.JsonString.Value.ToUpper = TNullableIntermediateObject.ClassName.ToUpper);
      if not Result then
        Exit;

      JSONObject := TJSONObject(JSONPair.JsonValue);
      Value := GetJsonPairValue(JSONObject, 'value');
      {$IF CompilerVersion < 27.0}
      JSONValue := GetJsonPairValue(JSONObject, 'isRequired');
      if JSONValue is TJSONFalse then
        IsRequired := False
      else
      if JSONValue is TJSONTrue then
        IsRequired := True
      else
        raise Exception.Create('Unexpected datatype of "isRequired"');

      JSONValue := GetJsonPairValue(JSONObject, 'isNull');
      if JSONValue is TJSONFalse then
        IsNull := False
      else
      if JSONValue is TJSONTrue then
        IsNull := True
      else
        raise Exception.Create('Unexpected datatype of "isNull"');
      {$ELSE}
        IsNull := (GetJsonPairValue(JSONObject, 'isNull') as TJSONBool).AsBoolean;
        IsRequired := (GetJsonPairValue(JSONObject, 'isRequired') as TJSONBool).AsBoolean;
      {$IFEND}
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
  PairsCount: integer;
  JSONPair: TJSONPair;
begin
  if (Root is TJSONArray) then
    for JSONValue in TJSONArray(Root) do
      PrepareDictionaryFields(JSONValue);

  if (Root is TJSONObject) then
  begin
    JSONObject := TJSONObject(Root);
    PairsCount := GetJsonObjectPairsCount(JSONObject);
    i := 0;
    while (i < PairsCount) do
    begin
      JSONPair := GetJsonPair(JSONObject, i);

      if IsDictionaryIntermediateObject(JSONPair.JsonValue, Pairs) then
      begin
        Name := JSONPair.JsonString.Value;
        JSONObject.RemovePair(JSONPair.JsonString.Value);

        NewJSONValue := TJSONObject.Create;
        for Pair in Pairs do
          NewJSONValue.AddPair(Pair);
        JSONObject.AddPair(Name, NewJSONValue);
      end
      else
      begin
        PrepareDictionaryFields(JSONPair.JsonValue);
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
  PairsCount: integer;
  JSONPair: TJSONPair;
begin
  if (Root is TJSONArray) then
    for JSONValue in TJSONArray(Root) do
      RemoveNullableFields(JSONValue);

  if (Root is TJSONObject) then
  begin
    JSONObject := TJSONObject(Root);
    i := 0;
    PairsCount := GetJsonObjectPairsCount(JSONObject);
    while (i < PairsCount) do
    begin
      JSONPair := GetJsonPair(JSONObject, i);

      if IsNullableObject(JSONPair.JsonValue, IsRequired, Value, IsNull) then
      begin
        Name := JSONPair.JsonString.Value;
        JSONValue := (Value.AsObject as TJsonValue).Clone as TJsonValue;
        FRemovedPairs.Add(JSONObject.RemovePair(JSONPair.JsonString.Value));

        if (not IsNull) then
          JSONObject.AddPair(Name, JSONValue)
        else
        begin
          if IsRequired then
            JSONObject.AddPair(Name, TJSONNull.Create);
          FreeAndNil(JSONValue);
        end;
      end
      else
      if IsNullableArray(JSONPair.JsonValue, IsRequired, JSONValue) then
      begin
        Name := JSONPair.JsonString.Value;
        JSONValue := JSONValue.Clone as TJSONValue;
        FRemovedPairs.Add(JSONObject.RemovePair(JSONPair.JsonString.Value));

        IsNull := (JSONValue is TJSONNull) or (
          (JSONValue is TJSONArray) and (GetJsonArrayLength(TJSONArray(JSONValue)) = 0));
        if (not IsNull) then
          JSONObject.AddPair(Name, JSONValue)
        else
        begin
          if IsRequired then
            JSONObject.AddPair(Name, TJSONNull.Create);
          FreeAndNil(JSONValue);
        end;
      end
      else
      begin
        RemoveNullableFields(JSONPair.JsonValue);
        inc(i);
      end;
    end;
  end;
end;

end.
