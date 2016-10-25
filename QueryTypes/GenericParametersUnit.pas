unit GenericParametersUnit;

interface

uses
  Classes, SysUtils, Rtti, System.Generics.Collections, System.JSON,
  REST.Json.Types, HttpQueryMemberAttributeUnit, JSONNullableAttributeUnit,
  CommonTypesUnit;

type

  TGenericParameters = class
  private
  const
    IsNullFieldCaption = 'FIsNull';
    ValueFieldCaption = 'FValue';
  protected
    [JSONMarshalled(False)]
    FConvertBooleansToInteger: boolean;

    [JSONMarshalled(False)]
    FParametersCollection: TListStringPair;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure AddParameter(Key, Value: String);

    function ToJsonString: String;
    function ToJsonValue: TJSONValue;
    function Serialize(ApiKey: String = ''): TListStringPair;
  end;

implementation

{ TGenericParameters }

uses
  Math,
  MarshalUnMarshalUnit;

procedure TGenericParameters.AddParameter(Key, Value: String);
begin
  FParametersCollection.Add(TStringPair.Create(Key, Value));
end;

constructor TGenericParameters.Create;
begin
  Inherited;

  FConvertBooleansToInteger := True;
  FParametersCollection := TListStringPair.Create;
end;

destructor TGenericParameters.Destroy;
begin
  FreeAndNil(FParametersCollection);

  inherited;
end;

function TGenericParameters.Serialize(ApiKey: String): TListStringPair;
  function GetHttpAttribute(Field: TRttiField): HttpQueryMemberAttribute;
  var
    Attr: TCustomAttribute;
  begin
      Result := nil;
      for Attr in Field.GetAttributes do
        if Attr is HttpQueryMemberAttribute then
          Exit(HttpQueryMemberAttribute(Attr));
  end;
  function IsNullableField(Field: TRttiField; out IsRequired: boolean): boolean;
  var
    Attr: TCustomAttribute;
  begin
    IsRequired := False;
    Result := False;
    for Attr in Field.GetAttributes do
      if Attr is NullableAttribute then
      begin
        IsRequired := NullableAttribute(Attr).IsRequired;
        Exit(True);
      end;
  end;

  function GetNullableFieldValue(Field: TRttiField): TValue;
  var
    RttiRecord: TRttiRecordType;
    ValueField, IsNullField: TRttiField;
    IsNull: boolean;
    Ptr: Pointer;
  begin
    if (not Field.FieldType.IsRecord) then
        raise Exception.Create('The field marked attribute "Nullable" must be a record.');
    RttiRecord := Field.FieldType.AsRecord;
    Ptr := Field.GetValue(Self).GetReferenceToRawData;

    IsNullField := RttiRecord.GetField(IsNullFieldCaption);
    if (IsNullField <> nil) and (IsNullField.FieldType.TypeKind = tkEnumeration) then
      IsNull := IsNullField.GetValue(Ptr).AsBoolean
    else
      raise Exception.Create(Format(
        'The field marked attribute "JSONNullableAttribute" must have a field "%s: boolean"', [IsNullFieldCaption]));
    if (IsNull) then
      Exit(TValue.From(nil));

    ValueField := RttiRecord.GetField(ValueFieldCaption);
    if (ValueField = nil) then
        raise Exception.Create(Format(
          'Unsupported type (%d) of the field marked attribute "JSONNullableAttribute"', [Integer(IsNullField.FieldType.TypeKind)]))
    else
      Result := ValueField.GetValue(Ptr);
  end;
var
  ctx: TRttiContext;
  RttiType: TRttiType;
  Field: TRttiField;
  Value: TValue;
  Attr: HttpQueryMemberAttribute;
  FieldName, FieldValue: String;
  Param: TStringPair;
  IsRequired: boolean;
begin
  Result := TListStringPair.Create;

  if (ApiKey <> EmptyStr) then
    Result.Add(TStringPair.Create('api_key', ApiKey));

  for Param in FParametersCollection do
    Result.Add(Param);

  ctx := TRttiContext.Create;
  try
    rttiType := ctx.GetType(Self.ClassType);

    for Field in rttiType.GetFields do
    begin
      Attr := GetHttpAttribute(Field);
      if (Attr = nil) then
        Continue;

      if (IsNullableField(Field, IsRequired)) then
      begin
        Value := GetNullableFieldValue(Field);
{        if Value.IsEmpty then
          Continue;}
      end
      else
      begin
        Value := Field.GetValue(Self);
      end;

      FieldName := Attr.Name;

      if (Value.IsEmpty) then
      begin
        if Attr.IsRequired then
          FieldValue := Attr.DefaultValue
        else
          Continue;

//        FieldValue := 'null';
      end
      else
      begin
        FieldValue := Value.ToString;

        if (Value.Kind = tkEnumeration) and (FConvertBooleansToInteger) then
          if Value.AsBoolean then
            FieldValue := '1'
          else
            FieldValue := '0';
      end;

      Result.Add(TStringPair.Create(FieldName, FieldValue));
    end;
  finally
    ctx.Free;
  end;
end;

function TGenericParameters.ToJsonString: String;
begin
  Result := TMarshalUnMarshal.ToJson(Self);
end;

function TGenericParameters.ToJsonValue: TJSONValue;
begin
  Result := TMarshalUnMarshal.ToJsonValue(Self);
end;

end.
