unit GenericParametersUnit;

interface

uses
  Classes, SysUtils, Rtti, System.Generics.Collections,
  REST.Json.Types, HttpQueryMemberAttributeUnit, JSONNullableAttributeUnit;

type

  TGenericParameters = class
  private
  const
    IsNullFieldCaption = 'FIsNull';
    ValueFieldCaption = 'FValue';
  protected
    [JSONMarshalled(False)]
    FConvertBooleansToInteger: boolean;
  public
    constructor Create;

    function ToJsonString: String;
    function Serialize(ApiKey: String = ''): String;
  end;

implementation

{ TGenericParameters }

uses
  Math,
  MarshalUnMarshalUnit;

constructor TGenericParameters.Create;
begin
  Inherited;

  FConvertBooleansToInteger := True;
end;

function TGenericParameters.Serialize(ApiKey: String): String;
  function GetHttpAttribute(Field: TRttiField): HttpQueryMemberAttribute;
  var
    Attr: TCustomAttribute;
  begin
      Result := nil;
      for Attr in Field.GetAttributes do
        if Attr is HttpQueryMemberAttribute then
          Exit(HttpQueryMemberAttribute(Attr));
  end;
  function IsNullableField(Field: TRttiField): boolean;
  var
    Attr: TCustomAttribute;
  begin
      Result := False;
      for Attr in Field.GetAttributes do
        if Attr is NullableAttribute then
          Exit(True);
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
      Result := ValueField;
  end;
type
  TStringPair = TPair<String,String>;
var
  ctx: TRttiContext;
  RttiType: TRttiType;
  Field: TRttiField;
  Value: TValue;
  Attr: HttpQueryMemberAttribute;
  ParamsCollection: TList<TStringPair>;
  Pair: TStringPair;
  FieldName, FieldValue: String;
begin
  ParamsCollection := TList<TStringPair>.Create;
  try
    ctx := TRttiContext.Create;

    try
      rttiType := ctx.GetType(Self.ClassType);

      for Field in rttiType.GetFields do
      begin
        Attr := GetHttpAttribute(Field);
        if (Attr = nil) then
          Continue;

        if (IsNullableField(Field)) then
        begin
          Value := GetNullableFieldValue(Field);
          if Value.IsEmpty then
            Continue;
        end;

        FieldName := Field.Name;

        Value := Field.GetValue(Self);
        if (Value.IsEmpty) then
        begin
          if Attr.IsRequired then
            FieldValue := Attr.DefaultValue;

          FieldValue := 'null';
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

        ParamsCollection.Add(TStringPair.Create(FieldName, FieldValue));
      end;
    finally
      ctx.Free;
    end;

    if (ApiKey = EmptyStr) then
      Result := '?'
    else
      Result := Format('?api_key=%s', [ApiKey]);

    if (ParamsCollection.Count > 0) then
    begin
      for Pair in ParamsCollection do
        Result := Result + Format('%s=%s&', [Pair.Key, Pair.Value]);
      Delete(Result, Length(Result), 1);
    end;
  finally
    ParamsCollection.Free;
  end;
end;

function TGenericParameters.ToJsonString: String;
begin
  Result := TMarshalUnMarshal.ToJson(Self);
end;

end.
