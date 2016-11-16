unit UpdateRoutesCustomDataRequestUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit, JSONNullableAttributeUnit, NullableBasicTypesUnit,
  JSONDictionaryIntermediateObjectUnit, HttpQueryMemberAttributeUnit;

type
  TUpdateRoutesCustomDataRequest = class(TGenericParameters)
  private
    [JSONMarshalled(False)]
    [Nullable]
    [HttpQueryMember('route_id')]
    FRouteId: NullableString;

    [JSONMarshalled(False)]
    [Nullable]
    [HttpQueryMember('route_destination_id')]
    FRouteDestinationId: NullableInteger;

    [JSONName('custom_fields')]
    [NullableObject(TDictionaryStringIntermediateObject)]
    FCustomFields: NullableObject;

    function GetCustomFields: TDictionaryStringIntermediateObject;
    procedure SetCustomFields(const Value: TDictionaryStringIntermediateObject);
  public
    constructor Create; override;
    destructor Destroy; override;

    property RouteId: NullableString read FRouteId write FRouteId;

    property RouteDestinationId: NullableInteger read FRouteDestinationId write FRouteDestinationId;

    property CustomFields: TDictionaryStringIntermediateObject read GetCustomFields write SetCustomFields;
    procedure AddCustomField(Key: String; Value: String);
  end;

implementation

procedure TUpdateRoutesCustomDataRequest.AddCustomField(Key, Value: String);
var
  Dic: TDictionaryStringIntermediateObject;
begin
  if (FCustomFields.IsNull) then
    FCustomFields := TDictionaryStringIntermediateObject.Create();
  Dic := FCustomFields.Value as TDictionaryStringIntermediateObject;
  Dic.Add(Key, Value);
end;

constructor TUpdateRoutesCustomDataRequest.Create;
begin
  Inherited;

  FRouteId := NullableString.Null;
  FRouteDestinationId := NullableInteger.Null;
  FCustomFields := NullableObject.Null;
end;

destructor TUpdateRoutesCustomDataRequest.Destroy;
begin
  FCustomFields.Free;

  inherited;
end;

function TUpdateRoutesCustomDataRequest.GetCustomFields: TDictionaryStringIntermediateObject;
begin
  if FCustomFields.IsNull then
    Result := nil
  else
    Result := FCustomFields.Value as TDictionaryStringIntermediateObject;
end;

procedure TUpdateRoutesCustomDataRequest.SetCustomFields(
  const Value: TDictionaryStringIntermediateObject);
begin
  FCustomFields := Value;
end;

end.
