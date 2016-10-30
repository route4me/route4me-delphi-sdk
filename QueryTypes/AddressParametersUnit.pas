unit AddressParametersUnit;

interface

uses
  NullableBasicTypesUnit,
  GenericParametersUnit, HttpQueryMemberAttributeUnit, JSONNullableAttributeUnit;

type
  TAddressParameters = class(TGenericParameters)
  private
    [HttpQueryMember('route_id')]
    [Nullable]
    FRouteId: NullableString;

    [HttpQueryMember('route_destination_id')]
    [Nullable]
    FRouteDestinationId: NullableInteger;

    [HttpQueryMember('notes')]
    FNotes: boolean;
  public
    constructor Create; override;

    property RouteId: NullableString read FRouteId write FRouteId;
    property RouteDestinationId: NullableInteger read FRouteDestinationId write FRouteDestinationId;
    property Notes: Boolean read FNotes write FNotes;
  end;
implementation

{ TAddressParameters }

constructor TAddressParameters.Create;
begin
  Inherited Create;

  FNotes := False;

  FRouteId := NullableString.Null;
  FRouteDestinationId := NullableInteger.Null;
end;

end.
