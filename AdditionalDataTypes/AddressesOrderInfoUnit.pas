unit AddressesOrderInfoUnit;

interface

uses
  REST.Json.Types,
  JSONNullableAttributeUnit, HttpQueryMemberAttributeUnit,
  GenericParametersUnit;

type
  TAddressInfo = class(TGenericParameters)
  private
    [JSONName('route_destination_id')]
    FDestinationId: integer;

    [JSONName('sequence_no')]
    FSequenceNo: integer;

    [JSONName('is_depot')]
    FIsDepot: boolean;
  public
    property DestinationId: integer read FDestinationId write FDestinationId;
    property SequenceNo: integer read FSequenceNo write FSequenceNo;
    property IsDepot: boolean read FIsDepot write FIsDepot;
  end;

  TAddressesOrderInfo = class(TGenericParameters)
  private
    [JSONMarshalled(False)]
    [HttpQueryMember('route_id')]
    FRouteId: String;

    [JSONName('addresses')]
    FAddresses: TArray<TAddressInfo>;
  public
    constructor Create(RouteId: String); reintroduce;

    procedure AddAddress(Address: TAddressInfo);

    property RouteId: String read FRouteId write FRouteId;
    property Addresses: TArray<TAddressInfo> read FAddresses write FAddresses;
  end;

implementation

{ TAddressesOrderInfo }

procedure TAddressesOrderInfo.AddAddress(Address: TAddressInfo);
begin
  SetLength(FAddresses, Length(FAddresses) + 1);
  FAddresses[High(FAddresses)] := Address;
end;

constructor TAddressesOrderInfo.Create(RouteId: String);
begin
  inherited Create;

  FRouteId := RouteId;
  SetLength(FAddresses, 0);
end;

end.
