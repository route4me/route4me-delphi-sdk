unit GetVendorsResponseUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit, VendorUnit;

type
  TGetVendorsResponse = class(TGenericParameters)
  private
    [JSONName('vendors')]
    FVendors: TVendorArray;
  public
    constructor Create;
    destructor Destroy; override;

    property Vendors: TVendorArray read FVendors write FVendors;
  end;

implementation

{ TGetVendorsResponse }

constructor TGetVendorsResponse.Create;
begin
  Inherited;

  SetLength(FVendors, 0);
end;

destructor TGetVendorsResponse.Destroy;
begin
  Finalize(FVendors);

  inherited;
end;

end.
