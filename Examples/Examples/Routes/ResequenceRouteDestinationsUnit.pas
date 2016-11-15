unit ResequenceRouteDestinationsUnit;

interface

uses SysUtils, BaseOptimizationExampleUnit, DataObjectUnit;

type
  TResequenceRouteDestinations = class(TBaseOptimizationExample)
  public
    procedure Execute(Route: TDataObjectRoute);
  end;

implementation

uses
  AddressUnit, AddressesOrderInfoUnit;

procedure TResequenceRouteDestinations.Execute(Route: TDataObjectRoute);
var
  AddressesOrderInfo: TAddressesOrderInfo;
  Address: TAddress;
  AddressInfo: TAddressInfo;
  i: integer;
  SequenceNo: integer;
  ErrorString: String;
  NewRoute: TDataObjectRoute;
begin
  if (Length(Route.Addresses) < 3) then
  begin
    WriteLn(Format('ResequenceRouteDestinations error: "%s"',
      ['Route.Addresses.Length < 3. Number of addresses should be >= 3']));
    Exit;
   end;

  // Switch 2 addresses after departure address:
  AddressesOrderInfo := TAddressesOrderInfo.Create(Route.RouteID);
  try
    for i := 0 to Length(Route.Addresses) - 1 do
    begin
      Address := Route.Addresses[i];

      SequenceNo := i;
      if (i = 1) then
        SequenceNo := 2
      else
        if (i = 2) then
          SequenceNo := 1;

      AddressInfo := TAddressInfo.Create;
      AddressInfo.DestinationId := Address.RouteDestinationId.Value;
      AddressInfo.SequenceNo := SequenceNo;
      AddressInfo.IsDepot := (AddressInfo.SequenceNo = 0);

      AddressesOrderInfo.AddAddress(AddressInfo);
    end;

    NewRoute := Route4MeManager.Route.Resequence(AddressesOrderInfo, ErrorString);
    try
      PrintExampleOptimizationResult(
        'ResequenceRouteDestinations, switch 2 addresses.', NewRoute, ErrorString);
      WriteLn('');
    finally
      FreeAndNil(NewRoute);
    end;
  finally
    FreeAndNil(AddressesOrderInfo);
  end;
end;

end.
