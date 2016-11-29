unit AddOrderToRouteParameterProviderUnit;

interface

uses AddOrderToRouteRequestUnit, AddressUnit, RouteParametersUnit;

type
  IAddOrderToRouteParameterProvider = interface
    ['{794E1351-AC94-40F7-8D02-DC4D7FFDFC59}']
    function GetParameters: TRouteParameters;
    function GetAddresses: TOrderedAddressArray;
  end;

  TAddOrderToRouteParameterProvider = class(TInterfacedObject, IAddOrderToRouteParameterProvider)
  public
    function GetAddresses: TOrderedAddressArray;
    function GetParameters: TRouteParameters;
  end;

implementation

{ TAddOrderToRouteParameterProvider }

uses EnumsUnit;

function TAddOrderToRouteParameterProvider.GetAddresses: TOrderedAddressArray;
var
  Address: TOrderedAddress;
begin
  SetLength(Result, 2);

  Address := TOrderedAddress.Create;
  Address.AddressString := '325 Broadway, New York, NY 10007, USA';
  Address.Alias := 'BK Restaurant #: 20333';
  Address.Latitude := 40.71615;
  Address.Longitude := -74.00505;
  Address.CurbsideLatitude := 40.71615;
  Address.CurbsideLongitude := -74.00505;
  Address.Phone := '(212) 227-7535';
  Address.OrderId := 7205704;
  Result[0] := Address;

  Address := TOrderedAddress.Create;
  Address.AddressString := '106 Fulton St, Farmingdale, NY 11735, USA';
  Address.Alias := 'BK Restaurant #: 17871';
  Address.Latitude := 40.73073;
  Address.Longitude := -73.459283;
  Address.CurbsideLatitude := 40.73073;
  Address.CurbsideLongitude := -73.459283;
  Address.Phone := '(212) 566-5132';
  Address.OrderId := 7205703;
  Result[1] := Address;
end;

function TAddOrderToRouteParameterProvider.GetParameters: TRouteParameters;
begin
  Result := TRouteParameters.Create;
  Result.RouteName := 'Wednesday 15th of June 2016 07:01 PM (+03:00)';
  Result.RouteDate := 1465948800;
  Result.RouteTime := 14400;
  Result.Optimize := TOptimize.Time;
  Result.RouteType := 'single';
  Result.AlgorithmType := TAlgorithmType.TSP;
  Result.RT := False;
  Result.LockLast := False;
  Result.MemberId := '1116';
  Result.DisableOptimization := False;
end;

end.
