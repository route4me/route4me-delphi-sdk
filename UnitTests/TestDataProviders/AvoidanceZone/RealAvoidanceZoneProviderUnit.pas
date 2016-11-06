unit RealAvoidanceZoneProviderUnit;

interface

uses
  SysUtils,
  AvoidanceZoneUnit, IAvoidanceZoneProviderUnit;

type
  TRealAvoidanceZoneProvider = class(TInterfacedObject, IAvoidanceZoneProvider)
  protected
  public
    function AvoidanceZone: TAvoidanceZone;
  end;

implementation

uses
  DateUtils,
  EnumsUnit, UtilsUnit, JSONDictionaryIntermediateObjectUnit,
  RouteParametersUnit, LinksUnit, AddressUnit;

function TRealAvoidanceZoneProvider.AvoidanceZone: TAvoidanceZone;
{var
  Parameters: TRouteParameters;
  Route: TDataObjectRoute;
  Address1, Address2, Address3, Address4, Address5, Address6, Address7, Address8, Address9, Address10: TAddress;}
begin
  Result := nil;

{  Parameters := TRouteParameters.Create;
  Parameters.IsUpload := False;
  Parameters.RT := False;
  Parameters.RouteName := 'Single Driver Route 10 Stops';
  Parameters.RouteDate := 1477872000;
  Parameters.RouteTime := 25200;
  Parameters.DisableOptimization := False;
  Parameters.Optimize := TOptimize.Distance;
  Parameters.LockLast := False;
  Parameters.DistanceUnit := TDistanceUnit.MI;
  Parameters.TravelMode := TTravelMode.Driving;
  Parameters.Avoid := TAvoid.Empty;
  Parameters.RouteMaxDuration := 86399;
  Parameters.StoreRoute := True;
  Parameters.Metric := TMetric.Matrix;
  Parameters.AlgorithmType := TAlgorithmType.TSP;
  Parameters.MemberId := '1';
  Parameters.Ip := '2998453096';
  Parameters.DM := 1;
  Parameters.Dirm := 5;
  Parameters.Parts := 10;
  Parameters.DeviceType := TDeviceType.Web;
  Parameters.HasTrailer := False;
  Parameters.OptimizationQuality := 1;
  Parameters.MinTourSize := 0;

  Address1 := TAddress.Create;
  Address1.RouteDestinationId := 195054701;
  Address1.Alias := '';
  Address1.MemberId := '1';
  Address1.AddressString := '151 Arbor Way Milledgeville GA 31061';
  Address1.IsDepot := True;
  Address1.Latitude := 33.132675170898;
  Address1.Longitude := -83.244743347168;
  Address1.CurbsideLatitude := 33.1326752;
  Address1.CurbsideLongitude := -83.2447433;
  Address1.OptimizationProblemId := '85DA4196C5F5EC5EF6779DA8E83E6AEC';
  Address1.SequenceNo := 0;
  Address1.Time := 0;
  Address1.AddCustomField('color', 'red');
  Address1.AddCustomField('size', 'huge');

  Route := TDataObjectRoute.Create;
  Route.RouteId := 'E8266C1041A28310B1068070EF050158';
  Route.OptimizationProblemId := '85DA4196C5F5EC5EF6779DA8E83E6AEC';
  Route.MemberId := '1';
  Route.MemberEmail := 'dan@novapulsar.com';
  Route.VehicleAlias := 'N/A';
  Route.DriverAlias := 'N/A';
  Route.TripDistance := 9.82;
  Route.RouteCost := 0;
  Route.RouteRevenue := 0;
  Route.NetRevenuePerDistanceUnit := 0;
  Route.CreatedTimestamp := 1477793078;
  Route.Mpg := '10';
  Route.GasPrice := 2;
  Route.RouteDurationSec := 1772;
  Route.Parameters := Parameters;
  Route.AddAddress(Address1);
  Route.AddAddress(Address2);
  Route.AddAddress(Address3);
  Route.AddAddress(Address4);
  Route.AddAddress(Address5);
  Route.AddAddress(Address6);
  Route.AddAddress(Address7);
  Route.AddAddress(Address8);
  Route.AddAddress(Address9);
  Route.AddAddress(Address10);
  Route.Links := TLinks.Create;
  Route.Links.Route := 'http://www.route4me.com/api.v4/route.php?route_id=E8266C1041A28310B1068070EF050158&api_key=11111111111111111111111111111111&member_id=1&device_tracking_history=0&original=0&notes=0';
  Route.Links.OptimizationProblemId := 'http://www.route4me.com/api.v4/optimization_problem.php?optimization_problem_id=85DA4196C5F5EC5EF6779DA8E83E6AEC&api_key=11111111111111111111111111111111&member_id=1';

  Result := TDataObject.Create;
  Result.OptimizationProblemId := '85DA4196C5F5EC5EF6779DA8E83E6AEC';
  Result.State := TOptimizationState.Optimized;
//  Result.CreatedTimestamp := 1477793076;
  Result.Parameters := Parameters;
  Result.AddAddress(Address1);
  Result.AddAddress(Address2);
  Result.AddAddress(Address3);
  Result.AddAddress(Address4);
  Result.AddAddress(Address5);
  Result.AddAddress(Address6);
  Result.AddAddress(Address7);
  Result.AddAddress(Address8);
  Result.AddAddress(Address9);
  Result.AddAddress(Address10);
  Result.AddRoute(Route);
  Result.Links := TLinks.Create;
  Result.Links.View := 'http://www.route4me.com/api.v4/optimization_problem.php?optimization_problem_id=85DA4196C5F5EC5EF6779DA8E83E6AEC&api_key=11111111111111111111111111111111&member_id=1';}
end;

end.
