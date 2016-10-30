﻿unit MultipleDepotMultipleDriverTimeWindowTestDataProviderUnit;

interface

uses
  SysUtils,
  BaseOptimizationParametersProviderUnit, AddressUnit, RouteParametersUnit;

type
  TMultipleDepotMultipleDriverTimeWindowTestDataProvider = class(TBaseOptimizationParametersProvider)
  protected
    function MakeAddresses(): TArray<TAddress>; override;
    function MakeRouteParameters(): TRouteParameters; override;
  public

  end;

implementation

{ TMultipleDepotMultipleDriverTimeWindowTestDataProvider }

uses
  DateUtils,
  EnumsUnit, UtilsUnit;

function TMultipleDepotMultipleDriverTimeWindowTestDataProvider.MakeAddresses: TArray<TAddress>;
var
  FirstAddress: TAddress;
begin
  Result := TArray<TAddress>.Create();

  FirstAddress := TAddress.Create(
    '455 S 4th St, Louisville, KY 40202', 38.251698, -85.757308, 300);
  //indicate that this is a departure stop
  // single depot routes can only have one departure depot
  FirstAddress.IsDepot := True;
  //together these two specify the time window of a destination
  //seconds offset relative to the route start time for the open availability of a destination
  FirstAddress.TimeWindowStart := 28800;
  //seconds offset relative to the route end time for the open availability of a destination
  FirstAddress.TimeWindowEnd := 30477;
  AddAddress(FirstAddress, Result);

  AddAddress(TAddress.Create('1604 PARKRIDGE PKWY, Louisville, KY, 40214', 38.141598, -85.793846, 300, 30477, 33406), Result);
  AddAddress(TAddress.Create('1407 א53MCCOY, Louisville, KY, 40215', 38.202496, -85.786514, 300, 33406, 36228), Result);
  AddAddress(TAddress.Create('4805 BELLEVUE AVE, Louisville, KY, 40215', 38.178844, -85.774864, 300, 36228, 37518), Result);
  AddAddress(TAddress.Create('730 CECIL AVENUE, Louisville, KY, 40211', 38.248684, -85.821121, 300, 37518, 39550), Result);
  AddAddress(TAddress.Create('650 SOUTH 29TH ST UNIT 315, Louisville, KY, 40211', 38.251923, -85.800034, 300, 39550, 41348), Result);
  AddAddress(TAddress.Create('4629 HILLSIDE DRIVE, Louisville, KY, 40216', 38.176067, -85.824638, 300, 41348, 42261), Result);
  AddAddress(TAddress.Create('4738 BELLEVUE AVE, Louisville, KY, 40215', 38.179806, -85.775558, 300, 42261, 45195), Result);
  AddAddress(TAddress.Create('318 SO. 39TH STREET, Louisville, KY, 40212', 38.259335, -85.815094, 300, 45195, 46549), Result);
  AddAddress(TAddress.Create('1324 BLUEGRASS AVE, Louisville, KY, 40215', 38.179253, -85.785118, 300, 46549, 47353), Result);
  AddAddress(TAddress.Create('7305 ROYAL WOODS DR, Louisville, KY, 40214', 38.162472, -85.792854, 300, 47353, 50924), Result);
  AddAddress(TAddress.Create('1661 W HILL ST, Louisville, KY, 40210', 38.229584, -85.783966, 300, 50924, 51392), Result);
  AddAddress(TAddress.Create('3222 KINGSWOOD WAY, Louisville, KY, 40216', 38.210606, -85.822594, 300, 51392, 52451), Result);
  AddAddress(TAddress.Create('1922 PALATKA RD, Louisville, KY, 40214', 38.153767, -85.796783, 300, 52451, 55631), Result);
  AddAddress(TAddress.Create('1314 SOUTH 26TH STREET, Louisville, KY, 40210', 38.235847, -85.796852, 300, 55631, 58516), Result);
  AddAddress(TAddress.Create('2135 MCCLOSKEY AVENUE, Louisville, KY, 40210', 38.218662, -85.789032, 300, 58516, 61080), Result);
  AddAddress(TAddress.Create('1409 PHYLLIS AVE, Louisville, KY, 40215', 38.206154, -85.781387, 100, 61080, 61504), Result);
  AddAddress(TAddress.Create('4504 SUNFLOWER AVE, Louisville, KY, 40216', 38.187511, -85.839149, 300, 61504, 62061), Result);
  AddAddress(TAddress.Create('2512 GREENWOOD AVE, Louisville, KY, 40210', 38.241405, -85.795059, 300, 62061, 65012), Result);
  AddAddress(TAddress.Create('5500 WILKE FARM AVE, Louisville, KY, 40216', 38.166065, -85.863319, 300, 65012, 67541), Result);
  AddAddress(TAddress.Create('3640 LENTZ AVE, Louisville, KY, 40215', 38.193283, -85.786201, 300, 67541, 69120), Result);
  AddAddress(TAddress.Create('1020 BLUEGRASS AVE, Louisville, KY, 40215', 38.17952, -85.780037, 300, 69120, 70572), Result);
  AddAddress(TAddress.Create('123 NORTH 40TH ST, Louisville, KY, 40212', 38.26498, -85.814156, 300, 70572, 73177), Result);
  AddAddress(TAddress.Create('7315 ST ANDREWS WOODS CIRCLE UNIT 104, Louisville, KY, 40214', 38.151072, -85.802867, 300, 73177, 75231), Result);
  AddAddress(TAddress.Create('3210 POPLAR VIEW DR, Louisville, KY, 40216', 38.182594, -85.849937, 300, 75231, 77663), Result);
  AddAddress(TAddress.Create('4519 LOUANE WAY, Louisville, KY, 40216', 38.1754, -85.811447, 300, 77663, 79796), Result);
  AddAddress(TAddress.Create('6812 MANSLICK RD, Louisville, KY, 40214', 38.161839, -85.798279, 300, 79796, 80813), Result);
  AddAddress(TAddress.Create('1524 HUNTOON AVENUE, Louisville, KY, 40215', 38.172031, -85.788353, 300, 80813, 83956), Result);
  AddAddress(TAddress.Create('1307 LARCHMONT AVE, Louisville, KY, 40215', 38.209663, -85.779816, 300, 83956, 84365), Result);
  AddAddress(TAddress.Create('434 N 26TH STREET #2, Louisville, KY, 40212', 38.26844, -85.791962, 300, 84365, 85367), Result);
  AddAddress(TAddress.Create('678 WESTLAWN ST, Louisville, KY, 40211', 38.250397, -85.80629, 300, 85367, 86400), Result);
  AddAddress(TAddress.Create('2308 W BROADWAY, Louisville, KY, 40211', 38.248882, -85.790421, 300, 86400, 88703), Result);
  AddAddress(TAddress.Create('2332 WOODLAND AVE, Louisville, KY, 40210', 38.233579, -85.794257, 300, 88703, 89320), Result);
  AddAddress(TAddress.Create('1706 WEST ST. CATHERINE, Louisville, KY, 40210', 38.239697, -85.783928, 300, 89320, 90054), Result);
  AddAddress(TAddress.Create('1699 WATHEN LN, Louisville, KY, 40216', 38.216465, -85.792397, 300, 90054, 91150), Result);
  AddAddress(TAddress.Create('2416 SUNSHINE WAY, Louisville, KY, 40216', 38.186245, -85.831787, 300, 91150, 91915), Result);
  AddAddress(TAddress.Create('6925 MANSLICK RD, Louisville, KY, 40214', 38.158466, -85.798355, 300, 91915, 93407), Result);
  AddAddress(TAddress.Create('2707 7TH ST, Louisville, KY, 40215', 38.212438, -85.785082, 300, 93407, 95992), Result);
  AddAddress(TAddress.Create('2014 KENDALL LN, Louisville, KY, 40216', 38.179394, -85.826668, 300, 95992, 99307), Result);
  AddAddress(TAddress.Create('612 N 39TH ST, Louisville, KY, 40212', 38.273354, -85.812012, 300, 99307, 102906), Result);
  AddAddress(TAddress.Create('2215 ROWAN ST, Louisville, KY, 40212', 38.261703, -85.786781, 300, 102906, 106021), Result);
  AddAddress(TAddress.Create('1826 W. KENTUCKY ST, Louisville, KY, 40210', 38.241611, -85.78653, 300, 106021, 107276), Result);
  AddAddress(TAddress.Create('1810 GREGG AVE, Louisville, KY, 40210', 38.224716, -85.796211, 300, 107276, 107948), Result);
  AddAddress(TAddress.Create('4103 BURRRELL DRIVE, Louisville, KY, 40216', 38.191753, -85.825836, 300, 107948, 108414), Result);
  AddAddress(TAddress.Create('359 SOUTHWESTERN PKWY, Louisville, KY, 40212', 38.259903, -85.823463, 200, 108414, 108685), Result);
  AddAddress(TAddress.Create('2407 W CHESTNUT ST, Louisville, KY, 40211', 38.252781, -85.792109, 300, 108685, 110109), Result);
  AddAddress(TAddress.Create('225 S 22ND ST, Louisville, KY, 40212', 38.257616, -85.786658, 300, 110109, 111375), Result);
  AddAddress(TAddress.Create('1404 MCCOY AVE, Louisville, KY, 40215', 38.202122, -85.786072, 300, 111375, 112120), Result);
  AddAddress(TAddress.Create('117 FOUNT LANDING CT, Louisville, KY, 40212', 38.270061, -85.799438, 300, 112120, 114095), Result);
  AddAddress(TAddress.Create('5504 SHOREWOOD DRIVE, Louisville, KY, 40214', 38.145851, -85.7798, 300, 114095, 115743), Result);
  AddAddress(TAddress.Create('1406 CENTRAL AVE, Louisville, KY, 40208', 38.211025, -85.780251, 300, 115743, 117716), Result);
  AddAddress(TAddress.Create('901 W WHITNEY AVE, Louisville, KY, 40215', 38.194115, -85.77494, 300, 117716, 119078), Result);
  AddAddress(TAddress.Create('2109 SCHAFFNER AVE, Louisville, KY, 40210', 38.219699, -85.779363, 300, 119078, 121147), Result);
  AddAddress(TAddress.Create('2906 DIXIE HWY, Louisville, KY, 40216', 38.209278, -85.798653, 300, 121147, 124281), Result);
  AddAddress(TAddress.Create('814 WWHITNEY AVE, Louisville, KY, 40215', 38.193596, -85.773521, 300, 124281, 124675), Result);
  AddAddress(TAddress.Create('1610 ALGONQUIN PWKY, Louisville, KY, 40210', 38.222153, -85.784187, 300, 124675, 127148), Result);
  AddAddress(TAddress.Create('3524 WHEELER AVE, Louisville, KY, 40215', 38.195293, -85.788643, 300, 127148, 130667), Result);
  AddAddress(TAddress.Create('5009 NEW CUT RD, Louisville, KY, 40214', 38.165905, -85.779701, 300, 130667, 131980), Result);
  AddAddress(TAddress.Create('3122 ELLIOTT AVE, Louisville, KY, 40211', 38.251213, -85.804199, 300, 131980, 134402), Result);
  AddAddress(TAddress.Create('911 GAGEL AVE, Louisville, KY, 40216', 38.173512, -85.807854, 300, 134402, 136787), Result);
  AddAddress(TAddress.Create('4020 GARLAND AVE #lOOA, Louisville, KY, 40211', 38.246181, -85.818901, 300, 136787, 138073), Result);
  AddAddress(TAddress.Create('5231 MT HOLYOKE DR, Louisville, KY, 40216', 38.169369, -85.85704, 300, 138073, 141407), Result);
  AddAddress(TAddress.Create('1339 28TH S #2, Louisville, KY, 40211', 38.235275, -85.800156, 300, 141407, 143561), Result);
  AddAddress(TAddress.Create('836 S 36TH ST, Louisville, KY, 40211', 38.24651, -85.811234, 300, 143561, 145941), Result);
  AddAddress(TAddress.Create('2132 DUNCAN STREET, Louisville, KY, 40212', 38.262135, -85.785172, 300, 145941, 148296), Result);
  AddAddress(TAddress.Create('3529 WHEELER AVE, Louisville, KY, 40215', 38.195057, -85.787949, 300, 148296, 150177), Result);
  AddAddress(TAddress.Create('2829 DE MEL #11, Louisville, KY, 40214', 38.171662, -85.807271, 300, 150177, 150981), Result);
  AddAddress(TAddress.Create('1325 EARL AVENUE, Louisville, KY, 40215', 38.204556, -85.781555, 300, 150981, 151854), Result);
  AddAddress(TAddress.Create('3632 MANSLICK RD #10, Louisville, KY, 40215', 38.193542, -85.801147, 300, 151854, 152613), Result);
  AddAddress(TAddress.Create('637 S 41ST ST, Louisville, KY, 40211', 38.253632, -85.81897, 300, 152613, 156131), Result);
  AddAddress(TAddress.Create('3420 VIRGINIA AVENUE, Louisville, KY, 40211', 38.238693, -85.811386, 300, 156131, 157212), Result);
  AddAddress(TAddress.Create('3501 MALIBU CT APT 6, Louisville, KY, 40216', 38.166481, -85.825928, 300, 157212, 158655), Result);
  AddAddress(TAddress.Create('4912 DIXIE HWY, Louisville, KY, 40216', 38.170728, -85.826817, 300, 158655, 159145), Result);
  AddAddress(TAddress.Create('7720 DINGLEDELL RD, Louisville, KY, 40214', 38.162472, -85.792854, 300, 159145, 161831), Result);
  AddAddress(TAddress.Create('2123 RATCLIFFE AVE, Louisville, KY, 40210', 38.21978, -85.797615, 300, 161831, 163705), Result);
  AddAddress(TAddress.Create('1321 OAKWOOD AVE, Louisville, KY, 40215', 38.17704, -85.783829, 300, 163705, 164953), Result);
  AddAddress(TAddress.Create('2223 WEST KENTUCKY STREET, Louisville, KY, 40210', 38.242516, -85.790695, 300, 164953, 166189), Result);
  AddAddress(TAddress.Create('8025 GLIMMER WAY #3308, Louisville, KY, 40214', 38.131981, -85.77935, 300, 166189, 166640), Result);
  AddAddress(TAddress.Create('1155 S 28TH ST, Louisville, KY, 40211', 38.238621, -85.799911, 300, 166640, 168147), Result);
  AddAddress(TAddress.Create('840 IROQUOIS AVE, Louisville, KY, 40214', 38.166355, -85.779396, 300, 168147, 170385), Result);
  AddAddress(TAddress.Create('5573 BRUCE AVE, Louisville, KY, 40214', 38.145222, -85.779205, 300, 170385, 171096), Result);
  AddAddress(TAddress.Create('1727 GALLAGHER, Louisville, KY, 40210', 38.239334, -85.784882, 300, 171096, 171951), Result);
  AddAddress(TAddress.Create('1309 CATALPA ST APT 204, Louisville, KY, 40211', 38.236524, -85.801619, 300, 171951, 172393), Result);
  AddAddress(TAddress.Create('1330 ALGONQUIN PKWY, Louisville, KY, 40208', 38.219846, -85.777344, 300, 172393, 175337), Result);
  AddAddress(TAddress.Create('823 SUTCLIFFE, Louisville, KY, 40211', 38.246956, -85.811569, 300, 175337, 176867), Result);
  AddAddress(TAddress.Create('4405 CHURCHMAN AVENUE #2, Louisville, KY, 40215', 38.177768, -85.792545, 300, 176867, 178051), Result);
  AddAddress(TAddress.Create('3211 DUMESNIL ST #1, Louisville, KY, 40211', 38.237789, -85.807968, 300, 178051, 179083), Result);
  AddAddress(TAddress.Create('3904 WEWOKA AVE, Louisville, KY, 40212', 38.270367, -85.813118, 300, 179083, 181543), Result);
  AddAddress(TAddress.Create('660 SO. 42ND STREET, Louisville, KY, 40211', 38.252865, -85.822624, 300, 181543, 184193), Result);
  AddAddress(TAddress.Create('3619  LENTZ  AVE, Louisville, KY, 40215', 38.193249, -85.785492, 300, 184193, 185853), Result);
  AddAddress(TAddress.Create('4305  STOLTZ  CT, Louisville, KY, 40215', 38.178707, -85.787292, 300, 185853, 187252), Result);
end;

function TMultipleDepotMultipleDriverTimeWindowTestDataProvider.MakeRouteParameters: TRouteParameters;
begin
  Result := TRouteParameters.Create();
  // specify capacitated vehicle routing with time windows and multiple depots, with multiple drivers
  Result.AlgorithmType := TAlgorithmType.CVRP_TW_MD;
  Result.StoreRoute := False;

  // set an arbitrary route name
  // this value shows up in the website, and all the connected mobile device
  Result.RouteName := 'Multiple Depot, Multiple Driver, Time Window';
  // the route start date in UTC, unix timestamp seconds (Tomorrow)
  Result.RouteDate := 53583232; //TUtils.ConvertToUnixTimestamp(IncDay(Now, 1));
  // the time in UTC when a route is starting (7AM)
  Result.RouteTime := 60 * 60 * 7;
  // the maximum duration of a route
  Result.RouteMaxDuration := 86400 * 3;
  Result.RT := True;
  Result.VehicleCapacity := '99';
  Result.VehicleMaxDistanceMI := '99999';
  Result.Optimize := TOptimize.Time;
  Result.DistanceUnit := TDistanceUnit.MI;
  Result.DeviceType := TDeviceType.Web;
  Result.TravelMode := TTravelMode.Driving;
  Result.Metric := TMetric.Geodesic;
end;

end.
