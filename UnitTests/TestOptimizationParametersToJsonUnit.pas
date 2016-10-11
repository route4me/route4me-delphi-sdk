unit TestOptimizationParametersToJsonUnit;

interface

uses
  REST.Json.Types, REST.JsonReflect,
  JSONNullableAttributeUnit,
  TestFramework, System.Generics.Collections, Classes,
  AddressUnit, RouteParametersUnit,
  OptimizationParametersUnit, GenericParametersUnit,
  NullableBasicTypesUnit;

type
  TTestOptimizationParametersToJson = class(TTestCase)
  private
    procedure SaveTestDataToFile(s: String);

    procedure CheckEquals(EtalonFilename: String; Actual: String);
    function EtalonFilename(TestName: String): String;

    function EtalonSingleDriverRoute10Stops(): String;
    function EtalonSingleDriverRoundTrip(): String;
    function EtalonSingleDepotMultipleDriverNoTimeWindow(): String;
    function EtalonMultipleDepotMultipleDriver(): String;
  published
    procedure MultipleDepotMultipleDriverOptimizationParametersToJson();
    procedure MultipleDepotMultipleDriverTimeWindowOptimizationParametersToJson();
    procedure MultipleDepotMultipleDriverWith24StopsTimeWindowOptimizationParametersToJson();

    procedure SingleDepotMultipleDriverNoTimeWindowOptimizationParametersToJson();
    procedure SingleDriverMultipleTimeWindowsOptimizationParametersToJson();
    procedure SingleDriverRoundTripOptimizationParametersToJson();
    procedure SingleDriverRoute10StopsOptimizationParametersToJson();
  end;

implementation

{ TTestOptimizationParametersToJson }

uses
  EnumsUnit, UtilsUnit, SingleDriverRoute10StopsTestDataProviderUnit,
  SingleDriverRoundTripTestDataProviderUnit,
  MultipleDepotMultipleDriverTestDataProviderUnit,
  SingleDepotMultipleDriverNoTimeWindowTestDataProviderUnit,
  IOptimizationParametersProviderUnit,
  SingleDriverMultipleTimeWindowsTestDataProviderUnit,
  MultipleDepotMultipleDriverTimeWindowTestDataProviderUnit,
  MultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProviderUnit;

procedure TTestOptimizationParametersToJson.CheckEquals(EtalonFilename,
  Actual: String);
var
  EtalonList: TStringList;
  ActualList: TStringList;
begin
  EtalonList := TStringList.Create;
  ActualList := TStringList.Create;
  try
    EtalonList.LoadFromFile(EtalonFilename);
    ActualList.Text := Actual;
    CheckTrue(EtalonList.Equals(ActualList));
  finally
    ActualList.Free;
    EtalonList.Free;
  end;
end;

function TTestOptimizationParametersToJson.EtalonFilename(
  TestName: String): String;
begin
  Result := '..\..\Etalons\OptimizationParametersToJson\' + TestName + '.json';
end;

function TTestOptimizationParametersToJson.EtalonMultipleDepotMultipleDriver: String;
begin
  Result := '{"addresses":[{"address":"3634 W Market St, Fairlawn, OH 44333","lat":41.135762259364,"lng":-81.629313826561,"is_depot":true,"time":300,"curbside_lat":null,"curbside_lng":null,"time_window_start":28800,"time_window_end":29465},' +
    '{"address":"1218 Ruth Ave, Cuyahoga Falls, OH 44221","lat":41.135762259364,"lng":-81.629313826561,"time":300,"curbside_lat":null,"curbside_lng":null,"time_window_start":29465,"time_window_end":30529},' +
    '{"address":"512 Florida Pl, Barberton, OH 44203","lat":41.003671512008,"lng":-81.598461046815,"time":300,"curbside_lat":null,"curbside_lng":null,"time_window_start":30529,"time_window_end":33779},' +
    '{"address":"512 Florida Pl, Barberton, OH 44203","lat":41.003671512008,"lng":-81.598461046815,"time":100,"curbside_lat":null,"curbside_lng":null,"time_window_start":33779,"time_window_end":33944},' +
    '{"address":"3495 Purdue St, Cuyahoga Falls, OH 44221","lat":41.162971496582,"lng":-81.479049682617,"time":300,"curbside_lat":null,"curbside_lng":null,"time_window_start":33944,"time_window_end":34801},' +
    '{"address":"1659 Hibbard Dr, Stow, OH 44224","lat":41.194505989552,"lng":-81.443351581693,"time":300,"curbside_lat":null,"curbside_lng":null,"time_window_start":34801,"time_window_end":36366},' +
    '{"address":"2705 N River Rd, Stow, OH 44224","lat":41.145240783691,"lng":-81.410247802734,"time":300,"curbside_lat":null,"curbside_lng":null,"time_window_start":36366,"time_window_end":39173},' +
    '{"address":"10159 Bissell Dr, Twinsburg, OH 44087","lat":41.340042114258,"lng":-81.421226501465,"time":300,"curbside_lat":null,"curbside_lng":null,"time_window_start":39173,"time_window_end":41617},' +
    '{"address":"367 Cathy Dr, Munroe Falls, OH 44262","lat":41.148578643799,"lng":-81.429229736328,"time":300,"curbside_lat":null,"curbside_lng":null,"time_window_start":41617,"time_window_end":43660},' +
    '{"address":"367 Cathy Dr, Munroe Falls, OH 44262","lat":41.148578643799,"lng":-81.429229736328,"time":300,"curbside_lat":null,"curbside_lng":null,"time_window_start":43660,"time_window_end":46392},' +
    '{"address":"512 Florida Pl, Barberton, OH 44203","lat":41.003671512008,"lng":-81.598461046815,"time":300,"curbside_lat":null,"curbside_lng":null,"time_window_start":46392,"time_window_end":48389},' +
    '{"address":"559 W Aurora Rd, Northfield, OH 44067","lat":41.315116882324,"lng":-81.558746337891,"time":50,"curbside_lat":null,"curbside_lng":null,"time_window_start":48389,"time_window_end":48449},' +
    '{"address":"3933 Klein Ave, Stow, OH 44224","lat":41.169467926025,"lng":-81.429420471191,"time":300,"curbside_lat":null,"curbside_lng":null,"time_window_start":48449,"time_window_end":50152},' +
    '{"address":"2148 8th St, Cuyahoga Falls, OH 44221","lat":41.136692047119,"lng":-81.493492126465,"time":300,"curbside_lat":null,"curbside_lng":null,"time_window_start":50152,"time_window_end":51982},' +
    '{"address":"3731 Osage St, Stow, OH 44224","lat":41.161357879639,"lng":-81.42293548584,"time":100,"curbside_lat":null,"curbside_lng":null,"time_window_start":51982,"time_window_end":52180},' +
    '{"address":"3731 Osage St, Stow, OH 44224","lat":41.161357879639,"lng":-81.42293548584,"time":300,"curbside_lat":null,"curbside_lng":null,"time_window_start":52180,"time_window_end":54379}],' +
    '"parameters":{"route_name":"Multiple Depot, Multiple Driver","algorithm_type":4,"route_date":53583232,"route_time":25200,' +
      '"optimize":"Distance","distance_unit":"mi","device_type":"web","route_max_duration":86400,"vehicle_capacity":"1","vehicle_max_distance_mi":"10000","travel_mode":"Driving","metric":3}}';
end;

function TTestOptimizationParametersToJson.EtalonSingleDepotMultipleDriverNoTimeWindow: String;
var
  st: TStringList;
begin
  st := TStringList.Create;
  try
    st.LoadFromFile('..\..\Etalons\EtalonSingleDepotMultipleDriverNoTimeWindow.txt');
    Result := st.Text;
  finally
    st.Free;
  end;
end;

function TTestOptimizationParametersToJson.EtalonSingleDriverRoundTrip: String;
begin
  Result := '{"addresses":[{"address":"754 5th Ave New York, NY 10019","lat":40.7636197,"lng":-73.9744388,"alias":"Bergdorf Goodman","is_depot":true,"time":0,"curbside_lat":null,"curbside_lng":null},' +
    '{"address":"717 5th Ave New York, NY 10022","lat":40.7669692,"lng":-73.9693864,"alias":"Giorgio Armani","time":0,"curbside_lat":null,"curbside_lng":null},' +
    '{"address":"888 Madison Ave New York, NY 10014","lat":40.7715154,"lng":-73.9669241,"alias":"Ralph Lauren Women''s and Home","time":0,"curbside_lat":null,"curbside_lng":null},' +
    '{"address":"1011 Madison Ave New York, NY 10075","lat":40.7772129,"lng":-73.9669,"alias":"Yigal Azrou''l","time":0,"curbside_lat":null,"curbside_lng":null},' +
    '{"address":"440 Columbus Ave New York, NY 10024","lat":40.7808364,"lng":-73.9732729,"alias":"Frank Stella Clothier","time":0,"curbside_lat":null,"curbside_lng":null},' +
    '{"address":"324 Columbus Ave #1 New York, NY 10023","lat":40.7803123,"lng":-73.9793079,"alias":"Liana","time":0,"curbside_lat":null,"curbside_lng":null},' +
    '{"address":"110 W End Ave New York, NY 10023","lat":40.7753077,"lng":-73.9861529,"alias":"Toga Bike Shop","time":0,"curbside_lat":null,"curbside_lng":null},' +
    '{"address":"555 W 57th St New York, NY 10019","lat":40.7718005,"lng":-73.9897716,"alias":"BMW of Manhattan","time":0,"curbside_lat":null,"curbside_lng":null},' +
    '{"address":"57 W 57th St New York, NY 10019","lat":40.7558695,"lng":-73.9862019,"alias":"Verizon Wireless","time":0,"curbside_lat":null,"curbside_lng":null}],' +
    '"parameters":{"route_name":"Single Driver Round Trip","algorithm_type":1,"store_route":false,"route_date":53583232,' +
      '"route_time":25200,"optimize":"Distance","distance_unit":"mi","device_type":"web","route_max_duration":86400,"vehicle_capacity":"1","vehicle_max_distance_mi":"10000","travel_mode":"Driving"}}';

end;

function TTestOptimizationParametersToJson.EtalonSingleDriverRoute10Stops: String;
begin
  Result := '{"addresses":[{"address":"151 Arbor Way Milledgeville GA 31061","lat":33.132675170898,"lng":-83.244743347168,"is_depot":true,"time":0,"curbside_lat":null,"curbside_lng":null,"custom_fields":{"color":"red","size":"huge"}},' +
    '{"address":"230 Arbor Way Milledgeville GA 31061","lat":33.129695892334,"lng":-83.24577331543,"time":0,"curbside_lat":null,"curbside_lng":null},' +
    '{"address":"148 Bass Rd NE Milledgeville GA 31061","lat":33.143497,"lng":-83.224487,"time":0,"curbside_lat":null,"curbside_lng":null},' +
    '{"address":"117 Bill Johnson Rd NE Milledgeville GA 31061","lat":33.141784667969,"lng":-83.237518310547,"time":0,"curbside_lat":null,"curbside_lng":null},' +
    '{"address":"119 Bill Johnson Rd NE Milledgeville GA 31061","lat":33.141086578369,"lng":-83.238258361816,"time":0,"curbside_lat":null,"curbside_lng":null},' +
    '{"address":"131 Bill Johnson Rd NE Milledgeville GA 31061","lat":33.142036437988,"lng":-83.238845825195,"time":0,"curbside_lat":null,"curbside_lng":null},' +
    '{"address":"138 Bill Johnson Rd NE Milledgeville GA 31061","lat":33.14307,"lng":-83.239334,"time":0,"curbside_lat":null,"curbside_lng":null},' +
    '{"address":"139 Bill Johnson Rd NE Milledgeville GA 31061","lat":33.142734527588,"lng":-83.237442016602,"time":0,"curbside_lat":null,"curbside_lng":null},' +
    '{"address":"145 Bill Johnson Rd NE Milledgeville GA 31061","lat":33.143871307373,"lng":-83.237342834473,"time":0,"curbside_lat":null,"curbside_lng":null},' +
    '{"address":"221 Blake Cir Milledgeville GA 31061","lat":33.081462860107,"lng":-83.208511352539,"time":0,"curbside_lat":null,"curbside_lng":null}],' +
    '"parameters":{"route_name":"Single Driver Route 10 Stops","algorithm_type":1,"store_route":false,"route_date":53583232,"route_time":25200,"optimize":"Distance","distance_unit":"mi","device_type":"web"}}';
end;

procedure TTestOptimizationParametersToJson.MultipleDepotMultipleDriverOptimizationParametersToJson();
var
  DataProvider: IOptimizationParametersProvider;
begin
  DataProvider := TMultipleDepotMultipleDriverTestDataProvider.Create;
  try
    CheckEquals(EtalonFilename('MultipleDepotMultipleDriver'), DataProvider.OptimizationParameters.ToJsonString);
  finally
    DataProvider := nil;
  end;
end;

procedure TTestOptimizationParametersToJson.MultipleDepotMultipleDriverTimeWindowOptimizationParametersToJson;
var
  DataProvider: IOptimizationParametersProvider;
begin
  DataProvider := TMultipleDepotMultipleDriverTimeWindowTestDataProvider.Create;
  try
    CheckEquals(EtalonFilename('MultipleDepotMultipleDriverTimeWindow'), DataProvider.OptimizationParameters.ToJsonString);
  finally
    DataProvider := nil;
  end;
end;

procedure TTestOptimizationParametersToJson.MultipleDepotMultipleDriverWith24StopsTimeWindowOptimizationParametersToJson;
var
  DataProvider: IOptimizationParametersProvider;
begin
  DataProvider := TMultipleDepotMultipleDriverWith24StopsTimeWindowTestDataProvider.Create;
  try
    CheckEquals(EtalonFilename('MultipleDepotMultipleDriverWith24StopsTimeWindow'), DataProvider.OptimizationParameters.ToJsonString);
  finally
    DataProvider := nil;
  end;
end;

procedure TTestOptimizationParametersToJson.SaveTestDataToFile(s: String);
var
  st: TStringList;
begin
  st := TStringList.Create;
  st.Text := s;
  st.SaveToFile('TestData.txt');
  st.Free;
end;

procedure TTestOptimizationParametersToJson.SingleDepotMultipleDriverNoTimeWindowOptimizationParametersToJson;
var
  DataProvider: IOptimizationParametersProvider;
begin
  DataProvider := TSingleDepotMultipleDriverNoTimeWindowTestDataProvider.Create;
  try
    CheckEquals(EtalonFilename('SingleDepotMultipleDriverNoTimeWindow'), DataProvider.OptimizationParameters.ToJsonString);
  finally
    DataProvider := nil;
  end;
end;

procedure TTestOptimizationParametersToJson.SingleDriverMultipleTimeWindowsOptimizationParametersToJson;
var
  DataProvider: IOptimizationParametersProvider;
begin
  DataProvider := TSingleDriverMultipleTimeWindowsTestDataProvider.Create;
  try
    CheckEquals(EtalonFilename('SingleDriverMultipleTimeWindows'), DataProvider.OptimizationParameters.ToJsonString);
  finally
    DataProvider := nil;
  end;
end;

procedure TTestOptimizationParametersToJson.SingleDriverRoundTripOptimizationParametersToJson;
var
  DataProvider: IOptimizationParametersProvider;
begin
  DataProvider := TSingleDriverRoundTripTestDataProvider.Create;
  try
    CheckEquals(EtalonFilename('SingleDriverRoundTrip'), DataProvider.OptimizationParameters.ToJsonString);
  finally
    DataProvider := nil;
  end;
end;

procedure TTestOptimizationParametersToJson.SingleDriverRoute10StopsOptimizationParametersToJson;
var
  DataProvider: IOptimizationParametersProvider;
begin
  DataProvider := TSingleDriverRoute10StopsTestDataProvider.Create;
  try
    CheckEquals(EtalonFilename('SingleDriverRoute10Stops'), DataProvider.OptimizationParameters.ToJsonString);
  finally
    DataProvider := nil;
  end;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('JSON\', TTestOptimizationParametersToJson.Suite);
end.
