unit SingleDriverRoute10StopsTestDataProviderUnit;

interface

uses
  SysUtils,
  BaseOptimizationParametersProviderUnit, AddressUnit, RouteParametersUnit;

type
  TSingleDriverRoute10StopsTestDataProvider = class(TBaseOptimizationParametersProvider)
  protected
    function MakeAddresses(): TArray<TAddress>; override;
    function MakeRouteParameters(): TRouteParameters; override;
  public

  end;
implementation

{ TSingleDriverRoute10StopsTestDataProvider }

uses
  DateUtils,
  EnumsUnit, UtilsUnit;

function TSingleDriverRoute10StopsTestDataProvider.MakeAddresses: TArray<TAddress>;
var
  FirstAddress: TAddress;
begin
  Result := TArray<TAddress>.Create();

  FirstAddress := TAddress.Create(
    '151 Arbor Way Milledgeville GA 31061', 33.132675170898, -83.244743347168, 0);
  //indicate that this is a departure stop
  // single depot routes can only have one departure depot
  FirstAddress.IsDepot := True;
  // input as many custom fields as needed, custom data is passed through to mobile devices and to the manifest
  FirstAddress.AddCustomField('color', 'red');
  FirstAddress.AddCustomField('size', 'huge');
  AddAddress(FirstAddress, Result);

  AddAddress(TAddress.Create(
    '230 Arbor Way Milledgeville GA 31061', 33.129695892334, -83.24577331543, 0),
    Result);
  AddAddress(TAddress.Create(
    '148 Bass Rd NE Milledgeville GA 31061', 33.143497, -83.224487, 0),
    Result);
  AddAddress(TAddress.Create(
    '117 Bill Johnson Rd NE Milledgeville GA 31061', 33.141784667969, -83.237518310547, 0),
    Result);
  AddAddress(TAddress.Create(
    '119 Bill Johnson Rd NE Milledgeville GA 31061', 33.141086578369, -83.238258361816, 0),
    Result);
  AddAddress(TAddress.Create(
    '131 Bill Johnson Rd NE Milledgeville GA 31061', 33.142036437988, -83.238845825195, 0),
    Result);
  AddAddress(TAddress.Create(
    '138 Bill Johnson Rd NE Milledgeville GA 31061', 33.14307, -83.239334, 0),
    Result);
  AddAddress(TAddress.Create(
    '139 Bill Johnson Rd NE Milledgeville GA 31061', 33.142734527588, -83.237442016602, 0),
    Result);
  AddAddress(TAddress.Create(
    '145 Bill Johnson Rd NE Milledgeville GA 31061', 33.143871307373, -83.237342834473, 0),
    Result);
  AddAddress(TAddress.Create(
    '221 Blake Cir Milledgeville GA 31061', 33.081462860107, -83.208511352539, 0),
    Result);
end;

function TSingleDriverRoute10StopsTestDataProvider.MakeRouteParameters: TRouteParameters;
begin
  Result := TRouteParameters.Create();
  Result.AlgorithmType := Integer(TAlgorithmType.TSP);
  Result.StoreRoute := False;
  Result.RouteName := 'Single Driver Route 10 Stops';
  Result.RouteDate := 53583232;//TUtils.ConvertToUnixTimestamp(IncDay(Now, 1));
  Result.RouteTime := 60 * 60 * 7;
  Result.Optimize := TOptimizeDescription[Distance];
  Result.DistanceUnit := TDistanceUnitDescription[MI];
  Result.DeviceType := TDeviceTypeDescription[Web];
end;

end.
