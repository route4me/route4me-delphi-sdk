unit SingleDriverRoute10StopsTestDataProviderUnit;

interface

uses
  SysUtils,
  BaseOptimizationParametersProviderUnit, AddressUnit, RouteParametersUnit,
  OptimizationParametersUnit;

type
  TSingleDriverRoute10StopsTestDataProvider = class(TBaseOptimizationParametersProvider)
  protected
    function MakeAddresses(): TAddressesArray; override;
    function MakeRouteParameters(): TRouteParameters; override;
    procedure CorrectForResponse(OptimizationParameters: TOptimizationParameters); override;
  public

  end;
implementation

{ TSingleDriverRoute10StopsTestDataProvider }

uses
  DateUtils,
  EnumsUnit, UtilsUnit, JSONDictionaryIntermediateObjectUnit;

type
  TAddressTag = record
    AddressString: String;
    Latitude, Longitude: double;

    constructor Create(AddressString: String; Latitude, Longitude: double);

    function Equals(Address: TAddress): boolean;
  end;

var
  Addresses: TArray<TAddressTag>;

function GetAddressIndexByTag(Tag: TAddressTag; Addresses: TAddressesArray): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Length(Addresses) - 1 do
    if Tag.Equals(Addresses[i]) then
      Exit(i);
end;

procedure TSingleDriverRoute10StopsTestDataProvider.CorrectForResponse(
  OptimizationParameters: TOptimizationParameters);
  function Index(index: integer): integer;
  begin
    Result := GetAddressIndexByTag(Addresses[index], OptimizationParameters.Addresses);
  end;
var
  RouteParameters: TRouteParameters;
  i: integer;
begin
  inherited;

  RouteParameters := OptimizationParameters.Parameters.Value as TRouteParameters;
  RouteParameters.IsUpload := False;
  RouteParameters.RT := False;
  RouteParameters.DisableOptimization := False;
  RouteParameters.StoreRoute := True;
  RouteParameters.RouteDate := 0;
  RouteParameters.RouteMaxDuration := 86399;
  RouteParameters.TravelMode := TTravelMode.Driving;
  RouteParameters.Metric := TMetric.Matrix;
  RouteParameters.Parts := 10;
  RouteParameters.Dirm := 5;
  RouteParameters.DM := 1;
  RouteParameters.MemberId := '1';
  RouteParameters.LockLast := False;
  RouteParameters.Avoid := TAvoid.Empty;
  RouteParameters.HasTrailer := False;
  RouteParameters.MinTourSize := 0;
  RouteParameters.OptimizationQuality := 1;
  RouteParameters.Ip := '2998453096';

  OptimizationParameters.Addresses[Index(0)].RouteDestinationId := 193721035;
  OptimizationParameters.Addresses[Index(0)].MemberId := 1;
  OptimizationParameters.Addresses[Index(0)].CurbsideLatitude := 33.1326752;
  OptimizationParameters.Addresses[Index(0)].CurbsideLongitude := -83.2447433;
  OptimizationParameters.Addresses[Index(0)].SequenceNo := 0;
  OptimizationParameters.Addresses[Index(0)].DriveTimeToNextDestination := 48;
  OptimizationParameters.Addresses[Index(0)].DistanceToNextDestination := 0.21;

  OptimizationParameters.Addresses[Index(1)].RouteDestinationId := 193721036;
  OptimizationParameters.Addresses[Index(1)].MemberId := 1;
  OptimizationParameters.Addresses[Index(1)].CurbsideLatitude := 33.1296959;
  OptimizationParameters.Addresses[Index(1)].CurbsideLongitude := -83.2457733;
  OptimizationParameters.Addresses[Index(1)].SequenceNo := 1;
  OptimizationParameters.Addresses[Index(1)].IsDepot := False;
  OptimizationParameters.Addresses[Index(1)].RouteId := 'FC3D90EDECBAA80F46C37EAFBE4ADB36';
  OptimizationParameters.Addresses[Index(1)].DriveTimeToNextDestination := 312;
  OptimizationParameters.Addresses[Index(1)].DistanceToNextDestination := 1.4;
  OptimizationParameters.Addresses[Index(1)].TrackingNumber := 'NR173QRE';
  OptimizationParameters.Addresses[Index(1)].CustomFields := TDictionaryStringIntermediateObject.Create;

  OptimizationParameters.Addresses[Index(2)].RouteDestinationId := 193721037;
  OptimizationParameters.Addresses[Index(2)].MemberId := 1;
  OptimizationParameters.Addresses[Index(2)].CurbsideLatitude := 33.143497;
  OptimizationParameters.Addresses[Index(2)].CurbsideLongitude := -83.224487;
  OptimizationParameters.Addresses[Index(2)].SequenceNo := 8;
  OptimizationParameters.Addresses[Index(2)].IsDepot := False;
  OptimizationParameters.Addresses[Index(2)].RouteId := 'FC3D90EDECBAA80F46C37EAFBE4ADB36';
  OptimizationParameters.Addresses[Index(2)].DriveTimeToNextDestination := 921;
  OptimizationParameters.Addresses[Index(2)].DistanceToNextDestination := 5.86;
  OptimizationParameters.Addresses[Index(2)].TrackingNumber := 'X70P9K70';
  OptimizationParameters.Addresses[Index(2)].CustomFields := TDictionaryStringIntermediateObject.Create;

  OptimizationParameters.Addresses[Index(3)].RouteDestinationId := 193721038;
  OptimizationParameters.Addresses[Index(3)].MemberId := 1;
  OptimizationParameters.Addresses[Index(3)].CurbsideLatitude := 33.1417847;
  OptimizationParameters.Addresses[Index(3)].CurbsideLongitude := -83.2375183;
  OptimizationParameters.Addresses[Index(3)].SequenceNo := 3;
  OptimizationParameters.Addresses[Index(3)].IsDepot := False;
  OptimizationParameters.Addresses[Index(3)].RouteId := 'FC3D90EDECBAA80F46C37EAFBE4ADB36';
  OptimizationParameters.Addresses[Index(3)].DriveTimeToNextDestination := 29;
  OptimizationParameters.Addresses[Index(3)].DistanceToNextDestination := 0.13;
  OptimizationParameters.Addresses[Index(3)].TrackingNumber := 'Q76PXM7V';
  OptimizationParameters.Addresses[Index(3)].CustomFields := TDictionaryStringIntermediateObject.Create;

  OptimizationParameters.Addresses[Index(4)].RouteDestinationId := 193721039;
  OptimizationParameters.Addresses[Index(4)].MemberId := 1;
  OptimizationParameters.Addresses[Index(4)].CurbsideLatitude := 33.1410866;
  OptimizationParameters.Addresses[Index(4)].CurbsideLongitude := -83.2382584;
  OptimizationParameters.Addresses[Index(4)].SequenceNo := 7;
  OptimizationParameters.Addresses[Index(4)].IsDepot := False;
  OptimizationParameters.Addresses[Index(4)].RouteId := 'FC3D90EDECBAA80F46C37EAFBE4ADB36';
  OptimizationParameters.Addresses[Index(4)].DriveTimeToNextDestination := 380;
  OptimizationParameters.Addresses[Index(4)].DistanceToNextDestination := 1.85;
  OptimizationParameters.Addresses[Index(4)].TrackingNumber := 'RQEPR8QN';
  OptimizationParameters.Addresses[Index(4)].CustomFields := TDictionaryStringIntermediateObject.Create;

  OptimizationParameters.Addresses[Index(5)].RouteDestinationId := 193721040;
  OptimizationParameters.Addresses[Index(5)].MemberId := 1;
  OptimizationParameters.Addresses[Index(5)].CurbsideLatitude := 33.1420364;
  OptimizationParameters.Addresses[Index(5)].CurbsideLongitude := -83.2388458;
  OptimizationParameters.Addresses[Index(5)].SequenceNo := 6;
  OptimizationParameters.Addresses[Index(5)].IsDepot := False;
  OptimizationParameters.Addresses[Index(5)].RouteId := 'FC3D90EDECBAA80F46C37EAFBE4ADB36';
  OptimizationParameters.Addresses[Index(5)].DriveTimeToNextDestination := 14;
  OptimizationParameters.Addresses[Index(5)].DistanceToNextDestination := 0.06;
  OptimizationParameters.Addresses[Index(5)].TrackingNumber := 'Y3NPGE3Q';
  OptimizationParameters.Addresses[Index(5)].CustomFields := TDictionaryStringIntermediateObject.Create;

  OptimizationParameters.Addresses[Index(6)].RouteDestinationId := 193721041;
  OptimizationParameters.Addresses[Index(6)].MemberId := 1;
  OptimizationParameters.Addresses[Index(6)].CurbsideLatitude := 33.14307;
  OptimizationParameters.Addresses[Index(6)].CurbsideLongitude := -83.239334;
  OptimizationParameters.Addresses[Index(6)].SequenceNo := 5;
  OptimizationParameters.Addresses[Index(6)].IsDepot := False;
  OptimizationParameters.Addresses[Index(6)].RouteId := 'FC3D90EDECBAA80F46C37EAFBE4ADB36';
  OptimizationParameters.Addresses[Index(6)].DriveTimeToNextDestination := 24;
  OptimizationParameters.Addresses[Index(6)].DistanceToNextDestination := 0.11;
  OptimizationParameters.Addresses[Index(6)].TrackingNumber := 'ZLDPVNLY';
  OptimizationParameters.Addresses[Index(6)].CustomFields := TDictionaryStringIntermediateObject.Create;

  OptimizationParameters.Addresses[Index(7)].RouteDestinationId := 193721042;
  OptimizationParameters.Addresses[Index(7)].MemberId := 1;
  OptimizationParameters.Addresses[Index(7)].CurbsideLatitude := 33.1427345;
  OptimizationParameters.Addresses[Index(7)].CurbsideLongitude := -83.237442;
  OptimizationParameters.Addresses[Index(7)].SequenceNo := 2;
  OptimizationParameters.Addresses[Index(7)].IsDepot := False;
  OptimizationParameters.Addresses[Index(7)].RouteId := 'FC3D90EDECBAA80F46C37EAFBE4ADB36';
  OptimizationParameters.Addresses[Index(7)].DriveTimeToNextDestination := 0;
  OptimizationParameters.Addresses[Index(7)].DistanceToNextDestination := 0;
  OptimizationParameters.Addresses[Index(7)].TrackingNumber := 'PLKP0DLM';
  OptimizationParameters.Addresses[Index(7)].CustomFields := TDictionaryStringIntermediateObject.Create;

  OptimizationParameters.Addresses[Index(8)].RouteDestinationId := 193721043;
  OptimizationParameters.Addresses[Index(8)].MemberId := 1;
  OptimizationParameters.Addresses[Index(8)].CurbsideLatitude := 33.1438713;
  OptimizationParameters.Addresses[Index(8)].CurbsideLongitude := -83.2373428;
  OptimizationParameters.Addresses[Index(8)].SequenceNo := 4;
  OptimizationParameters.Addresses[Index(8)].IsDepot := False;
  OptimizationParameters.Addresses[Index(8)].RouteId := 'FC3D90EDECBAA80F46C37EAFBE4ADB36';
  OptimizationParameters.Addresses[Index(8)].DriveTimeToNextDestination := 44;
  OptimizationParameters.Addresses[Index(8)].DistanceToNextDestination := 0.2;
  OptimizationParameters.Addresses[Index(8)].TrackingNumber := 'LGM3KQGK';
  OptimizationParameters.Addresses[Index(8)].CustomFields := TDictionaryStringIntermediateObject.Create;

  OptimizationParameters.Addresses[Index(9)].RouteDestinationId := 193721044;
  OptimizationParameters.Addresses[Index(9)].MemberId := 1;
  OptimizationParameters.Addresses[Index(9)].CurbsideLatitude := 33.0814629;
  OptimizationParameters.Addresses[Index(9)].CurbsideLongitude := -83.2085114;
  OptimizationParameters.Addresses[Index(9)].SequenceNo := 9;
  OptimizationParameters.Addresses[Index(9)].IsDepot := False;
  OptimizationParameters.Addresses[Index(9)].RouteId := 'FC3D90EDECBAA80F46C37EAFBE4ADB36';
  OptimizationParameters.Addresses[Index(9)].TrackingNumber := 'DQ9MGE6V';
  OptimizationParameters.Addresses[Index(9)].CustomFields := TDictionaryStringIntermediateObject.Create;

  for i := 0 to 9 do
  begin
    OptimizationParameters.Addresses[Index(i)].OptimizationProblemId := 'E589A245F11E3E1E82457703BFAAB14D';
    OptimizationParameters.Addresses[Index(i)].TimeframeViolationTime := 0;
    OptimizationParameters.Addresses[Index(i)].TimeframeViolationRate := 0;
    OptimizationParameters.Addresses[Index(i)].AddressStopType := astDelivery;
    OptimizationParameters.Addresses[Index(i)].Geocoded := False;
    OptimizationParameters.Addresses[Index(i)].FailedGeocoding := False;
    OptimizationParameters.Addresses[Index(i)].IsVisited := False;
    OptimizationParameters.Addresses[Index(i)].IsDeparted := False;
    OptimizationParameters.Addresses[Index(i)].Weight := 0;
    OptimizationParameters.Addresses[Index(i)].Cost := 0;
    OptimizationParameters.Addresses[Index(i)].Revenue := 0;
    OptimizationParameters.Addresses[Index(i)].Cube := 0;
    OptimizationParameters.Addresses[Index(i)].Pieces := 0;
    OptimizationParameters.Addresses[Index(i)].DestinationNoteCount := 0;

  end;

//  OptimizationParameters.Addresses[Index(9)].CustomFields := TDictionaryStringIntermediateObject.Create;
end;

function TSingleDriverRoute10StopsTestDataProvider.MakeAddresses: TAddressesArray;
var
  FirstAddress: TAddress;
  i: integer;
begin
  Result := TAddressesArray.Create();

  FirstAddress := TAddress.Create(
    Addresses[0].AddressString, Addresses[0].Latitude, Addresses[0].Longitude, 0);
  //indicate that this is a departure stop
  // single depot routes can only have one departure depot
  FirstAddress.IsDepot := True;
  // input as many custom fields as needed, custom data is passed through to mobile devices and to the manifest
  FirstAddress.AddCustomField('color', 'red');
  FirstAddress.AddCustomField('size', 'huge');
  AddAddress(FirstAddress, Result);

  for i := 1 to High(Addresses) do
    AddAddress(TAddress.Create(
      Addresses[i].AddressString, Addresses[i].Latitude, Addresses[i].Longitude, 0),
      Result);
end;

function TSingleDriverRoute10StopsTestDataProvider.MakeRouteParameters: TRouteParameters;
begin
  Result := TRouteParameters.Create();
  Result.AlgorithmType := TAlgorithmType.TSP;
  Result.StoreRoute := False;
  Result.RouteName := 'Single Driver Route 10 Stops';
  Result.RouteDate := 53583232;//TUtils.ConvertToUnixTimestamp(IncDay(Now, 1));
  Result.RouteTime := 60 * 60 * 7;
  Result.Optimize := TOptimize.Distance;
  Result.DistanceUnit := TDistanceUnit.MI;
  Result.DeviceType := TDeviceType.Web;
end;

{ TAddressTag }

constructor TAddressTag.Create(AddressString: String; Latitude,
  Longitude: double);
begin
  Self.AddressString := AddressString;
  Self.Latitude := Latitude;
  Self.Longitude := Longitude;
end;

function TAddressTag.Equals(Address: TAddress): boolean;
begin
  Result := (AddressString = Address.AddressString) and
    (Latitude = Address.Latitude) and
    (Longitude = Address.Longitude);
end;

initialization
  SetLength(Addresses, 10);
  Addresses[0] := TAddressTag.Create('151 Arbor Way Milledgeville GA 31061', 33.132675170898, -83.244743347168);
  Addresses[1] := TAddressTag.Create('230 Arbor Way Milledgeville GA 31061', 33.129695892334, -83.24577331543);
  Addresses[2] := TAddressTag.Create('148 Bass Rd NE Milledgeville GA 31061', 33.143497, -83.224487);
  Addresses[3] := TAddressTag.Create('117 Bill Johnson Rd NE Milledgeville GA 31061', 33.141784667969, -83.237518310547);
  Addresses[4] := TAddressTag.Create('119 Bill Johnson Rd NE Milledgeville GA 31061', 33.141086578369, -83.238258361816);
  Addresses[5] := TAddressTag.Create('131 Bill Johnson Rd NE Milledgeville GA 31061', 33.142036437988, -83.238845825195);
  Addresses[6] := TAddressTag.Create('138 Bill Johnson Rd NE Milledgeville GA 31061', 33.14307, -83.239334);
  Addresses[7] := TAddressTag.Create('139 Bill Johnson Rd NE Milledgeville GA 31061', 33.142734527588, -83.237442016602);
  Addresses[8] := TAddressTag.Create('145 Bill Johnson Rd NE Milledgeville GA 31061', 33.143871307373, -83.237342834473);
  Addresses[9] := TAddressTag.Create('221 Blake Cir Milledgeville GA 31061', 33.081462860107, -83.208511352539);

end.
