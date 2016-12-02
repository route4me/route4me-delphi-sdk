unit CustomerQuestionAbout10StopsUnit;

interface

uses
  SysUtils, Classes,
  OutputUnit, AddressUnit;

const
  //your api key
  c_ApiKey = '11111111111111111111111111111111';

type
  TCustomerQuestionAbout10Stops = class
  private
    FOutput: IOutput;

    function MakeAddress(Name: String; Latitude, Longitude: double): TAddress;
    procedure PrintIfHasError(ErrorString: String);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute;
  end;

implementation

uses
  OptimizationParametersUnit, IRoute4MeManagerUnit, Route4MeManagerUnit,
  RouteParametersUnit, DataObjectUnit, ConnectionUnit,
  EnumsUnit;

constructor TCustomerQuestionAbout10Stops.Create;
begin
  FOutput := TOutputConsole.Create;
end;

destructor TCustomerQuestionAbout10Stops.Destroy;
begin
  FOutput := nil;

  inherited;
end;

procedure TCustomerQuestionAbout10Stops.Execute;
var
  Route4MeManager: IRoute4MeManager;
  RouteParameters: TRouteParameters;
  Address: TAddress;
  ErrorString: String;
  Parameters: TOptimizationParameters;
  DataObject: TDataObject;
  OptimizationProblemDetails: TDataObject;
  Route: TDataObjectRoute;
  DetailedRoute: TDataObjectRoute;
begin
  Route4MeManager := TRoute4MeManager.Create(TConnection.Create(c_ApiKey));
  try
    // Prepared input data with parameters
    RouteParameters := TRouteParameters.Create;
    RouteParameters.AlgorithmType := TAlgorithmType.TSP;
    RouteParameters.StoreRoute := True;
    RouteParameters.RouteName := 'Single Driver Route 6 Stops';
    RouteParameters.Optimize := TOptimize.Distance;
    RouteParameters.DistanceUnit := TDistanceUnit.MI;
    RouteParameters.DeviceType := TDeviceType.Web;

    Parameters := TOptimizationParameters.Create;
    try
      Parameters.Parameters := RouteParameters;
      Address := MakeAddress('139 Bill Johnson Road Northeast Milledgeville, GA 31061, USA', 33.142862, -83.237459);
      Address.SequenceNo := 1;
      Parameters.AddAddress(Address);
      Parameters.AddAddress(MakeAddress('117 Bill Johnson Road Northeast Milledgeville, GA 31061, USA', 33.141923, -83.237532));
      Parameters.AddAddress(MakeAddress('151 Bill Johnson Road Northeast Milledgeville, GA 31061, USA', 33.144148, -83.237352));
      Parameters.AddAddress(MakeAddress('138 Bill Johnson Road Northeast Milledgeville, GA 31061, USA', 33.143338, -83.239325));
      Parameters.AddAddress(MakeAddress('133 Bill Johnson Road Northeast Milledgeville, GA 31061, USA', 33.142288, -83.238881));
      Parameters.AddAddress(MakeAddress('119 Bill Johnson Road Northeast Milledgeville, GA 31061, USA', 33.141329, -83.238278));

      // Run optimization
      DataObject := Route4MeManager.Optimization.Run(Parameters, ErrorString);
      PrintIfHasError(ErrorString);
      try
        // Get more details
        OptimizationProblemDetails := Route4MeManager.Optimization.Get(
          DataObject.OptimizationProblemId, ErrorString);
        PrintIfHasError(ErrorString);

        for Route in OptimizationProblemDetails.Routes do
        begin
          DetailedRoute := Route4MeManager.Route.Get(Route.RouteId, TRoutePathOutput.rpoPoints, ErrorString);
          PrintIfHasError(ErrorString);
        end;
      finally
        FreeAndNil(DataObject);
      end;
    finally
      FreeAndNil(Parameters);
    end;
  finally
    FreeAndNil(Route4MeManager);
  end;
end;

function TCustomerQuestionAbout10Stops.MakeAddress(Name: String; Latitude,
  Longitude: double): TAddress;
begin
  Result := TAddress.Create;
  Result.AddressString := Name;
  Result.Latitude := Latitude;
  Result.Longitude := Longitude;
  Result.IsDepot := False;
end;

procedure TCustomerQuestionAbout10Stops.PrintIfHasError(ErrorString: String);
begin
  if (ErrorString <> EmptyStr) then
  begin
    FOutput.Writeln(ErrorString);
    raise Exception.Create('We has error message');
  end;
end;

end.
