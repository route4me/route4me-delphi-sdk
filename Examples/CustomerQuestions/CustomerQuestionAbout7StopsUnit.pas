unit CustomerQuestionAbout7StopsUnit;

interface

uses
  SysUtils, Classes,
  OutputUnit, AddressUnit;

const
  //your api key
  Default_ApiKey = '11111111111111111111111111111111';

type
  TCustomerQuestionAbout7Stops = class
  private
    FOutput: IOutput;
    FApiKey: String;

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

constructor TCustomerQuestionAbout7Stops.Create;
const
  ApikeyFilename = '..\..\..\..\apikey.txt';
var
  st: TStringList;
begin
  FOutput := TOutputConsole.Create;
  if FileExists(ApikeyFilename) then
  begin
    st := TStringList.Create;
    try
      st.LoadFromFile(ApikeyFilename);
      FApiKey := st[0];
    finally
      FreeAndNil(st);
    end;
  end
  else
    FApiKey := Default_ApiKey;
end;

destructor TCustomerQuestionAbout7Stops.Destroy;
begin
  FOutput := nil;

  inherited;
end;

procedure TCustomerQuestionAbout7Stops.Execute;
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
  GetRouteDirections: Boolean;
  GetRoutePathPoints: Boolean;
begin
  try
    Route4MeManager := TRoute4MeManager.Create(TConnection.Create(FApiKey));

    // Prepared input data with parameters
    RouteParameters := TRouteParameters.Create;
    RouteParameters.AlgorithmType := TAlgorithmType.TSP;
    RouteParameters.StoreRoute := True;
    RouteParameters.RouteName := 'Single Driver Route (Delphi, trucking)';
    RouteParameters.Optimize := TOptimize.Distance;
    RouteParameters.DistanceUnit := TDistanceUnit.MI;
    RouteParameters.DeviceType := TDeviceType.Web;
    RouteParameters.TravelMode := TTravelMode.Trucking;

    Parameters := TOptimizationParameters.Create;
    try
      Parameters.Parameters := RouteParameters;
      Address := MakeAddress('151 Arbor Way Milledgeville, GA 31061, USA', 33.132701, -83.244743);
      Address.SequenceNo := 1;
      Parameters.AddAddress(Address);
      Parameters.AddAddress(MakeAddress('119 Bill Johnson Road Northeast Milledgeville, GA 31061, USA', 33.141095, -83.238278));
      Parameters.AddAddress(MakeAddress('138 Bill Johnson Road Northeast Milledgeville, GA 31061, USA', 33.143078, -83.239336));
      Parameters.AddAddress(MakeAddress('133 Bill Johnson Road Northeast Milledgeville, GA 31061, USA', 33.142300, -83.239066));
      Parameters.AddAddress(MakeAddress('139 Bill Johnson Road Northeast Milledgeville, GA 31061, USA', 33.142745, -83.237464));
      Parameters.AddAddress(MakeAddress('117 Bill Johnson Road Northeast Milledgeville, GA 31061, USA', 33.141329, -83.238278));
      Parameters.AddAddress(MakeAddress('151 Bill Johnson Road Northeast Milledgeville, GA 31061, USA', 33.144197, -83.237563));

      // Run optimization
      DataObject := Route4MeManager.Optimization.Run(Parameters, ErrorString);
      PrintIfHasError(ErrorString);
      try
        // Get more details
        OptimizationProblemDetails := Route4MeManager.Optimization.Get(
          DataObject.OptimizationProblemId, ErrorString);
        PrintIfHasError(ErrorString);
        try
          for Route in OptimizationProblemDetails.Routes do
          begin
            GetRouteDirections := False;
            GetRoutePathPoints := True;
            DetailedRoute := Route4MeManager.Route.Get(Route.RouteId,
              GetRouteDirections, GetRoutePathPoints, ErrorString);
            try
              PrintIfHasError(ErrorString);
            finally
              FreeAndNil(DetailedRoute);
            end;
          end;
        finally
          FreeAndNil(OptimizationProblemDetails);
        end;
      finally
        FreeAndNil(DataObject);
      end;
    finally
      FreeAndNil(Parameters);
    end;
  finally
    WriteLn('');
    WriteLn('Press any key');

    ReadLn;
  end;
end;

function TCustomerQuestionAbout7Stops.MakeAddress(Name: String; Latitude,
  Longitude: double): TAddress;
begin
  Result := TAddress.Create;
  Result.AddressString := Name;
  Result.Latitude := Latitude;
  Result.Longitude := Longitude;
  Result.IsDepot := False;
end;

procedure TCustomerQuestionAbout7Stops.PrintIfHasError(ErrorString: String);
begin
  if (ErrorString <> EmptyStr) then
  begin
    FOutput.Writeln(ErrorString);
    raise Exception.Create('We has error message');
  end;
end;

end.
