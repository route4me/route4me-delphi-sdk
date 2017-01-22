unit TestRouteSamplesUnit;

interface

uses
  TestFramework, Classes, SysUtils, DateUtils,
  BaseTestOnlineExamplesUnit;

type
  TTestRouteSamples = class(TTestOnlineExamples)
  private
  published
    procedure AddRoute;
    procedure AddOrderToRoute;
    procedure GetListByText;
    procedure RemoveRoute;
  end;

implementation

uses AddressUnit, DataObjectUnit, RouteParametersUnit, EnumsUnit,
  MultipleDepotMultipleDriverTestDataProviderUnit, OptimizationParametersUnit,
  IOptimizationParametersProviderUnit, CommonTypesUnit, NullableBasicTypesUnit;

var
  FRouteId: NullableString;
  FAllRouteIds: TArray<String>;
  FOptimizationProblemId: NullableString;

procedure TTestRouteSamples.AddOrderToRoute;
{var
  Route: TDataObjectRoute;
  Parameters: TRouteParameters;
  Addresses: TOrderedAddress;
  ErrorString: String;
  RouteId: String;}
begin
// todo 4: сделать unit-тест
{  RouteId := 'qwe';
  Parameters := TRouteParameters.Create;
  Parameters.RouteName := 'Test Route';
  Parameters.Optimize := TOptimize.Time;
  Parameters.AlgorithmType := TAlgorithmType.TSP;
  Parameters.MemberId := '1116';
  Parameters.DisableOptimization := False;
  try
    Route := FRoute4MeManager.Route.AddOrder(FRouteId, Parameters, Addresses, ErrorString);

    CheckNull(Route);
    CheckNotEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Parameters);
  end;}
end;

procedure TTestRouteSamples.AddRoute;
var
  DataProvider: IOptimizationParametersProvider;
  DataObject: TDataObject;
  OptimizationProblemDetails: TDataObject;
  Parameters: TOptimizationParameters;
  ErrorString: String;
  i: integer;
begin
  FOptimizationProblemId := NullableString.Null;
  FRouteId := NullableString.Null;

  DataProvider := TMultipleDepotMultipleDriverTestDataProvider.Create;
  try
    Parameters := DataProvider.OptimizationParameters;
    try
      DataObject := FRoute4MeManager.Optimization.Run(Parameters, ErrorString);
      try
        CheckNotNull(DataObject);
        CheckEquals(EmptyStr, ErrorString);

        FOptimizationProblemId := DataObject.OptimizationProblemId;
      finally
        FreeAndNil(DataObject);
      end;
    finally
      FreeAndNil(Parameters);
    end;

    OptimizationProblemDetails := FRoute4MeManager.Optimization.Get(
      FOptimizationProblemId, ErrorString);
    try
      CheckNotNull(OptimizationProblemDetails);
      CheckEquals(EmptyStr, ErrorString);
      CheckTrue(Length(OptimizationProblemDetails.Routes) > 0);

      FRouteId := OptimizationProblemDetails.Routes[0].RouteId;
      SetLength(FAllRouteIds, Length(OptimizationProblemDetails.Routes) - 1);
      for i := 1 to High(OptimizationProblemDetails.Routes) do
        FAllRouteIds[i - 1] := OptimizationProblemDetails.Routes[i].RouteId;
    finally
      FreeAndNil(OptimizationProblemDetails);
    end;

  finally
    DataProvider := nil;
  end;
end;

procedure TTestRouteSamples.GetListByText;
var
  ErrorString: String;
  Text: String;
  Routes: TDataObjectRouteList;
begin
  Text := 'SomeUniqueText#d27zz';
  Routes := FRoute4MeManager.Route.GetList(Text, ErrorString);
  try
    CheckNotNull(Routes);
    CheckEquals(0, Routes.Count);
  finally
    FreeAndNil(Routes);
  end;

  Text := 'Tbilisi';
  Routes := FRoute4MeManager.Route.GetList(Text, ErrorString);
  try
    CheckNotNull(Routes);
    CheckTrue(Routes.Count > 0);
  finally
    FreeAndNil(Routes);
  end;
end;

procedure TTestRouteSamples.RemoveRoute;
var
  ErrorString: String;
  DeletedRouteIds: TStringArray;
begin
  DeletedRouteIds := FRoute4MeManager.Route.Delete(['qwe'], ErrorString);
  CheckEquals(0, Length(DeletedRouteIds));
  CheckNotEquals(EmptyStr, ErrorString);

  CheckTrue(FRouteId.IsNotNull);
  DeletedRouteIds := FRoute4MeManager.Route.Delete([FRouteId], ErrorString);
  CheckEquals(1, Length(DeletedRouteIds));
  CheckEquals(FRouteId, DeletedRouteIds[0]);
  CheckEquals(EmptyStr, ErrorString);

  DeletedRouteIds := FRoute4MeManager.Route.Delete(FAllRouteIds, ErrorString);
  CheckTrue(Length(DeletedRouteIds) > 0);
  CheckEquals(EmptyStr, ErrorString);
end;

initialization
  RegisterTest('Examples\Online\Routes\', TTestRouteSamples.Suite);
end.
