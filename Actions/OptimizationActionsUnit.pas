unit OptimizationActionsUnit;

interface

uses
  SysUtils, BaseActionUnit,
  DataObjectUnit, OptimizationParametersUnit, RouteParametersQueryUnit,
  AddOrderToOptimizationRequestUnit;

type
  TOptimizationActions = class(TBaseAction)
  public
    function Run(OptimizationParameters: TOptimizationParameters;
      out ErrorString: String): TDataObject;

    function Get(OptimizationProblemId: String;
      out ErrorString: String): TDataObject; overload;

    function Get(Limit, Offset: integer;
      out ErrorString: String): TDataObjectList; overload;

    function Update(OptimizationParameters: TOptimizationParameters;
      out ErrorString: String): TDataObject;

    function Remove(OptimizationId: String; out ErrorString: String): boolean;

    function RemoveDestination(OptimizationId: String; DestinationId: integer;
      out ErrorString: String): boolean;

    /// <summary>
    ///  Insert an existing order into an existing optimization.
    /// </summary>
    function AddOrder(Parameters: TAddOrderToOptimizationRequest;
      out ErrorString: String): TDataObjectRoute;
  end;

implementation

{ TOptimizationActions }

uses
  SettingsUnit, DataObjectOptimizationsResponseUnit,
  RemoveDestinationFromOptimizationResponseUnit, GenericParametersUnit,
  CommonTypesUnit, RemoveOptimizationResponseUnit;

function TOptimizationActions.AddOrder(
  Parameters: TAddOrderToOptimizationRequest;
  out ErrorString: String): TDataObjectRoute;
begin
  Result := FConnection.Put(TSettings.EndPoints.Optimization,
    Parameters, TDataObjectRoute, ErrorString) as TDataObjectRoute;

  if (Result = nil) and (ErrorString = EmptyStr) then
    ErrorString := 'Order to an optimization not added';
end;

function TOptimizationActions.Get(Limit, Offset: integer;
  out ErrorString: String): TDataObjectList;
var
  Response: TDataObjectOptimizationsResponse;
  Request: TRouteParametersQuery;
  i: integer;
begin
  Result := TDataObjectList.Create;

  Request := TRouteParametersQuery.Create;
  try
    Request.Limit := Limit;
    Request.Offset := Offset;

    Response := FConnection.Get(TSettings.EndPoints.Optimization, Request,
      TDataObjectOptimizationsResponse, ErrorString) as TDataObjectOptimizationsResponse;

    try
      if (Response <> nil) then
        for i := 0 to Length(Response.Optimizations) - 1 do
          Result.Add(Response.Optimizations[i]);
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request)
  end;
end;

function TOptimizationActions.Get(OptimizationProblemId: String;
  out ErrorString: String): TDataObject;
var
  OptimizationParameters: TOptimizationParameters;
begin
  OptimizationParameters := TOptimizationParameters.Create;
  try
    OptimizationParameters.OptimizationProblemID := OptimizationProblemId;

    Result := FConnection.Get(TSettings.EndPoints.Optimization, OptimizationParameters,
      TDataObject, ErrorString) as TDataObject;

//    Result := Get(OptimizationParameters, ErrorString);
  finally
    FreeAndNil(OptimizationParameters);
  end;
end;

function TOptimizationActions.Remove(OptimizationId: String;
  out ErrorString: String): boolean;
var
  GenericParameters: TGenericParameters;
  Response: TRemoveOptimizationResponse;
begin
  GenericParameters := TGenericParameters.Create();
  try
    GenericParameters.AddParameter('optimization_problem_id', OptimizationId);
    Response := FConnection.Delete(TSettings.EndPoints.Optimization, GenericParameters,
      TRemoveOptimizationResponse, ErrorString) as TRemoveOptimizationResponse;
    try
      Result := (Response <> nil) and (Response.Status) and (Response.Removed);
      if (not Result) and (ErrorString = EmptyStr) then
        ErrorString := 'Error removing optimization';
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(GenericParameters);
  end;
end;

function TOptimizationActions.RemoveDestination(OptimizationId: String;
  DestinationId: integer; out ErrorString: String): boolean;
var
  Response: TRemoveDestinationFromOptimizationResponse;
  GenericParameters: TGenericParameters;
begin
  GenericParameters := TGenericParameters.Create();
  try
    GenericParameters.AddParameter('optimization_problem_id', OptimizationId);
    GenericParameters.AddParameter('route_destination_id', IntToStr(DestinationId));

    Response := FConnection.Delete(TSettings.EndPoints.GetAddress, GenericParameters,
      TRemoveDestinationFromOptimizationResponse, ErrorString) as TRemoveDestinationFromOptimizationResponse;
    try
      Result := (Response <> nil) and (Response.Deleted);
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(GenericParameters);
  end;
end;

function TOptimizationActions.Run(
  OptimizationParameters: TOptimizationParameters;
  out ErrorString: String): TDataObject;
begin
  Result := FConnection.Post(TSettings.EndPoints.Optimization, OptimizationParameters,
    TDataObject, ErrorString) as TDataObject;
end;

function TOptimizationActions.Update(
  OptimizationParameters: TOptimizationParameters;
  out ErrorString: String): TDataObject;
begin
  Result := FConnection.Put(TSettings.EndPoints.Optimization, OptimizationParameters,
    TDataObject, ErrorString) as TDataObject;
end;

end.
