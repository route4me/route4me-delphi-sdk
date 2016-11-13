unit OptimizationActionUnit;

interface

uses
  SysUtils, BaseActionUnit,
  DataObjectUnit, OptimizationParametersUnit, RouteParametersQueryUnit;

type
  TOptimizationActions = class(TBaseAction)
  public
    function Run(OptimizationParameters: TOptimizationParameters;
      out ErrorString: String): TDataObject;

    function Get(OptimizationParameters: TOptimizationParameters;
      out ErrorString: String): TDataObject; overload;

    function Get(QueryParameters: TRouteParametersQuery;
      out ErrorString: String): TArray<TDataObject>; overload;

    function Update(OptimizationParameters: TOptimizationParameters;
      out ErrorString: String): TDataObject;

    function Remove(OptimizationId: String; out ErrorString: String): boolean;

    function RemoveDestination(OptimizationId: String; DestinationId: integer;
      out ErrorString: String): boolean;
  end;

implementation

{ TOptimizationActions }

uses
  SettingsUnit, DataObjectOptimizationsResponseUnit,
  RemoveDestinationFromOptimizationResponseUnit, GenericParametersUnit,
  CommonTypesUnit, RemoveOptimizationResponseUnit;

function TOptimizationActions.Get(
  OptimizationParameters: TOptimizationParameters;
  out ErrorString: String): TDataObject;
begin
  Result := FConnection.Get(TSettings.ApiHost, OptimizationParameters,
    TDataObject, ErrorString) as TDataObject;
end;

function TOptimizationActions.Get(QueryParameters: TRouteParametersQuery;
  out ErrorString: String): TArray<TDataObject>;
var
  Response: TDataObjectOptimizationsResponse;
begin
  SetLength(Result, 0);

  Response := FConnection.Get(TSettings.ApiHost, QueryParameters,
    TDataObjectOptimizationsResponse, ErrorString) as TDataObjectOptimizationsResponse;

  try
    if (Response <> nil) then
      Result := Response.Optimizations;
  finally
    FreeAndNil(Response);
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
    Response := FConnection.Delete(TSettings.ApiHost, GenericParameters,
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

    Response := FConnection.Delete(TSettings.GetAddress, GenericParameters,
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
  Result := FConnection.Post(TSettings.ApiHost, OptimizationParameters,
    TDataObject, ErrorString) as TDataObject;
end;

function TOptimizationActions.Update(
  OptimizationParameters: TOptimizationParameters;
  out ErrorString: String): TDataObject;
begin
  Result := FConnection.Put(TSettings.ApiHost, OptimizationParameters,
    TDataObject, ErrorString) as TDataObject;
end;

end.
