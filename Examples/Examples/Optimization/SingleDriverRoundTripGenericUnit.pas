unit SingleDriverRoundTripGenericUnit;

interface

uses SysUtils, Route4MeExamplesUnit, BaseExampleUnit, NullableBasicTypesUnit;

type
  TSingleDriverRoundTripGeneric = class(TBaseExample)
  public
    function Execute: NullableString;
  end;

implementation

uses
  IOptimizationParametersProviderUnit, OptimizationParametersUnit, AddressUnit,
  SingleDriverRoundTripGenericRequestUnit,
  SingleDriverRoundTripGenericResponseUnit,
  SingleDriverRoundTripGenericTestDataProviderUnit, RouteParametersUnit,
  SettingsUnit, EnumsUnit;

function TSingleDriverRoundTripGeneric.Execute: NullableString;
var
  DataProvider: IOptimizationParametersProvider;
  ErrorString: String;
  Request: TSingleDriverRoundTripGenericRequest;
  Response: TSingleDriverRoundTripGenericResponse;
  Address: TAddress;
  Parameters: TOptimizationParameters;
begin
  Result := NullableString.Null;

  DataProvider := TSingleDriverRoundTripGenericTestDataProvider.Create;
  Request := TSingleDriverRoundTripGenericRequest.Create;
  try
    Parameters := DataProvider.OptimizationParameters;
    try
      Request.Parameters := Parameters.Parameters.Value as TRouteParameters;
      Request.Addresses := Parameters.Addresses;

      Response := Route4MeManager.Connection.Post(TSettings.ApiHost, Request,
        TSingleDriverRoundTripGenericResponse, ErrorString) as TSingleDriverRoundTripGenericResponse;
      try
        WriteLn('');

        if (Response <> nil) then
        begin
          WriteLn('SingleDriverRoundTripGeneric executed successfully');
          WriteLn('');

          WriteLn(Format('Optimization Problem ID: %s', [Response.OptimizationProblemId]));
          WriteLn(Format('State: %s',
            [TOptimizationDescription[TOptimizationState(Response.MyState)]]));
          WriteLn('');

          for Address in Response.Addresses do
          begin
            WriteLn(Format('Address: %s', [Address.AddressString]));
            WriteLn(Format('Route ID: %s', [Address.RouteId.ToString]));
          end;

          Result := Response.OptimizationProblemId;
        end
        else
          WriteLn(Format('SingleDriverRoundTripGeneric error "%s"', [ErrorString]));
      finally
        FreeAndNil(Response);
      end;
    finally
      FreeAndNil(Parameters);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

end.
