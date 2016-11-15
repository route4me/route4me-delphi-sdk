unit AddDestinationToOptimizationUnit;

interface

uses SysUtils, BaseExampleUnit, DataObjectUnit;

type
  TAddDestinationToOptimization = class(TBaseExample)
  public
    function Execute(OptimizationId: String;
      AndReOptimize: boolean): TDataObject;
  end;

implementation

uses AddressUnit, OptimizationParametersUnit, EnumsUnit;

function TAddDestinationToOptimization.Execute(OptimizationId: String;
  AndReOptimize: boolean): TDataObject;
var
  Address: TAddress;
  Parameters: TOptimizationParameters;
  ErrorString: String;
begin
  Address := TAddress.Create;
  Address.AddressString := '717 5th Ave New York, NY 10021';
  Address.Alias := 'Giorgio Armani';
  Address.Latitude := 40.7669692;
  Address.Longitude := -73.9693864;
  Address.Time := 0;

  Parameters := TOptimizationParameters.Create;
  try
    Parameters.OptimizationProblemID := OptimizationId;
    Parameters.AddAddress(Address);
    Parameters.ReOptimize := AndReOptimize;

    Result := Route4MeManager.Optimization.Update(Parameters, ErrorString);

    WriteLn('');

    if (Result <> nil) then
    begin
      WriteLn('AddDestinationToOptimization executed successfully');
      WriteLn(Format('Optimization Problem ID: %s', [Result.OptimizationProblemId]));
      WriteLn(Format('State: %s',
        [TOptimizationDescription[TOptimizationState(Result.State)]]));
    end
    else
      WriteLn(Format('AddDestinationToOptimization error: %s', [ErrorString]));
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
