unit IOptimizationParametersProviderUnit;

interface

uses
  OptimizationParametersUnit;

type
  IOptimizationParametersProvider = interface
    ['{97BB0ECC-F135-4B21-A990-FBBC9C65C155}']

    function OptimizationParameters: TOptimizationParameters;
    function OptimizationParametersForResponce: TOptimizationParameters;
  end;

implementation

end.
