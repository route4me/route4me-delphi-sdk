unit GetOptimizationsUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetOptimizations = class(TBaseExample)
  public
    procedure Execute;
  end;

implementation

uses RouteParametersQueryUnit, DataObjectUnit;

procedure TGetOptimizations.Execute;
var
  Parameters: TRouteParametersQuery;
  DataObjects: TArray<TDataObject>;
  DataObject: TDataObject;
  ErrorString: String;
  i: integer;
begin
  Parameters := TRouteParametersQuery.Create;
  try
    Parameters.Limit := 10;
    Parameters.Offset := 5;

    DataObjects := Route4MeManager.Optimization.Get(Parameters, ErrorString);
    try
      WriteLn('');

      if Length(DataObjects) > 0 then
      begin
          WriteLn(Format(
            'GetOptimizations executed successfully, %d optimizations returned',
            [Length(DataObjects)]));
          WriteLn('');

          for DataObject in DataObjects do
            WriteLn(Format('Optimization Problem ID: %s', [DataObject.OptimizationProblemId]));
      end
      else
        WriteLn(Format('GetOptimizations error: "%s"', [ErrorString]));
    finally
      for i := Length(DataObjects) - 1 downto 0 do
        FreeAndNil(DataObjects[i]);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
