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
  DataObjects: TDataObjectList;
  DataObject: TDataObject;
  ErrorString: String;
  Limit, Offset: integer;
begin
  Limit := 10;
  Offset := 5;

  DataObjects := Route4MeManager.Optimization.Get(Limit, Offset, ErrorString);
  try
    WriteLn('');

    if (DataObjects.Count > 0) then
    begin
        WriteLn(Format(
          'GetOptimizations executed successfully, %d optimizations returned',
          [DataObjects.Count]));
        WriteLn('');

        for DataObject in DataObjects do
          WriteLn(Format('Optimization Problem ID: %s', [DataObject.OptimizationProblemId]));
    end
    else
      WriteLn(Format('GetOptimizations error: "%s"', [ErrorString]));
  finally
    FreeAndNil(DataObjects);
  end;
end;

end.
