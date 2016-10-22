unit Route4MeExamplesUnit;

interface

uses
  SysUtils,
  Route4MeManagerUnit, DataObjectUnit;

type
  TRoute4MeExamples = class
    //your api key
    const c_ApiKey = '11111111111111111111111111111111';
  protected
    Route4MeManager: TRoute4MeManager;

    procedure PrintExampleOptimizationResult(ExampleName: String;
      DataObject: TDataObject; ErrorString: String);
  public
    constructor Create;
    constructor CreateDebug;

    destructor Destroy; override;
  end;

implementation

{ TRoute4MeExamples }

uses AddressUnit, EnumsUnit;

constructor TRoute4MeExamples.Create;
begin
  // Create the manager with the api key
  Route4MeManager := TRoute4MeManager.Create(c_ApiKey);
end;

constructor TRoute4MeExamples.CreateDebug;
begin
  Create;
  Route4MeManager.SetConnectionProxy('irr-px01.rzdp.ru', 8080, 'BorzenkovIS', 'DocsVision33');
end;

destructor TRoute4MeExamples.Destroy;
begin
  Route4MeManager.Free;
  inherited;
end;

procedure TRoute4MeExamples.PrintExampleOptimizationResult(ExampleName: String;
  DataObject: TDataObject; ErrorString: String);
var
  UserError: String;
  Address: TAddress;
  AddressesSorted: TArray<TAddress>;
begin
  Writeln('');

  if (DataObject <> nil) then
  begin
    WriteLn(Format('$s executed successfully', [ExampleName]));
    WriteLn('');

    WriteLn(Format('Optimization Problem ID: %s', [DataObject.OptimizationProblemId]));
    WriteLn(Format('State: %s', [TOptimizationDescription[TOptimizationState(DataObject.State)]]));

    for UserError in DataObject.UserErrors do
      WriteLn(Format('UserError : "$s"', [UserError]));

    WriteLn('');

    // Sort addresses by sequence order
    AddressesSorted := DataObject.SortAddresses;
    for Address in AddressesSorted do
    begin
      WriteLn(Format('Address: %s', [Address.AddressString]));
      WriteLn(Format('Route ID: %s', [Address.RouteId.ToString]));
    end;
  end
  else
    WriteLn(Format('%s error: "%s"', [ExampleName, ErrorString]));
end;

end.
