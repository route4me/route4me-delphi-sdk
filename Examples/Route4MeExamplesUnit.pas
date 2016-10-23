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

    function SingleDriverRoute10Stops: TDataObject;
    procedure ResequenceRouteDestinations(Route: TDataObjectRoute);

  end;

implementation

{ TRoute4MeExamples }

uses AddressUnit, EnumsUnit, IOptimizationParametersProviderUnit,
  OptimizationParametersUnit, SingleDriverRoute10StopsTestDataProviderUnit,
  GenericParametersUnit, AddressesOrderInfoUnit;

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
    WriteLn(Format('%s executed successfully', [ExampleName]));
    WriteLn('');

    WriteLn(Format('Optimization Problem ID: %s', [DataObject.OptimizationProblemId]));
    WriteLn(Format('State: %s', [TOptimizationDescription[TOptimizationState(DataObject.State)]]));

    for UserError in DataObject.UserErrors do
      WriteLn(Format('UserError : "$s"', [UserError]));

    WriteLn('');

    // Sort addresses by sequence order
    AddressesSorted := AddressUnit.SortAddresses(DataObject.Addresses);
    for Address in AddressesSorted do
    begin
      WriteLn(Format('Address: %s', [Address.AddressString]));
      WriteLn(Format('Route ID: %s', [Address.RouteId.ToString]));
    end;
  end
  else
    WriteLn(Format('%s error: "%s"', [ExampleName, ErrorString]));
end;

procedure TRoute4MeExamples.ResequenceRouteDestinations(
  Route: TDataObjectRoute);
var
  AddressesOrderInfo: TAddressesOrderInfo;
  Address: TAddress;
  AddressInfo: TAddressInfo;
  i: integer;
  SequenceNo: integer;
  ErrorString: String;
  NewRoute: TDataObjectRoute;
begin
  if (Length(Route.Addresses) < 3) then
  begin
    WriteLn(Format('ResequenceRouteDestinations error: "%s"',
      ['Route.Addresses.Length < 3. Number of addresses should be >= 3']));
    Exit;
   end;

  // Switch 2 addresses after departure address:
  AddressesOrderInfo := TAddressesOrderInfo.Create(Route.RouteID);
  for i := 0 to Length(Route.Addresses) - 1 do
  begin
    Address := Route.Addresses[i];

    SequenceNo := i;
    if (i = 1) then
      SequenceNo := 2
    else
      if (i = 2) then
        SequenceNo := 1;

    AddressInfo := TAddressInfo.Create;
    AddressInfo.DestinationId := Address.RouteDestinationId.Value;
    AddressInfo.SequenceNo := SequenceNo;
    AddressInfo.IsDepot := (AddressInfo.SequenceNo = 0);

    AddressesOrderInfo.AddAddress(AddressInfo);
  end;

  // Run the query
  NewRoute := Route4MeManager.Route.Resequence(AddressesOrderInfo, ErrorString);

  // Output the result
  PrintExampleOptimizationResult('ResequenceRouteDestinations, switch 2 addresses.', NewRoute, ErrorString);
  WriteLn('');
end;

function TRoute4MeExamples.SingleDriverRoute10Stops: TDataObject;
var
  DataProvider: IOptimizationParametersProvider;
  ErrorString: String;
  DataObject: TDataObject;
begin
  DataProvider := TSingleDriverRoute10StopsTestDataProvider.Create;

  // Run the query
  DataObject := Route4MeManager.Optimization.Run(
    DataProvider.OptimizationParameters, ErrorString);

  // Output the result
  PrintExampleOptimizationResult('SingleDriverRoute10Stops', DataObject, ErrorString);

  Result := DataObject;
end;

end.
