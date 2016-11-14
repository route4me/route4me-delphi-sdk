unit BaseOptimizationExampleUnit;

interface

uses
  SysUtils,
  BaseExampleUnit, DataObjectUnit;

type
  TBaseOptimizationExample = class abstract (TBaseExample)
  protected
    procedure PrintExampleOptimizationResult(ExampleName: String;
      DataObject: TDataObject; ErrorString: String);
  end;

implementation

{ TBaseOptimizationExample }

uses AddressUnit, EnumsUnit;

procedure TBaseOptimizationExample.PrintExampleOptimizationResult(ExampleName: String;
  DataObject: TDataObject; ErrorString: String);
var
  UserError: String;
  Address: TAddress;
  AddressesSorted: TAddressesArray;
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

end.
