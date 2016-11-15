unit AddRouteDestinationsUnit;

interface

uses SysUtils, BaseOptimizationExampleUnit;

type
  TAddRouteDestinations = class(TBaseOptimizationExample)
  public
    function Execute(RouteId: String): TArray<integer>;
  end;

implementation

uses
  AddressUnit, CommonTypesUnit;

function TAddRouteDestinations.Execute(RouteId: String): TArray<integer>;
var
  Addresses: TAddressesArray;
  OptimalPosition: boolean;
  ErrorString: String;
  AddressIds: TStringArray;
  i: integer;
begin
  SetLength(Addresses, 1);
  try
    Addresses[0] := TAddress.Create(
      '146 Bill Johnson Rd NE Milledgeville GA 31061',
      33.143526, -83.240354, 0);
    Addresses[0].SequenceNo := 4;

    OptimalPosition := False;
    Result := Route4MeManager.Route.Add(RouteId, Addresses, OptimalPosition, ErrorString);

    WriteLn('');

    if (Length(Result) > 0) then
    begin
      WriteLn('AddRouteDestinations executed successfully');

      SetLength(AddressIds, Length(Result));
      for i := 0 to Length(Result) - 1 do
        AddressIds[i] := IntToStr(Result[i]);
      WriteLn(Format('Destination IDs: %s', [String.Join(' ', AddressIds)]));
    end
    else
      WriteLn(Format('AddRouteDestinations error: "%s"', [errorString]));
  finally
    for i := Length(Addresses) - 1 downto 0 do
      FreeAndNil(Addresses[i]);
  end;
end;

end.
