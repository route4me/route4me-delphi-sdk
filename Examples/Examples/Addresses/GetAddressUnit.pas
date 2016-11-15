unit GetAddressUnit;

interface

uses SysUtils, BaseExampleUnit;

type
  TGetAddress = class(TBaseExample)
  public
    procedure Execute(RouteId: String; RouteDestinationId: integer);
  end;

implementation

uses AddressParametersUnit, AddressUnit;

procedure TGetAddress.Execute(RouteId: String; RouteDestinationId: integer);
var
  ErrorString: String;
  Parameters: TAddressParameters;
  Address: TAddress;
begin
  Parameters := TAddressParameters.Create();
  try
    Parameters.RouteId := RouteId;
    Parameters.RouteDestinationId := RouteDestinationId;
    Parameters.Notes := True;

    Address := Route4MeManager.Address.Get(Parameters, ErrorString);
    try
      WriteLn('');

      if (Address <> nil) then
      begin
        WriteLn('GetAddress executed successfully');
        WriteLn(Format('RouteId: %s; RouteDestinationId: %d',
          [Address.RouteId.Value, Address.RouteDestinationId.Value]));
      end
      else
        WriteLn(Format('GetAddress error: "%s"', [ErrorString]));
    finally
      FreeAndNil(Address);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
