unit AddressActionUnit;

interface

uses
  SysUtils, BaseActionUnit,
  AddressParametersUnit, AddressUnit;

type
  TAddressActions = class(TBaseAction)
  public
    function Get(AddressParameters: TAddressParameters;
      out ErrorString: String): TAddress;
  end;

implementation

{ TAddressActions }

uses
  SettingsUnit;

function TAddressActions.Get(AddressParameters: TAddressParameters;
  out ErrorString: String): TAddress;
begin
  Result := FConnection.Get(TSettings.GetAddress, AddressParameters,
    TAddress, ErrorString) as TAddress;
end;

end.
