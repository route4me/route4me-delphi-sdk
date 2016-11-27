unit UserParameterProviderUnit;

interface

uses SysUtils, UserParametersUnit;

type
  IUserParameterProvider = interface
    ['{7B95CC9E-20C4-446E-BCC8-544D45AAA155}']
    function GetParameters(Email: String): TUserParameters;
  end;

  TUserParameterProvider = class(TInterfacedObject, IUserParameterProvider)
  public
    function GetParameters(Email: String): TUserParameters;
  end;

implementation

uses EnumsUnit;

function TUserParameterProvider.GetParameters(Email: String): TUserParameters;
begin
  Randomize;

  Result := TUserParameters.Create;
  Result.HideRoutedAddresses := False;
  Result.PhoneNumber := '571-259-5939';
  Result.Zip := '22102';
  Result.Email := Email;
  Result.HideVisitedAddresses := False;
  Result.ReadonlyUser := False;
  Result.MemberType := TMemberType.mtSubAccountDispatcher;
  Result.DateOfBirth := '2010';
  Result.FirstName := 'Clay';
  Result.LastName := 'Abraham';
  Result.Password := '123456';
  Result.HideNonFutureRoutes := False;
  Result.ShowAllVehicles := False;
  Result.ShowAllDrivers := False;
end;

end.
