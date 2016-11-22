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

    procedure MarkAsVisited(RouteId: String; RouteDestinationId: integer;
      IsVisited: boolean; out ErrorString: String);

    procedure MarkAsDeparted(RouteId: String; RouteDestinationId: integer;
      IsDeparted: boolean; out ErrorString: String);
  end;

implementation

{ TAddressActions }

uses
  SettingsUnit, GenericParametersUnit, GetAddressUnit,
  MarkAddressAsVisitedRequestUnit, MarkAddressAsDepartedRequestUnit;

function TAddressActions.Get(AddressParameters: TAddressParameters;
  out ErrorString: String): TAddress;
begin
  Result := FConnection.Get(TSettings.GetAddress, AddressParameters,
    TAddress, ErrorString) as TAddress;
end;

procedure TAddressActions.MarkAsDeparted(RouteId: String;
  RouteDestinationId: integer; IsDeparted: boolean; out ErrorString: String);
var
  Address: TAddress;
  Request: TMarkAddressAsDepartedRequest;
begin
  Request := TMarkAddressAsDepartedRequest.Create(
    RouteId, RouteDestinationId, IsDeparted);
  try
    Address := FConnection.Put(TSettings.Address, Request,
      TAddress, ErrorString) as TAddress;
    try
      if (Address = nil) and (ErrorString = EmptyStr) then
        ErrorString := 'Mark As Departed fault';
    finally
      FreeAndNil(Address);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

procedure TAddressActions.MarkAsVisited(RouteId: String;
  RouteDestinationId: integer; IsVisited: boolean; out ErrorString: String);
var
  Address: TAddress;
  Request: TMarkAddressAsVisitedRequest;
begin
  Request := TMarkAddressAsVisitedRequest.Create(
    RouteId, RouteDestinationId, IsVisited);
  try
    Address := FConnection.Put(TSettings.Address, Request,
      TAddress, ErrorString) as TAddress;
    try
      if (Address = nil) and (ErrorString = EmptyStr) then
        ErrorString := 'Mark As Visited fault';
    finally
      FreeAndNil(Address);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

end.
