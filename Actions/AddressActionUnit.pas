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

    procedure MarkAsVisited(RouteId: String; AddressId, MemberId: integer;
      IsVisited: boolean; out ErrorString: String);

    procedure MarkAsDeparted(RouteId: String; AddressId, MemberId: integer;
      IsDeparted: boolean; out ErrorString: String);

    procedure MarkAsDetectedAsVisited(RouteId: String; RouteDestinationId: integer;
      IsVisited: boolean; out ErrorString: String);

    procedure MarkAsDetectedAsDeparted(RouteId: String; RouteDestinationId: integer;
      IsDeparted: boolean; out ErrorString: String);
  end;

implementation

{ TAddressActions }

uses
  SettingsUnit, GenericParametersUnit, GetAddressUnit, StatusResponseUnit,
  MarkAddressAsDetectedAsVisitedRequestUnit,
  MarkAddressAsDetectedAsDepartedRequestUnit;

function TAddressActions.Get(AddressParameters: TAddressParameters;
  out ErrorString: String): TAddress;
begin
  Result := FConnection.Get(TSettings.GetAddress, AddressParameters,
    TAddress, ErrorString) as TAddress;
end;

procedure TAddressActions.MarkAsDeparted(RouteId: String;
  AddressId, MemberId: integer; IsDeparted: boolean; out ErrorString: String);
var
  Response: TStatusResponse;
  Request: TGenericParameters;
begin
  Request := TGenericParameters.Create;
  try
    Request.AddParameter('route_id', RouteId);
    Request.AddParameter('address_id', IntToStr(AddressId));
    Request.AddParameter('member_id', IntToStr(MemberId));
    if IsDeparted then
      Request.AddParameter('is_departed', '1')
    else
      Request.AddParameter('is_departed', '0');

    Response := FConnection.Get(TSettings.MarkAddressAsDeparted, Request,
      TStatusResponse, ErrorString) as TStatusResponse;
    try
      if (Response <> nil) and (Response.Status = False) and (ErrorString = EmptyStr) then
        ErrorString := 'Mark As Departed fault';
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

procedure TAddressActions.MarkAsDetectedAsDeparted(RouteId: String;
  RouteDestinationId: integer; IsDeparted: boolean; out ErrorString: String);
var
  Address: TAddress;
  Request: TMarkAddressAsDetectedAsDepartedRequest;
begin
  Request := TMarkAddressAsDetectedAsDepartedRequest.Create(
    RouteId, RouteDestinationId, IsDeparted);
  try
    Address := FConnection.Put(TSettings.Address, Request,
      TAddress, ErrorString) as TAddress;
    try
      if (Address = nil) and (ErrorString = EmptyStr) then
        ErrorString := 'Mark As Detected As Departed fault';
    finally
      FreeAndNil(Address);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

procedure TAddressActions.MarkAsDetectedAsVisited(RouteId: String;
  RouteDestinationId: integer; IsVisited: boolean; out ErrorString: String);
var
  Address: TAddress;
  Request: TMarkAddressAsDetectedAsVisitedRequest;
begin
  Request := TMarkAddressAsDetectedAsVisitedRequest.Create(
    RouteId, RouteDestinationId, IsVisited);
  try
    Address := FConnection.Put(TSettings.Address, Request,
      TAddress, ErrorString) as TAddress;
    try
      if (Address = nil) and (ErrorString = EmptyStr) then
        ErrorString := 'Mark As Detected As Visited fault';
    finally
      FreeAndNil(Address);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

procedure TAddressActions.MarkAsVisited(RouteId: String;
  AddressId, MemberId: integer; IsVisited: boolean; out ErrorString: String);
var
  Response: TStatusResponse;
  Request: TGenericParameters;
begin
  Request := TGenericParameters.Create;
  try
    Request.AddParameter('route_id', RouteId);
    Request.AddParameter('address_id', IntToStr(AddressId));
    Request.AddParameter('member_id', IntToStr(MemberId));
    if IsVisited then
      Request.AddParameter('is_visited', '1')
    else
      Request.AddParameter('is_visited', '0');

    Response := FConnection.Get(TSettings.MarkAddressAsVisited, Request,
      TStatusResponse, ErrorString) as TStatusResponse;
    try
      if (Response <> nil) and (Response.Status = False) and (ErrorString = EmptyStr) then
        ErrorString := 'Mark As Visited fault';
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

end.
