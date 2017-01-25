unit TestAddressesSamplesUnit;

interface

uses
  TestFramework, Classes, SysUtils,
  BaseTestOnlineExamplesUnit;

type
  TTestAddressesSamples = class(TTestOnlineExamples)
  published
    procedure MarkAsVisited;
    procedure MarkAsDeparted;
    procedure MarkAsDetectedAsVisited;
    procedure MarkAsDetectedAsDeparted;
  end;

implementation

uses AddressParametersUnit, AddressUnit, NullableBasicTypesUnit, AddressBookContactUnit, DataObjectUnit;

{ TTestAddressesSamples }

procedure TTestAddressesSamples.MarkAsDeparted;
var
  ErrorString: String;
  IsDeparted: boolean;
  RouteId: String;
  AddressId: integer;
  MemberId: integer;
begin
  RouteId := 'DD376C7148E7FEE36CFABE2BD9978BDD';
  MemberId := 1;
  AddressId := 183045808;

  IsDeparted := True;
  FRoute4MeManager.Address.MarkAsDeparted(
    RouteId, AddressId, MemberId, IsDeparted, ErrorString);
  CheckEquals(EmptyStr, ErrorString);

  FRoute4MeManager.Address.MarkAsDeparted(
    'qwe', AddressId, MemberId, IsDeparted, ErrorString);
  CheckNotEquals(EmptyStr, ErrorString);

  FRoute4MeManager.Address.MarkAsDeparted(
    RouteId, -123, MemberId, IsDeparted, ErrorString);
  CheckNotEquals(EmptyStr, ErrorString);

  FRoute4MeManager.Address.MarkAsDeparted(
    RouteId, AddressId, -123741, IsDeparted, ErrorString);
  // I don't know why this case is not considered an error
  CheckEquals(EmptyStr, ErrorString);

  IsDeparted := False;
  FRoute4MeManager.Address.MarkAsDeparted(
    RouteId, AddressId, MemberId, IsDeparted, ErrorString);
  CheckEquals(EmptyStr, ErrorString);
end;

procedure TTestAddressesSamples.MarkAsDetectedAsDeparted;
var
  ErrorString: String;
  IsDeparted: boolean;
  RouteId: String;
  RouteDestinationId: integer;
begin
  RouteId := '241466F15515D67D3F951E2DA38DE76D';
  RouteDestinationId := 167899269;

  IsDeparted := True;
  FRoute4MeManager.Address.MarkAsDetectedAsDeparted(
    RouteId, RouteDestinationId, IsDeparted, ErrorString);
  CheckEquals(EmptyStr, ErrorString);

  FRoute4MeManager.Address.MarkAsDetectedAsVisited(
    'qwe', RouteDestinationId, IsDeparted, ErrorString);
  CheckNotEquals(EmptyStr, ErrorString);

  FRoute4MeManager.Address.MarkAsDetectedAsVisited(
    RouteId, -123, IsDeparted, ErrorString);
  CheckNotEquals(EmptyStr, ErrorString);

  IsDeparted := False;
  FRoute4MeManager.Address.MarkAsDetectedAsVisited(
    RouteId, RouteDestinationId, IsDeparted, ErrorString);
  CheckEquals(EmptyStr, ErrorString);
end;

procedure TTestAddressesSamples.MarkAsDetectedAsVisited;
var
  ErrorString: String;
  IsVisited: boolean;
  RouteId: String;
  RouteDestinationId: integer;
begin
  RouteId := '241466F15515D67D3F951E2DA38DE76D';
  RouteDestinationId := 167899269;

  IsVisited := True;
  FRoute4MeManager.Address.MarkAsDetectedAsVisited(
    RouteId, RouteDestinationId, IsVisited, ErrorString);
  CheckEquals(EmptyStr, ErrorString);

  FRoute4MeManager.Address.MarkAsDetectedAsVisited(
    'qwe', RouteDestinationId, IsVisited, ErrorString);
  CheckNotEquals(EmptyStr, ErrorString);

  FRoute4MeManager.Address.MarkAsDetectedAsVisited(
    RouteId, -123, IsVisited, ErrorString);
  CheckNotEquals(EmptyStr, ErrorString);

  IsVisited := False;
  FRoute4MeManager.Address.MarkAsDetectedAsVisited(
    RouteId, RouteDestinationId, IsVisited, ErrorString);
  CheckEquals(EmptyStr, ErrorString);
end;

procedure TTestAddressesSamples.MarkAsVisited;
var
  ErrorString: String;
  IsVisited: boolean;
  RouteId: String;
  AddressId: integer;
  MemberId: integer;
  IsException: boolean;
begin
  RouteId := 'DD376C7148E7FEE36CFABE2BD9978BDD';
  MemberId := 1;
  AddressId := 183045808;

  IsVisited := True;
  FRoute4MeManager.Address.MarkAsVisited(
    RouteId, AddressId, MemberId, IsVisited, ErrorString);
  CheckEquals(EmptyStr, ErrorString);

  FRoute4MeManager.Address.MarkAsVisited(
    'qwe', AddressId, MemberId, IsVisited, ErrorString);
  CheckNotEquals(EmptyStr, ErrorString);

  FRoute4MeManager.Address.MarkAsVisited(
    RouteId, -123, MemberId, IsVisited, ErrorString);
  CheckNotEquals(EmptyStr, ErrorString);

  // I don't know why it happens exception
  IsException := False;
  try
    FRoute4MeManager.Address.MarkAsVisited(
      RouteId, AddressId, -123, IsVisited, ErrorString);
    CheckNotEquals(EmptyStr, ErrorString);
  except
    IsException := True;
  end;
  CheckTrue(IsException);

  IsVisited := False;
  FRoute4MeManager.Address.MarkAsVisited(
    RouteId, AddressId, MemberId, IsVisited, ErrorString);
  CheckEquals(EmptyStr, ErrorString);
end;

initialization
  RegisterTest('Examples\Online\Addresses\', TTestAddressesSamples.Suite);
end.
