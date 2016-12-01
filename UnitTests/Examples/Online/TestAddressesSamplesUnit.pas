unit TestAddressesSamplesUnit;

interface

uses
  TestFramework, Classes, SysUtils,
  BaseTestOnlineExamplesUnit;

type
  TTestAddressesSamples = class(TTestOnlineExamples)
  private
    procedure GetAddress;
    procedure MarkAsVisited;
    procedure MarkAsDeparted;
    procedure MarkAsDetectedAsVisited;
    procedure MarkAsDetectedAsDeparted;
  published
  end;

implementation

uses AddressParametersUnit, AddressUnit;

var
  FRouteId: String;
  FAddressId: integer;
  FMemberId: integer;
  FRouteDestinationId: integer;

{ TTestAddressesSamples }

procedure TTestAddressesSamples.GetAddress;
var
  ErrorString: String;
  Parameters: TAddressParameters;
  Address: TAddress;
begin
  Parameters := TAddressParameters.Create;

  Address := FRoute4MeManager.Address.Get(Parameters, ErrorString);
  CheckEquals(EmptyStr, ErrorString);

  // todo: проинициализировать параметры, после этого заработают остальные тесты
  FRouteId := Address.RouteId;
  FMemberId := Address.MemberId;
  FRouteDestinationId := Address.RouteDestinationId;
//  FAddressId := Address.adRouteId;
end;

procedure TTestAddressesSamples.MarkAsDeparted;
var
  ErrorString: String;
  IsDeparted: boolean;
begin
  IsDeparted := True;
  FRoute4MeManager.Address.MarkAsDeparted(
    FRouteId, FAddressId, FMemberId, IsDeparted, ErrorString);
  CheckEquals(EmptyStr, ErrorString);

  FRoute4MeManager.Address.MarkAsDeparted(
    'qwe', FAddressId, FMemberId, IsDeparted, ErrorString);
  CheckNotEquals(EmptyStr, ErrorString);

  FRoute4MeManager.Address.MarkAsDeparted(
    FRouteId, -123, FMemberId, IsDeparted, ErrorString);
  CheckNotEquals(EmptyStr, ErrorString);

  FRoute4MeManager.Address.MarkAsDeparted(
    FRouteId, FAddressId, -123, IsDeparted, ErrorString);
  CheckNotEquals(EmptyStr, ErrorString);

  IsDeparted := False;
  FRoute4MeManager.Address.MarkAsDeparted(
    FRouteId, FAddressId, FMemberId, IsDeparted, ErrorString);
  CheckEquals(EmptyStr, ErrorString);
end;

procedure TTestAddressesSamples.MarkAsDetectedAsDeparted;
var
  ErrorString: String;
  IsDeparted: boolean;
begin
  IsDeparted := True;
  FRoute4MeManager.Address.MarkAsDetectedAsDeparted(
    FRouteId, FRouteDestinationId, IsDeparted, ErrorString);
  CheckEquals(EmptyStr, ErrorString);

  FRoute4MeManager.Address.MarkAsDetectedAsVisited(
    'qwe', FRouteDestinationId, IsDeparted, ErrorString);
  CheckNotEquals(EmptyStr, ErrorString);

  FRoute4MeManager.Address.MarkAsDetectedAsVisited(
    FRouteId, -123, IsDeparted, ErrorString);
  CheckNotEquals(EmptyStr, ErrorString);

  IsDeparted := False;
  FRoute4MeManager.Address.MarkAsDetectedAsVisited(
    FRouteId, FRouteDestinationId, IsDeparted, ErrorString);
  CheckEquals(EmptyStr, ErrorString);
end;

procedure TTestAddressesSamples.MarkAsDetectedAsVisited;
var
  ErrorString: String;
  IsVisited: boolean;
begin
  IsVisited := True;
  FRoute4MeManager.Address.MarkAsDetectedAsVisited(
    FRouteId, FRouteDestinationId, IsVisited, ErrorString);
  CheckEquals(EmptyStr, ErrorString);

  FRoute4MeManager.Address.MarkAsDetectedAsVisited(
    'qwe', FRouteDestinationId, IsVisited, ErrorString);
  CheckNotEquals(EmptyStr, ErrorString);

  FRoute4MeManager.Address.MarkAsDetectedAsVisited(
    FRouteId, -123, IsVisited, ErrorString);
  CheckNotEquals(EmptyStr, ErrorString);

  IsVisited := False;
  FRoute4MeManager.Address.MarkAsDetectedAsVisited(
    FRouteId, FRouteDestinationId, IsVisited, ErrorString);
  CheckEquals(EmptyStr, ErrorString);
end;

procedure TTestAddressesSamples.MarkAsVisited;
var
  ErrorString: String;
  IsVisited: boolean;
begin
  IsVisited := True;
  FRoute4MeManager.Address.MarkAsVisited(
    FRouteId, FAddressId, FMemberId, IsVisited, ErrorString);
  CheckEquals(EmptyStr, ErrorString);

  FRoute4MeManager.Address.MarkAsVisited(
    'qwe', FAddressId, FMemberId, IsVisited, ErrorString);
  CheckNotEquals(EmptyStr, ErrorString);

  FRoute4MeManager.Address.MarkAsVisited(
    FRouteId, -123, FMemberId, IsVisited, ErrorString);
  CheckNotEquals(EmptyStr, ErrorString);

  FRoute4MeManager.Address.MarkAsVisited(
    FRouteId, FAddressId, -123, IsVisited, ErrorString);
  CheckNotEquals(EmptyStr, ErrorString);

  IsVisited := False;
  FRoute4MeManager.Address.MarkAsVisited(
    FRouteId, FAddressId, FMemberId, IsVisited, ErrorString);
  CheckEquals(EmptyStr, ErrorString);
end;

initialization
  RegisterTest('Examples\Online\Addresses\', TTestAddressesSamples.Suite);
end.
