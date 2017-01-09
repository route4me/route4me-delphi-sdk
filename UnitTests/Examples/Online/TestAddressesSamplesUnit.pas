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

uses AddressParametersUnit, AddressUnit, NullableBasicTypesUnit, AddressBookContactUnit, DataObjectUnit;

var
  FRouteId: NullableString;
  FAddressId: NullableInteger;
  FMemberId: NullableInteger;
  FRouteDestinationId: NullableInteger;

{ TTestAddressesSamples }

procedure TTestAddressesSamples.GetAddress;
{var
  ErrorString: String;
  Parameters: TAddressParameters;
  Address: TAddress;
  Limit, Offset, Total: Integer;
  Routes: TDataObjectRouteList;}
begin
      // todo 3: проинициализировать параметры, после этого заработают остальные тесты

{  Limit := 1;
  Offset := 0;
  Routes := FRoute4MeManager.Route.GetList(Limit, Offset, ErrorString);
  try
    CheckNotNull(Routes);
    CheckEquals(EmptyStr, ErrorString);
    CheckEquals(1, Routes.Count);

    FRouteId := Routes[0].RouteId;
    FMemberId := Routes[0].MemberId;

    CheckTrue(Length(Routes[0].Addresses) > 0);
    FAddressId := Routes[0].Addresses[0].id;
  finally
    FreeAndNil(Routes);
  end;

  Parameters := TAddressParameters.Create;
  try
    Address := FRoute4MeManager.Address.Get(Parameters, ErrorString);
    try
      CheckEquals(EmptyStr, ErrorString);

      FRouteId := Address.RouteId;
      FMemberId := Address.MemberId;
      FRouteDestinationId := Address.RouteDestinationId;
    finally
      FreeAndNil(Address);
    end;
  finally
    FreeAndNil(Parameters);
  end;}
end;

procedure TTestAddressesSamples.MarkAsDeparted;
var
  ErrorString: String;
  IsDeparted: boolean;
begin
  CheckTrue(FRouteId.IsNotNull);
  CheckTrue(FAddressId.IsNotNull);
  CheckTrue(FMemberId.IsNotNull);

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
  CheckTrue(FRouteId.IsNotNull);
  CheckTrue(FRouteDestinationId.IsNotNull);

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
  CheckTrue(FRouteId.IsNotNull);
  CheckTrue(FRouteDestinationId.IsNotNull);

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
  CheckTrue(FRouteId.IsNotNull);
  CheckTrue(FAddressId.IsNotNull);
  CheckTrue(FMemberId.IsNotNull);

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
  FRouteId := NullableString.Null;
  FAddressId := NullableInteger.Null;
  FMemberId := NullableInteger.Null;
  FRouteDestinationId := NullableInteger.Null;
end.
