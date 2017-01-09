unit TestAddressNotesSamplesUnit;

interface

uses
  TestFramework, Classes, SysUtils,
  BaseTestOnlineExamplesUnit;

type
  TTestAddressNotesSamples = class(TTestOnlineExamples)
  private
    procedure GetAddress;
  published
    procedure MarkAsDetectedAsVisited;
    procedure MarkAsDetectedAsDeparted;
  end;

implementation

uses AddressParametersUnit, AddressUnit, NullableBasicTypesUnit;

var
  FRouteId: NullableString;
  FRouteDestinationId: NullableInteger;

procedure TTestAddressNotesSamples.GetAddress;
var
  ErrorString: String;
  Parameters: TAddressParameters;
  Address: TAddress;
begin
  Parameters := TAddressParameters.Create;

  Address := FRoute4MeManager.Address.Get(Parameters, ErrorString);
  CheckEquals(EmptyStr, ErrorString);

  FRouteId := Address.RouteId;
  FRouteDestinationId := Address.RouteDestinationId;
end;

procedure TTestAddressNotesSamples.MarkAsDetectedAsDeparted;
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

procedure TTestAddressNotesSamples.MarkAsDetectedAsVisited;
var
  ErrorString: String;
  IsVisited: boolean;
begin
  GetAddress;

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

initialization
  RegisterTest('Examples\Online\AddressNotes\', TTestAddressNotesSamples.Suite);
  FRouteId := NullableString.Null;
  FRouteDestinationId := NullableInteger.Null;
end.
