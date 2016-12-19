unit TestGeocodingSamplesUnit;

interface

uses
  TestFramework, Classes, SysUtils,
  BaseTestOnlineExamplesUnit;

type
  TTestGeocodingSamples = class(TTestOnlineExamples)
  private
    procedure GetLimitedAddresses;
    procedure ForwardGeocodeAddress;
    procedure ReverseGeocodeAddress;
  published
    procedure GetSingleAddress;
    procedure GetAddresses;
  end;

implementation

uses NullableBasicTypesUnit, GeocodingUnit, DirectionPathPointUnit,
  GeocodingAddressUnit;

procedure TTestGeocodingSamples.ForwardGeocodeAddress;
var
  ErrorString: String;
  Address: String;
  Geocoding: TGeocoding;
begin
  Address := 'Los Angeles International Airport, CA';
  Geocoding := FRoute4MeManager.Geocoding.ForwardGeocodeAddress(Address, ErrorString);
  CheckNotNull(Geocoding);
  CheckEquals(EmptyStr, ErrorString);

  Address := 'qwsdwfwfwef2';
  Geocoding := FRoute4MeManager.Geocoding.ForwardGeocodeAddress(Address, ErrorString);
  CheckNull(Geocoding);
  CheckNotEquals(EmptyStr, ErrorString);
end;

procedure TTestGeocodingSamples.GetAddresses;
var
  ErrorString: String;
  Addresses: TGeocodingAddressList;
begin
  Addresses := FRoute4MeManager.Geocoding.GetAddresses(ErrorString);
  try
    CheckNotNull(Addresses);
    CheckTrue(Addresses.Count > 0);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Addresses);
  end;
end;

procedure TTestGeocodingSamples.GetLimitedAddresses;
var
  ErrorString: String;
  Addresses: TGeocodingAddressList;
  Limit, Offset: integer;
begin
  // todo: 404 ошибка в ответ с сервера
  Limit := 3;
  Offset := 0;
  Addresses := FRoute4MeManager.Geocoding.GetLimitedAddresses(Limit, Offset, ErrorString);
  try
    CheckNotNull(Addresses);
    CheckTrue(Addresses.Count > 0);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Addresses);
  end;
end;

procedure TTestGeocodingSamples.GetSingleAddress;
var
  Pk: integer;
  ErrorString: String;
  Address: TGeocodingAddress;
begin
  Pk := 4;
  Address := FRoute4MeManager.Geocoding.GetSingleAddress(Pk, ErrorString);
  try
    CheckNotNull(Address);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Address);
  end;

  Pk := -1;
  Address := FRoute4MeManager.Geocoding.GetSingleAddress(Pk, ErrorString);
  try
    CheckNull(Address);
    CheckNotEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Address);
  end;
end;

procedure TTestGeocodingSamples.ReverseGeocodeAddress;
var
  ErrorString: String;
  Location: TDirectionPathPoint;
  Geocoding: TGeocoding;
begin
  Location := TDirectionPathPoint.Create(42.35863, -71.05670);
  Geocoding := FRoute4MeManager.Geocoding.ReverseGeocodeAddress(Location, ErrorString);
  CheckNotNull(Geocoding);
  CheckEquals(EmptyStr, ErrorString);

  Location := TDirectionPathPoint.Create(123, 123);
  Geocoding := FRoute4MeManager.Geocoding.ReverseGeocodeAddress(Location, ErrorString);
  CheckNull(Geocoding);
  CheckNotEquals(EmptyStr, ErrorString);
end;

initialization
  RegisterTest('Examples\Online\Geocoding\', TTestGeocodingSamples.Suite);
end.
