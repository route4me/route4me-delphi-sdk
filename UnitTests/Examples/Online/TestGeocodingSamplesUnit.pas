unit TestGeocodingSamplesUnit;

interface

uses
  TestFramework, Classes, SysUtils,
  BaseTestOnlineExamplesUnit;

type
  TTestGeocodingSamples = class(TTestOnlineExamples)
  private
  published
    procedure ForwardGeocodeAddress;
    procedure ReverseGeocodeAddress;
    procedure GetSingleAddress;
    procedure GetAddresses;
    procedure GetZipCodes;
    procedure GetLimitedAddresses;
    procedure GetLimitedZipCodes;
    procedure GetZipCodeAndHouseNumber;
    procedure GetLimitedZipCodeAndHouseNumber;
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
  try
    CheckNotNull(Geocoding);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Geocoding);
  end;

  Address := 'qwsdwfwfwef2';
  Geocoding := FRoute4MeManager.Geocoding.ForwardGeocodeAddress(Address, ErrorString);
  try
    CheckNull(Geocoding);
    CheckNotEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Geocoding);
  end;
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
  Limit := 20;
  Offset := 1;
  Addresses := FRoute4MeManager.Geocoding.GetAddresses(Limit, Offset, ErrorString);
  try
    CheckNotNull(Addresses);
    CheckTrue(Addresses.Count > 0);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Addresses);
  end;
end;

procedure TTestGeocodingSamples.GetLimitedZipCodeAndHouseNumber;
var
  ErrorString: String;
  Addresses: TGeocodingAddressList;
  Limit, Offset: integer;
  ZipCode, HouseNumber: String;
begin
  Limit := 3;
  Offset := 1;
  ZipCode := '00601';
  HouseNumber := '17';

  Addresses := FRoute4MeManager.Geocoding.GetZipCodeAndHouseNumber(
    ZipCode, HouseNumber, Limit, Offset, ErrorString);
  try
    CheckNotNull(Addresses);
    CheckTrue(Addresses.Count > 0);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Addresses);
  end;
end;

procedure TTestGeocodingSamples.GetLimitedZipCodes;
var
  ErrorString: String;
  Addresses: TGeocodingAddressList;
  Limit, Offset: integer;
  ZipCode: String;
begin
  Limit := 3;
  Offset := 1;
  ZipCode := '00601';
  Addresses := FRoute4MeManager.Geocoding.GetZipCodes(ZipCode, Limit, Offset, ErrorString);
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

procedure TTestGeocodingSamples.GetZipCodeAndHouseNumber;
var
  ErrorString: String;
  Addresses: TGeocodingAddressList;
  ZipCode, HouseNumber: String;
begin
  ZipCode := '00601';
  HouseNumber := '17';

  Addresses := FRoute4MeManager.Geocoding.GetZipCodeAndHouseNumber(
    ZipCode, HouseNumber, ErrorString);
  try
    CheckNotNull(Addresses);
    CheckTrue(Addresses.Count > 0);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Addresses);
  end;
end;

procedure TTestGeocodingSamples.GetZipCodes;
var
  ErrorString: String;
  Addresses: TGeocodingAddressList;
  ZipCode: String;
begin
  ZipCode := '00601';
  Addresses := FRoute4MeManager.Geocoding.GetZipCodes(ZipCode, ErrorString);
  try
    CheckNotNull(Addresses);
    CheckTrue(Addresses.Count > 0);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Addresses);
  end;
end;

procedure TTestGeocodingSamples.ReverseGeocodeAddress;
var
  ErrorString: String;
  Location: TDirectionPathPoint;
  Geocoding: TGeocodingList;
begin
  Location := TDirectionPathPoint.Create(42.35863, -71.05670);
  Geocoding := FRoute4MeManager.Geocoding.ReverseGeocodeAddress(Location, ErrorString);
  try
    CheckNotNull(Geocoding);
    CheckTrue(Geocoding.Count > 0);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Geocoding);
  end;

  Location := TDirectionPathPoint.Create(123456789, 1234564789);
  try
    Geocoding := FRoute4MeManager.Geocoding.ReverseGeocodeAddress(Location, ErrorString);
    CheckNotNull(Geocoding);
    CheckEquals(0, Geocoding.Count);
    CheckEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Geocoding);
  end;
end;

initialization
  RegisterTest('Examples\Online\Geocoding\', TTestGeocodingSamples.Suite);
end.
