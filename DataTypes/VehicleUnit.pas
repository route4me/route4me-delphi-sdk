unit VehicleUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections, SysUtils,
  JSONNullableAttributeUnit,
  NullableBasicTypesUnit, GenericParametersUnit, EnumsUnit;

type
  /// <summary>
  ///  Vehicles in the user's account
  /// </summary>
  /// <remarks>
  ///  https://github.com/route4me/json-schemas/blob/master/Vehicle.dtd
  /// </remarks>
  TVehicle = class(TGenericParameters)
  private
    [JSONName('vehicle_id')]
    [Nullable]
    FId: NullableString;

    [JSONName('created_time')]
    [Nullable]
    FCreatedTime: NullableString;

    [JSONName('member_id')]
    [Nullable]
    FMemberId: NullableInteger;

    [JSONName('vehicle_alias')]
    [Nullable]
    FAlias: NullableString;

    [JSONName('vehicle_reg_state')]
    [Nullable]
    FRegState: NullableString;

    [JSONName('vehicle_reg_state_id')]
    [Nullable]
    FRegStateId: NullableString;

    [JSONName('vehicle_reg_country')]
    [Nullable]
    FRegCountry: NullableString;

    [JSONName('vehicle_reg_country_id')]
    [Nullable]
    FRegCountryId: NullableString;

    [JSONName('vehicle_license_plate')]
    [Nullable]
    FLicensePlate: NullableString;

    [JSONName('vehicle_make')]
    [Nullable]
    FMaker: NullableString;

    [JSONName('vehicle_model_year')]
    [Nullable]
    FModelYear: NullableString;

    [JSONName('vehicle_model')]
    [Nullable]
    FModel: NullableString;

    [JSONName('vehicle_year_acquired')]
    [Nullable]
    FYearAcquired: NullableString;

    [JSONName('vehicle_cost_new')]
    [Nullable]
    FCostNew: NullableDouble;

    [JSONName('license_start_date')]
    [Nullable]
    FLicenseStartDate: NullableString;

    [JSONName('license_end_date')]
    [Nullable]
    FLicenseEndDate: NullableString;

    [JSONName('vehicle_axle_count')]
    [Nullable]
    FAxleCount: NullableString;

    [JSONName('mpg_city')]
    [Nullable]
    FMpgCity: NullableString;

    [JSONName('mpg_highway')]
    [Nullable]
    FMpgHighway: NullableString;

    [JSONName('fuel_type')]
    [Nullable]
    FFuelType: NullableString;

    [JSONName('height_inches')]
    [Nullable]
    FHeightInches: NullableString;

    [JSONName('weight_lb')]
    [Nullable]
    FWeightLb: NullableString;
  public
    /// <remarks>
    ///  Constructor with 0-arguments must be and be public.
    ///  For JSON-deserialization.
    /// </remarks>
    constructor Create; overload; override;

    function Equals(Obj: TObject): Boolean; override;

    /// <summary>
    ///  A unique identifcation 32-char string of the vehicle
    /// </summary>
    property Id: NullableString read FId write FId;

    /// <summary>
    ///  Created time of the record about vehicle
    /// </summary>
    property CreatedTime: NullableString read FCreatedTime write FCreatedTime;

    /// <summary>
    ///  An unique identification number of the member
    /// </summary>
    property MemberId: NullableInteger read FMemberId write FMemberId;

    /// <summary>
    ///  Internal name of the vehicle
    /// </summary>
    property Alias: NullableString read FAlias write FAlias;

    /// <summary>
    ///  A state where vehicle was registered
    /// </summary>
    property RegistrationState: NullableString read FRegState write FRegState;

    /// <summary>
    ///  An ID of the state, where vehicle was registered
    /// </summary>
    property RegistrationStateId: NullableString read FRegStateId write FRegStateId;

    /// <summary>
    ///  A country where vehicle was registered
    /// </summary>
    property RegistrationCountry: NullableString read FRegCountry write FRegCountry;

    /// <summary>
    ///  An ID of the country, where vehicle was registered
    /// </summary>
    property RegistrationCountryId: NullableString read FRegCountryId write FRegCountryId;

    /// <summary>
    ///  A license plate of the vehicle
    /// </summary>
    property LicensePlate: NullableString read FLicensePlate write FLicensePlate;

    /// <summary>
    ///  Vehicle maker brend
    /// </summary>
    property Maker: NullableString read FMaker write FMaker;

    /// <summary>
    ///  A year of the vehicle model
    /// </summary>
    property ModelYear: NullableString read FModelYear write FModelYear;

    /// <summary>
    ///  A model of the vehicle
    /// </summary>
    property Model: NullableString read FModel write FModel;

    /// <summary>
    ///  A year the vehicle was acquired
    /// </summary>
    property YearAcquired: NullableString read FYearAcquired write FYearAcquired;

    /// <summary>
    ///  A cost of the new vehicle
    /// </summary>
    property CostNew: NullableDouble read FCostNew write FCostNew;

    /// <summary>
    ///  A start date of the license
    /// </summary>
    property LicenseStartDate: NullableString read FLicenseStartDate write FLicenseStartDate;

    /// <summary>
    ///  An end date of the license
    /// </summary>
    property LicenseEndDate: NullableString read FLicenseEndDate write FLicenseEndDate;

    /// <summary>
    ///  A number of the vehicle's axles
    /// </summary>
    property AxleCount: NullableString read FAxleCount write FAxleCount;

    /// <summary>
    ///  Miles per gallon in the city area
    /// </summary>
    property MpgCity: NullableString read FMpgCity write FMpgCity;

    /// <summary>
    ///  Miles per gallon in the highway area
    /// </summary>
    property MpgHighway: NullableString read FMpgHighway write FMpgHighway;

    /// <summary>
    ///  A type of the fuel
    /// </summary>
    property FuelType: NullableString read FFuelType write FFuelType;

    /// <summary>
    ///  A height of the vehicle in the inches
    /// </summary>
    property HeightInches: NullableString read FHeightInches write FHeightInches;

    /// <summary>
    ///  A weight of the vehicle in the pounds
    /// </summary>
    property WeightLb: NullableString read FWeightLb write FWeightLb;
  end;

  TVehicleList = TObjectList<TVehicle>;

implementation

{ TVehicle }

constructor TVehicle.Create;
begin
  Inherited Create;

  FId := NullableString.Null;
  FCreatedTime := NullableString.Null;
  FMemberId := NullableInteger.Null;
  FAlias := NullableString.Null;
  FRegState := NullableString.Null;
  FRegStateId := NullableString.Null;
  FRegCountry := NullableString.Null;
  FRegCountryId := NullableString.Null;
  FLicensePlate := NullableString.Null;
  FMaker := NullableString.Null;
  FModelYear := NullableString.Null;
  FModel := NullableString.Null;
  FYearAcquired := NullableString.Null;
  FCostNew := NullableDouble.Null;
  FLicenseStartDate := NullableString.Null;
  FLicenseEndDate := NullableString.Null;
  FAxleCount := NullableString.Null;
  FMpgCity := NullableString.Null;
  FMpgHighway := NullableString.Null;
  FFuelType := NullableString.Null;
  FHeightInches := NullableString.Null;
  FWeightLb := NullableString.Null;
end;

function TVehicle.Equals(Obj: TObject): Boolean;
var
  Other: TVehicle;
begin
  Result := False;

  if not (Obj is TVehicle) then
    Exit;

  Other := TVehicle(Obj);

  Result :=
    (FId = Other.FId) and
    (FCreatedTime = Other.FCreatedTime) and
    (FMemberId = Other.FMemberId) and
    (FAlias = Other.FAlias) and
    (FRegState = Other.FRegState) and
    (FRegStateId = Other.FRegStateId) and
    (FRegCountry = Other.FRegCountry) and
    (FRegCountryId = Other.FRegCountryId) and
    (FLicensePlate = Other.FLicensePlate) and
    (FMaker = Other.FMaker) and
    (FModelYear = Other.FModelYear) and
    (FModel = Other.FModel) and
    (FYearAcquired = Other.FYearAcquired) and
    (FCostNew = Other.FCostNew) and
    (FLicenseStartDate = Other.FLicenseStartDate) and
    (FLicenseEndDate = Other.FLicenseEndDate) and
    (FAxleCount = Other.FAxleCount) and
    (FMpgCity = Other.FMpgCity) and
    (FMpgHighway = Other.FMpgHighway) and
    (FFuelType = Other.FFuelType) and
    (FHeightInches = Other.FHeightInches) and
    (FWeightLb = Other.FWeightLb);
end;

end.
