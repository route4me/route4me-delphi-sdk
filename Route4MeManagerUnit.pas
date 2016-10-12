unit Route4MeManagerUnit;

interface

uses
  Classes, SysUtils,
  OptimizationParametersUnit, DataObjectUnit, IRoute4MeManagerUnit,
  AddressBookContactUnit;

type
  TRoute4MeManager = class(TInterfacedObject, IRoute4MeManager)
  private
    FApiKey: String;
    FAddressBookContact: TAddressBookContact;

    function GetAddressBookContact: TAddressBookContact;
  public
    constructor Create(ApiKey: String);
    destructor Destroy; override;

    function RunOptimization(optimizationParameters: TOptimizationParameters; out errorString: String): TDataObject;

    property AddressBookContact: TAddressBookContact read GetAddressBookContact;
  end;

implementation

{ TRoute4MeManager }

uses EnumsUnit, R4MeInfrastructureSettingsUnit;

constructor TRoute4MeManager.Create(ApiKey: String);
begin
  FApiKey := ApiKey;
  FAddressBookContact := nil;
end;

destructor TRoute4MeManager.Destroy;
begin
  if (FAddressBookContact <> nil) then
    FreeAndNil(FAddressBookContact);

  inherited;
end;

function TRoute4MeManager.GetAddressBookContact: TAddressBookContact;
begin
  if (FAddressBookContact = nil) then
    FAddressBookContact := TAddressBookContact.Create;
  Result := FAddressBookContact;
end;

function TRoute4MeManager.RunOptimization(
  OptimizationParameters: TOptimizationParameters;
  out ErrorString: String): TDataObject;
begin
{    Result := GetJsonObjectFromAPI<TDataObject>(
      optimizationParameters, TR4MeInfrastructureSettings.ApiHost,
      HttpMethodType.Post, False, ErrorString);}
end;

end.
