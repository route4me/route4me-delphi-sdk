unit Route4MeManagerUnit;

interface

uses
  Classes, SysUtils,
  OptimizationParametersUnit, DataObjectUnit, IRoute4MeManagerUnit,
  AddressBookContactUnit, AddressBookContactActionsUnit, ConnectionUnit;

type
  TRoute4MeManager = class(TInterfacedObject, IRoute4MeManager)
  private
    FApiKey: String;
    FAddressBookContact: TAddressBookContactActions;
    FConnection: TConnection;
  public
    constructor Create(ApiKey: String);
    destructor Destroy; override;

    function RunOptimization(optimizationParameters: TOptimizationParameters; out errorString: String): TDataObject;

    function AddressBookContact: TAddressBookContactActions;
  end;

implementation

{ TRoute4MeManager }

uses EnumsUnit;

constructor TRoute4MeManager.Create(ApiKey: String);
begin
  FApiKey := ApiKey;
  FAddressBookContact := nil;
  FConnection := TConnection.Create;
end;

destructor TRoute4MeManager.Destroy;
begin
  if (FAddressBookContact <> nil) then
    FreeAndNil(FAddressBookContact);

  FConnection.Free;
  inherited;
end;

function TRoute4MeManager.AddressBookContact: TAddressBookContactActions;
begin
  if (FAddressBookContact = nil) then
    FAddressBookContact := TAddressBookContactActions.Create(FConnection);
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
