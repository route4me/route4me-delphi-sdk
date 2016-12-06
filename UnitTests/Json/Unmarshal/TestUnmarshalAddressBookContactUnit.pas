unit TestUnmarshalAddressBookContactUnit;

interface

uses
  TestFramework, SysUtils,
  TestBaseJsonUnmarshalUnit,
  System.Generics.Collections,
  IAddressBookContactProviderUnit;

type
  TTestUnmarshalAddressBookContact = class(TTestBaseJsonUnmarshal)
  private
    procedure CheckEquals(Etalon: IAddressBookContactProvider; TestName: String); overload;
  published
    procedure DefaultAddressBookContact();
    procedure FullAddressBookContact();
    procedure RealAddressBookContact();

    procedure AddressBookContactFindResponse;
  end;

implementation

{ TTestOptimizationParametersToJson }

uses
  Classes, System.JSON,
  DefaultAddressBookContactProviderUnit, FullAddressBookContactProviderUnit,
  AddressBookContactUnit, MarshalUnMarshalUnit,
  RealAddressBookContactProviderUnit, AddressBookContactFindResponseUnit;

procedure TTestUnmarshalAddressBookContact.AddressBookContactFindResponse;
const
  s = '{"results":[[6601883,"Some address3916589616287113937","John6129484611666145821"],' +
    '[7681272,"100 Ogle St, Johnstown, PA 15905, USA",""]],"total":142,"fields":["address_id","first_name","address_1"]}';
var
  Response: TAddressBookContactFindResponse;
  JsonValue: TJSONValue;
begin
  JsonValue := TJSONObject.ParseJSONValue(s);
  try
    Response := TMarshalUnMarshal.FromJson(
      TAddressBookContactFindResponse, JsonValue) as TAddressBookContactFindResponse;
    try
      CheckNotNull(Response);
      CheckEquals(142, Response.Total);

      CheckEquals(3, Length(Response.Fields));
      CheckEquals('address_id', Response.Fields[0]);
      CheckEquals('first_name', Response.Fields[1]);
      CheckEquals('address_1', Response.Fields[2]);

      CheckEquals(2, Length(Response.Results));

      CheckEquals(3, Length(Response.Results[0]));
      CheckEquals('6601883', Response.Results[0][0]);
      CheckEquals('Some address3916589616287113937', Response.Results[0][1]);
      CheckEquals('John6129484611666145821', Response.Results[0][2]);

      CheckEquals(3, Length(Response.Results[1]));
      CheckEquals('7681272', Response.Results[1][0]);
      CheckEquals('100 Ogle St, Johnstown, PA 15905, USA', Response.Results[1][1]);
      CheckEquals('', Response.Results[1][2]);
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(JsonValue);
  end;
end;

procedure TTestUnmarshalAddressBookContact.CheckEquals(
  Etalon: IAddressBookContactProvider; TestName: String);
var
  ActualList: TStringList;
  Actual: TAddressBookContact;
  JsonFilename: String;
  AddressBookContact: TAddressBookContact;
  JsonValue: TJSONValue;
begin
  JsonFilename := EtalonFilename(TestName);
  ActualList := TStringList.Create;
  try
    ActualList.LoadFromFile(JsonFilename);

    JsonValue := TJSONObject.ParseJSONValue(ActualList.Text);
    try
      AddressBookContact := Etalon.AddressBookContact;
      Actual := TMarshalUnMarshal.FromJson(
        AddressBookContact.ClassType, JsonValue) as TAddressBookContact;
      try
        CheckTrue(AddressBookContact.Equals(Actual));
      finally
        FreeAndNil(Actual);
        FreeAndNil(AddressBookContact);
      end;
    finally
      FreeAndNil(JsonValue);
    end;
  finally
    FreeAndNil(ActualList);
  end;
  Etalon := nil;
end;

procedure TTestUnmarshalAddressBookContact.DefaultAddressBookContact;
begin
  CheckEquals(
    TDefaultAddressBookContactProvider.Create,
    'AddressBookContactToJson\DefaultAddressBookContact');
end;

procedure TTestUnmarshalAddressBookContact.FullAddressBookContact;
begin
  CheckEquals(
    TFullAddressBookContactProvider.Create,
    'AddressBookContactToJson\FullAddressBookContact');
end;

procedure TTestUnmarshalAddressBookContact.RealAddressBookContact;
begin
  CheckEquals(
    TRealAddressBookContactProvider.Create,
    'AddressBookContactToJson\RealAddressBookContact');
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('JSON\Unmarshal\', TTestUnmarshalAddressBookContact.Suite);
end.
