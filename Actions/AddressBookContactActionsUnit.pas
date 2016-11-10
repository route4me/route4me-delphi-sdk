unit AddressBookContactActionsUnit;

interface

uses
  System.Generics.Collections, SysUtils,
  AddressBookContactUnit, AddressBookParametersUnit,
  IConnectionUnit, BaseActionUnit, SettingsUnit, CommonTypesUnit;

type

  TAddressBookContactActions = class(TBaseAction)
  private
  public
    function Add(Contact: TAddressBookContact; out ErrorString: String): TAddressBookContact;
    function Update(Contact: TAddressBookContact; out ErrorString: String): TAddressBookContact;
    function Remove(AddressId: integer; out ErrorString: String): boolean; overload;
    function Remove(AddressIds: TArray<integer>; out ErrorString: String): boolean; overload;
    function Get(AddressBookParameters: TAddressBookParameters;
      out Total: integer; out ErrorString: String): TAddressBookContactArray;
  end;

implementation

{ TAddressBookContact }

uses RemoveAddressBookContactsRequestUnit,
  StatusResponseUnit, GetAddressBookContactsResponseUnit;

function TAddressBookContactActions.Remove(AddressId: integer;
  out ErrorString: String): boolean;
begin
  Result := Remove([AddressId], ErrorString);
end;

function TAddressBookContactActions.Add(Contact: TAddressBookContact;
  out ErrorString: String): TAddressBookContact;
begin
  Result := FConnection.Post(TSettings.AddressBook, Contact,
    TAddressBookContact, ErrorString) as TAddressBookContact;
end;

function TAddressBookContactActions.Get(
  AddressBookParameters: TAddressBookParameters; out Total: integer;
  out ErrorString: String): TAddressBookContactArray;
var
  Response: TGetAddressBookContactsResponse;
begin
  SetLength(Result, 0);

  Response := FConnection.Get(TSettings.AddressBook, AddressBookParameters,
    TGetAddressBookContactsResponse, ErrorString) as TGetAddressBookContactsResponse;
  try
    if (Response <> nil) then
    begin
      Result := Response.Results;
      Total := Response.Total;
    end;
  finally
    FreeAndNil(Response);
  end;
end;

function TAddressBookContactActions.Remove(AddressIds: TArray<integer>;
  out ErrorString: String): boolean;
var
  Request: TRemoveAddressBookContactsRequest;
  Response: TStatusResponse;
begin
  Request := TRemoveAddressBookContactsRequest.Create();
  try
    Request.AddressIds := AddressIds;

    Response := FConnection.Delete(TSettings.AddressBook, Request,
      TStatusResponse, ErrorString) as TStatusResponse;
    try
      Result := (Response <> nil) and (Response.Status);
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

function TAddressBookContactActions.Update(
  Contact: TAddressBookContact; out ErrorString: String): TAddressBookContact;
begin
  Result := FConnection.Put(TSettings.AddressBook, Contact,
    TAddressBookContact, ErrorString) as TAddressBookContact;
end;

end.
