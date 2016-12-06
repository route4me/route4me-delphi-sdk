unit AddressBookContactActionsUnit;

interface

uses
  System.Generics.Collections, SysUtils,
  AddressBookContactUnit, AddressBookParametersUnit,
  IConnectionUnit, BaseActionUnit, SettingsUnit, CommonTypesUnit, EnumsUnit;

type

  TAddressBookContactActions = class(TBaseAction)
  private
  public
    /// <summary>
    ///  ADD a location to a user’s address book.
    /// </summary>
    function Add(Contact: TAddressBookContact; out ErrorString: String): TAddressBookContact;

    /// <summary>
    ///  UPDATE existing address book location parameters.
    /// </summary>
    function Update(Contact: TAddressBookContact; out ErrorString: String): TAddressBookContact;

    /// <summary>
    ///  REMOVE specified location from an address book.
    /// </summary>
    function Remove(AddressId: integer; out ErrorString: String): boolean; overload;

    /// <summary>
    ///  REMOVE locations from an address book.
    /// </summary>
    function Remove(AddressIds: TArray<integer>; out ErrorString: String): boolean; overload;

    /// <summary>
    ///  GET all locations from a user’s address book.
    /// </summary>
    function Get(Limit, Offset: integer; out Total: integer;
      out ErrorString: String): TAddressBookContactList; overload;

    /// <summary>
    ///  GET locations from an address book by a specified list of locations IDs.
    /// </summary>
    function Get(AddressesIds: TArray<integer>; out ErrorString: String): TAddressBookContactList; overload;

    /// <summary>
    ///  GET an address book location by containing specified text in any field.
    /// </summary>
    function Find(Query: String; Limit, Offset: integer; out Total: integer;
      out ErrorString: String): TAddressBookContactList; overload;

    /// <summary>
    ///  GET specified fields from an address book by containing specified text in any field.
    /// </summary>
    function Find(Query: String; Fields: TArray<String>; Limit, Offset: integer;
      out Total: integer; out ErrorString: String): T2DimensionalStringArray; overload;

    /// <summary>
    ///  Display locations included in the routes.
    /// </summary>
    function Find(DisplayLocations: TDisplayLocations; Limit, Offset: integer;
      out Total: integer; out ErrorString: String): TAddressBookContactList; overload;
  end;

implementation

uses RemoveAddressBookContactsRequestUnit, AddressBookContactFindResponseUnit,
  StatusResponseUnit, GetAddressBookContactsResponseUnit, GenericParametersUnit;

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

function TAddressBookContactActions.Get(Limit, Offset: integer;
  out Total: integer; out ErrorString: String): TAddressBookContactList;
var
  Response: TGetAddressBookContactsResponse;
  Request: TGenericParameters;
  i: integer;
begin
  Result := TAddressBookContactList.Create;

  Request := TGenericParameters.Create;
  try
    Request.AddParameter('limit', IntToStr(Limit));
    Request.AddParameter('offset', IntToStr(Offset));

    Response := FConnection.Get(TSettings.AddressBook, Request,
      TGetAddressBookContactsResponse, ErrorString) as TGetAddressBookContactsResponse;
    try
      if (Response <> nil) then
      begin
        for i := 0 to Length(Response.Results) - 1 do
          Result.Add(Response.Results[i]);
        Total := Response.Total;
      end;
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

function TAddressBookContactActions.Find(Query: String; Fields: TArray<String>;
  Limit, Offset: integer; out Total: integer;
  out ErrorString: String): T2DimensionalStringArray;
var
  Response: TAddressBookContactFindResponse;
  Request: TGenericParameters;
  i: integer;
  FieldsStr: String;
begin
  SetLength(Result, 0);

  Request := TGenericParameters.Create;
  try
    Request.AddParameter('limit', IntToStr(Limit));
    Request.AddParameter('offset', IntToStr(Offset));
    Request.AddParameter('query', Query);
    FieldsStr := EmptyStr;
    for i := 0 to Length(Fields) - 1 do
    begin
      if (i > 0) then
        FieldsStr := FieldsStr + ',';
      FieldsStr := FieldsStr + Fields[i];
    end;
    Request.AddParameter('fields', FieldsStr);

    Response := FConnection.Get(TSettings.AddressBook, Request,
      TAddressBookContactFindResponse, ErrorString) as TAddressBookContactFindResponse;
    try
      if (Response <> nil) then
      begin
        Result := Response.Results;
        Total := Response.Total;
      end;
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

function TAddressBookContactActions.Find(DisplayLocations: TDisplayLocations;
  Limit, Offset: integer; out Total: integer;
  out ErrorString: String): TAddressBookContactList;
var
  Response: TGetAddressBookContactsResponse;
  Request: TGenericParameters;
  i: integer;
begin
  Result := TAddressBookContactList.Create;

  Request := TGenericParameters.Create;
  try
    Request.AddParameter('limit', IntToStr(Limit));
    Request.AddParameter('offset', IntToStr(Offset));
    Request.AddParameter('display', TDisplayLocationsDescription[DisplayLocations]);

    Response := FConnection.Get(TSettings.AddressBook, Request,
      TGetAddressBookContactsResponse, ErrorString) as TGetAddressBookContactsResponse;
    try
      if (Response <> nil) then
      begin
        for i := 0 to Length(Response.Results) - 1 do
          Result.Add(Response.Results[i]);
        Total := Response.Total;
      end;
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

function TAddressBookContactActions.Get(AddressesIds: TArray<integer>;
  out ErrorString: String): TAddressBookContactList;
var
  Response: TGetAddressBookContactsResponse;
  Request: TGenericParameters;
  i: integer;
  Ids: String;
begin
  Result := TAddressBookContactList.Create;

  Request := TGenericParameters.Create;
  try
    Ids := EmptyStr;
    if Length(AddressesIds) = 1 then
      Ids := '1,' + IntToStr(AddressesIds[0])
    else
      for i := 0 to Length(AddressesIds) - 1 do
      begin
        if (i > 0) then
          Ids := Ids + ',';
        Ids := Ids + IntToStr(AddressesIds[i]);
      end;
    Request.AddParameter('address_id', Ids);

    Response := FConnection.Get(TSettings.AddressBook, Request,
      TGetAddressBookContactsResponse, ErrorString) as TGetAddressBookContactsResponse;
    try
      if (Response <> nil) then
        for i := 0 to Length(Response.Results) - 1 do
          Result.Add(Response.Results[i]);
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
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

function TAddressBookContactActions.Find(Query: String; Limit, Offset: integer;
  out Total: integer; out ErrorString: String): TAddressBookContactList;
var
  Response: TGetAddressBookContactsResponse;
  Request: TGenericParameters;
  i: integer;
begin
  Result := TAddressBookContactList.Create;

  Request := TGenericParameters.Create;
  try
    Request.AddParameter('limit', IntToStr(Limit));
    Request.AddParameter('offset', IntToStr(Offset));
    Request.AddParameter('query', Query);

    Response := FConnection.Get(TSettings.AddressBook, Request,
      TGetAddressBookContactsResponse, ErrorString) as TGetAddressBookContactsResponse;
    try
      if (Response <> nil) then
      begin
        for i := 0 to Length(Response.Results) - 1 do
          Result.Add(Response.Results[i]);
        Total := Response.Total;
      end;
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Request);
  end;
end;

end.
