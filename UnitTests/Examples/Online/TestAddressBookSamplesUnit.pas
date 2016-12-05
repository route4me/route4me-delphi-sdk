unit TestAddressBookSamplesUnit;

interface

uses
  TestFramework, Classes, SysUtils,
  BaseTestOnlineExamplesUnit;

type
  TTestAddressBookSamples = class(TTestOnlineExamples)
  private
    procedure LocationSearch;
  published
    procedure CreateLocation;
    procedure GetLocations;
    procedure GetLocation;
    procedure GetLocationsByIds;
    procedure DisplayRouted;
    procedure UpdateLocation;
    procedure RemoveLocations;
  end;

implementation

uses AddressBookContactUnit, EnumsUnit, NullableBasicTypesUnit;

var
  FContact: TAddressBookContact;
  FContactIdForDelete: integer;

procedure TTestAddressBookSamples.CreateLocation;
var
  ErrorString: String;
  Contact, NewContact: TAddressBookContact;
begin
  Contact := TAddressBookContact.Create;
  try
    Contact.Address := '3634 W Market St, Fairlawn, OH 44333';
    Contact.FirstName := 'John';
    Contact.LastName := 'Smith';
    Contact.Latitude := 41.135762259364;
    Contact.Longitude := -81.629313826561;

    FContact := FRoute4MeManager.AddressBookContact.Add(Contact, ErrorString);

    CheckNotNull(FContact);
    CheckEquals(EmptyStr, ErrorString);
    CheckTrue(FContact.Id.IsNotNull);
  finally
    FreeAndNil(Contact);
  end;

  Contact := TAddressBookContact.Create;
  try
    Contact.Address := EmptyStr;
    Contact.Latitude := 41.135762259364;
    Contact.Longitude := -81.629313826561;

    NewContact := FRoute4MeManager.AddressBookContact.Add(Contact, ErrorString);
    try
      CheckNotNull(NewContact);
      CheckEquals(EmptyStr, ErrorString);
      CheckTrue(NewContact.Id.IsNotNull);
      FContactIdForDelete := NewContact.Id;
    finally
      FreeAndNil(NewContact)
    end;
  finally
    FreeAndNil(Contact);
  end;
end;

procedure TTestAddressBookSamples.DisplayRouted;
var
  ErrorString: String;
  Contacts: TAddressBookContactList;
  Limit, Offset, Total: integer;
begin
  Limit := 2;
  Offset := 0;

  // todo: посмотреть что это за параметра такой
  Contacts := FRoute4MeManager.AddressBookContact.Find(
    TDisplayLocations.dlAll, Limit, Offset, Total, ErrorString);
  try
    CheckNotNull(Contacts);
    CheckEquals(EmptyStr, ErrorString);
    CheckTrue(Total > 0);
    CheckTrue(Contacts.Count > 0);
  finally
    FreeAndNil(Contacts)
  end;

  // todo: посмотреть что это за параметра такой
  Contacts := FRoute4MeManager.AddressBookContact.Find(
    TDisplayLocations.dlRouted, Limit, Offset, Total, ErrorString);
  try
    CheckNotNull(Contacts);
    CheckEquals(EmptyStr, ErrorString);
    CheckTrue(Total > 0);
    CheckTrue(Contacts.Count > 0);
  finally
    FreeAndNil(Contacts)
  end;

  // todo: посмотреть что это за параметра такой
  Contacts := FRoute4MeManager.AddressBookContact.Find(
    TDisplayLocations.dlUnrouted, Limit, Offset, Total, ErrorString);
  try
    CheckNotNull(Contacts);
    CheckEquals(EmptyStr, ErrorString);
    CheckTrue(Total > 0);
    CheckTrue(Contacts.Count > 0);
  finally
    FreeAndNil(Contacts)
  end;
end;

procedure TTestAddressBookSamples.GetLocation;
var
  ErrorString: String;
  Contacts: TAddressBookContactList;
  Limit, Offset, Total: integer;
  Query: String;
begin
  Limit := 2;
  Offset := 0;

  Query := 'john';
  Contacts := FRoute4MeManager.AddressBookContact.Find(Query, Limit, Offset, Total, ErrorString);
  try
    CheckNotNull(Contacts);
    CheckEquals(EmptyStr, ErrorString);
    CheckTrue(Total > 0);
    CheckTrue(Contacts.Count > 0);
  finally
    FreeAndNil(Contacts)
  end;

  Query := 'Fairlawn';
  Contacts := FRoute4MeManager.AddressBookContact.Find(Query, Limit, Offset, Total, ErrorString);
  try
    CheckNotNull(Contacts);
    CheckEquals(EmptyStr, ErrorString);
    CheckTrue(Total > 0);
    CheckTrue(Contacts.Count > 0);
  finally
    FreeAndNil(Contacts)
  end;

  Query := 'uniqueText#$2';
  Contacts := FRoute4MeManager.AddressBookContact.Find(Query, Limit, Offset, Total, ErrorString);
  try
    CheckNotNull(Contacts);
    CheckEquals(EmptyStr, ErrorString);
    CheckEquals(0, Total);
    CheckEquals(0, Contacts.Count);
  finally
    FreeAndNil(Contacts)
  end;
end;

procedure TTestAddressBookSamples.GetLocations;
var
  ErrorString: String;
  Contacts: TAddressBookContactList;
  Limit, Offset, Total: integer;
begin
  Limit := 2;
  Offset := 0;
  Contacts := FRoute4MeManager.AddressBookContact.Get(Limit, Offset, Total, ErrorString);
  try
    CheckNotNull(Contacts);
    CheckEquals(EmptyStr, ErrorString);
    CheckTrue(Total > 0);
    CheckTrue(Contacts.Count > 0);

    FContact := Contacts[0];
    Contacts.OwnsObjects := False;
    Contacts.Remove(FContact);
    Contacts.OwnsObjects := True;
  finally
    FreeAndNil(Contacts)
  end;
end;

procedure TTestAddressBookSamples.GetLocationsByIds;
var
  ErrorString: String;
  Contacts: TAddressBookContactList;
begin
  Contacts := FRoute4MeManager.AddressBookContact.Get([-123], ErrorString);
  try
    CheckNotNull(Contacts);
    CheckEquals(EmptyStr, ErrorString);
//    CheckNotEquals(EmptyStr, ErrorString);
    CheckEquals(0, Contacts.Count);
  finally
    FreeAndNil(Contacts)
  end;

  Contacts := FRoute4MeManager.AddressBookContact.Get([-123, -321], ErrorString);
  try
    CheckNotNull(Contacts);
    CheckEquals(EmptyStr, ErrorString);
    CheckEquals(0, Contacts.Count);
  finally
    FreeAndNil(Contacts)
  end;

  Contacts := FRoute4MeManager.AddressBookContact.Get([-123, FContact.Id], ErrorString);
  try
    CheckNotNull(Contacts);
    CheckEquals(EmptyStr, ErrorString);
    CheckEquals(1, Contacts.Count);
  finally
    FreeAndNil(Contacts)
  end;

  Contacts := FRoute4MeManager.AddressBookContact.Get([FContact.Id, FContactIdForDelete], ErrorString);
  try
    CheckNotNull(Contacts);
    CheckEquals(EmptyStr, ErrorString);
    CheckEquals(2, Contacts.Count);
  finally
    FreeAndNil(Contacts)
  end;

  // todo: почему-то, если id единственный, выдает неверный результат. Спросил у Олега
  // сейчас подогнал логику в TAddressBookContactActions.Get
  Contacts := FRoute4MeManager.AddressBookContact.Get([FContact.Id], ErrorString);
  try
    CheckNotNull(Contacts);
    CheckEquals(EmptyStr, ErrorString);
    CheckEquals(1, Contacts.Count);
  finally
    FreeAndNil(Contacts)
  end;
end;

procedure TTestAddressBookSamples.LocationSearch;
var
  ErrorString: String;
  Contacts: TAddressBookContactList;
  Limit, Offset, Total: integer;
  Query: String;
begin
  Limit := 2;
  Offset := 0;

  Query := 'john';
  Contacts := FRoute4MeManager.AddressBookContact.Find(
    Query, ['address_id','first_name', 'address_1'], Limit, Offset, Total, ErrorString);
  try
    CheckNotNull(Contacts);
    CheckEquals(EmptyStr, ErrorString);
    CheckTrue(Total > 0);
    CheckTrue(Contacts.Count > 0);
  finally
    FreeAndNil(Contacts)
  end;

  Query := 'Fairlawn';
  Contacts := FRoute4MeManager.AddressBookContact.Find(
    Query, ['address_1'], Limit, Offset, Total, ErrorString);
  try
    CheckNotNull(Contacts);
    CheckEquals(EmptyStr, ErrorString);
    CheckTrue(Total > 0);
    CheckTrue(Contacts.Count > 0);
  finally
    FreeAndNil(Contacts)
  end;

  Query := 'Fairlawn';
  Contacts := FRoute4MeManager.AddressBookContact.Find(
    Query, ['first_name'], Limit, Offset, Total, ErrorString);
  try
    CheckNotNull(Contacts);
    CheckEquals(EmptyStr, ErrorString);
    CheckEquals(0, Total);
    CheckEquals(0, Contacts.Count);
  finally
    FreeAndNil(Contacts)
  end;

  Query := 'uniqueText#$2';
  Contacts := FRoute4MeManager.AddressBookContact.Find(
    Query, ['first_name', 'address_1'], Limit, Offset, Total, ErrorString);
  try
    CheckNotNull(Contacts);
    CheckEquals(EmptyStr, ErrorString);
    CheckEquals(0, Total);
    CheckEquals(0, Contacts.Count);
  finally
    FreeAndNil(Contacts)
  end;
end;

procedure TTestAddressBookSamples.RemoveLocations;
var
  ErrorString: String;
begin
  CheckTrue(FRoute4MeManager.AddressBookContact.Remove([-123], ErrorString));
  CheckEquals(EmptyStr, ErrorString);

  CheckTrue(FRoute4MeManager.AddressBookContact.Remove(
    [FContact.Id, FContactIdForDelete], ErrorString));
  CheckEquals(EmptyStr, ErrorString);

  FreeAndNil(FContact);
end;

procedure TTestAddressBookSamples.UpdateLocation;
var
  ErrorString: String;
  Contact: TAddressBookContact;
begin
  FContact.FirstName := 'Mary';

  Contact := FRoute4MeManager.AddressBookContact.Update(FContact, ErrorString);
  try
    CheckNotNull(Contact);
    CheckEquals(EmptyStr, ErrorString);
    CheckTrue(Contact.Id.IsNotNull);
    CheckEquals('Mary', Contact.FirstName);
  finally
    FreeAndNil(Contact)
  end;

  FContact.Id := NullableInteger.Null;
  FContact.FirstName := 'Tom';
  Contact := FRoute4MeManager.AddressBookContact.Update(FContact, ErrorString);
  try
    CheckNull(Contact);
    CheckNotEquals(EmptyStr, ErrorString);
  finally
    FreeAndNil(Contact)
  end;
end;

initialization
  RegisterTest('Examples\Online\AddressBook\', TTestAddressBookSamples.Suite);
end.
