unit AddressBookParametersUnit;

interface

uses
  NullableBasicTypesUnit,
  GenericParametersUnit, HttpQueryMemberAttributeUnit;

type
  TAddressBookParameters = class(TGenericParameters)
  private
    [HttpQueryMember('address_id')]
    FAddressId: NullableString;

    [HttpQueryMember('limit')]
    FLimit: NullableInteger;

    [HttpQueryMember('offset')]
    FOffset: NullableInteger;

    [HttpQueryMember('start')]
    FStart: NullableInteger;

    [HttpQueryMember('query')]
    FQuery: NullableString;

    [HttpQueryMember('fields')]
    FFields: NullableString;

    [HttpQueryMember('display')]
    FDisplay: NullableString;
  public
    constructor Create; overload;
    constructor Create(Limit: integer; Offset: integer); overload;

    property AddressId: NullableString read FAddressId write FAddressId;
    property Limit: NullableInteger read FLimit write FLimit;
    property Offset: NullableInteger read FOffset write FOffset;
    property Start: NullableInteger read FStart write FStart;
    property Query: NullableString read FQuery write FQuery;
    property Fields: NullableString read FFields write FFields;
    property Display: NullableString read FDisplay write FDisplay;
  end;
implementation

{ TAddressBookParameters }

constructor TAddressBookParameters.Create(Limit: integer; Offset: integer);
begin
  Create;

  FLimit := Limit;
  FOffset := Offset;
end;

constructor TAddressBookParameters.Create;
begin
    FAddressId := NullableString.Null;
    FLimit := NullableInteger.Null;
    FOffset := NullableInteger.Null;
    FStart := NullableInteger.Null;
    FQuery := NullableString.Null;
    FFields := NullableString.Null;
    FDisplay := NullableString.Null;
end;

end.
