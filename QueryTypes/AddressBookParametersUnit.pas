unit AddressBookParametersUnit;

interface

uses
  HttpQueryMemberAttributeUnit, JSONNullableAttributeUnit,
  NullableBasicTypesUnit, GenericParametersUnit;

type
  TAddressBookParameters = class(TGenericParameters)
  private
    [HttpQueryMember('address_id')]
    [Nullable]
    FAddressId: NullableString;

    [HttpQueryMember('limit')]
    [Nullable]
    FLimit: NullableInteger;

    [HttpQueryMember('offset')]
    [Nullable]
    FOffset: NullableInteger;

    [HttpQueryMember('start')]
    [Nullable]
    FStart: NullableInteger;

    [HttpQueryMember('query')]
    [Nullable]
    FQuery: NullableString;

    [HttpQueryMember('fields')]
    [Nullable]
    FFields: NullableString;

    [HttpQueryMember('display')]
    [Nullable]
    FDisplay: NullableString;
  public
    constructor Create; overload; override;
    constructor Create(Limit: integer; Offset: integer); reintroduce; overload;

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
  Inherited Create;

  FAddressId := NullableString.Null;
  FLimit := NullableInteger.Null;
  FOffset := NullableInteger.Null;
  FStart := NullableInteger.Null;
  FQuery := NullableString.Null;
  FFields := NullableString.Null;
  FDisplay := NullableString.Null;
end;

end.
