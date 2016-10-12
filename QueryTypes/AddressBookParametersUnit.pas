unit AddressBookParametersUnit;

interface

uses NullableBasicTypesUnit, GenericParametersUnit;

type
  TAddressBookParameters = class(TGenericParameters)
  private
//    [HttpQueryMemberAttribute(Name = "address_id", EmitDefaultValue = false)]
    FAddressId: NullableString;

//    [HttpQueryMemberAttribute(Name = "limit", EmitDefaultValue = false)]
    FLimit: NullableInteger;

//    [HttpQueryMemberAttribute(Name = "offset", EmitDefaultValue = false)]
    FOffset: NullableInteger;

//    [HttpQueryMemberAttribute(Name = "start", EmitDefaultValue = false)]
    FStart: NullableInteger;

//    [HttpQueryMemberAttribute(Name = "query", EmitDefaultValue = false)]
    FQuery: NullableString;

//    [HttpQueryMemberAttribute(Name = "fields", EmitDefaultValue = false)]
    FFields: NullableString;

//    [HttpQueryMemberAttribute(Name = "display", EmitDefaultValue = false)]
    FDisplay: NullableString;
  public
    constructor Create;

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
