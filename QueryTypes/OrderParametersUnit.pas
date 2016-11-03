unit OrderParametersUnit;

interface

uses
  HttpQueryMemberAttributeUnit, JSONNullableAttributeUnit,
  NullableBasicTypesUnit, GenericParametersUnit;

type
  TOrderParameters = class(TGenericParameters)
  private
    [HttpQueryMember('limit')]
    [Nullable]
    FLimit: NullableInteger;

    [HttpQueryMember('offset')]
    [Nullable]
    FOffset: NullableInteger;

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
    constructor Create; override;

    /// <summary>
    /// Limit per page, if you use 0 you will get all records
    /// </summary>
    property Limit: NullableInteger read FLimit write FLimit;

    /// <summary>
    /// Offset
    /// </summary>
    property Offset: NullableInteger read FOffset write FOffset;

    /// <summary>
    /// if query is array search engine will search by fields, if query is string will search by all text fields. Array / string.
    /// </summary>
    property Query: NullableString read FQuery write FQuery;

    /// <summary>
    /// Use it for get specific fields. String / coma separated
    /// </summary>
    property Fields: NullableString read FFields write FFields;

    /// <summary>
    /// filter routed/unrouted. enum(all,routed,unrouted)
    /// </summary>
    property Display: NullableString read FDisplay write FDisplay;
  end;

implementation

{ TAddressParameters }

constructor TOrderParameters.Create;
begin
  Inherited Create;

  FLimit := NullableInteger.Null;
  FOffset := NullableInteger.Null;
  FQuery := NullableString.Null;
  FFields := NullableString.Null;
  FDisplay := NullableString.Null;
end;

end.
