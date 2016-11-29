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

  end;

implementation

constructor TOrderParameters.Create;
begin
  Inherited Create;

  FLimit := NullableInteger.Null;
  FOffset := NullableInteger.Null;
end;

end.
