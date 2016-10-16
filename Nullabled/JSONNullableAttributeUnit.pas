unit JSONNullableAttributeUnit;

interface

uses
  REST.Json.Types, REST.JsonReflect;

type
  JSONNullableAttribute = class(JsonReflectAttribute)
  private
    FIsRequired: boolean;
  public
    /// <summary>
    /// Constructor of JSONNullableAttribute
    /// <param name="IsRequired"> Required attribute or not. </param>
    /// </summary>
    constructor Create(IsRequired: boolean = False); virtual;

    property IsRequired: boolean read FIsRequired;
  end;

implementation

{ JSONNullableAttribute }

uses NullableInterceptorUnit, NullableCustomizerUnit;

constructor JSONNullableAttribute.Create(IsRequired: boolean);
begin
  Inherited Create(ctObject, rtString, TNullableInterceptor, TNullableCustomizer);
  FIsRequired := IsRequired;
end;

end.
