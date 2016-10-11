unit JSONNullableAttributeUnit;

interface

uses
  REST.Json.Types, REST.JsonReflect;

type
  JSONNullableAttribute = class(JsonReflectAttribute)
  private
    FIsRequired: boolean;
  protected
    function GetInterceptorType: TClass; virtual;
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

uses NullableInterceptorUnit;

constructor JSONNullableAttribute.Create(IsRequired: boolean);
begin
  Inherited Create(ctObject, rtString, GetInterceptorType);
  FIsRequired := IsRequired;
end;

function JSONNullableAttribute.GetInterceptorType: TClass;
begin
  Result := TNullableInterceptor;
end;

end.
