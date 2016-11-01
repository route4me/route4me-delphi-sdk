unit JSONNullableAttributeUnit;

interface

uses
  REST.Json.Types, REST.JsonReflect;

type
  BaseJSONNullableAttribute = class abstract(JsonReflectAttribute)
  private
    FIsRequired: boolean;
  protected
    constructor CreateCommon;
  public
    /// <summary>
    /// Constructor of JSONNullableAttribute
    /// <param name="IsRequired"> Required attribute or not. </param>
    /// </summary>
    constructor Create(IsRequired: boolean = False); overload;

    property IsRequired: boolean read FIsRequired;
  end;

  NullableAttribute = class(BaseJSONNullableAttribute)
  end;

  NullableArrayAttribute = class(JsonReflectAttribute)
  private
    FIsRequired: boolean;
  public
    constructor Create(IsRequired: boolean = False); reintroduce;
  end;

  NullableObjectAttribute = class(NullableAttribute)
  private
    FClass: TClass;
  public
    /// <summary>
    /// Constructor of JSONNullableAttribute
    /// <param name="IsRequired"> Required attribute or not. </param>
    /// </summary>
    constructor Create(Clazz: TClass; IsRequired: boolean = False); reintroduce;

    property Clazz: TClass read FClass;
  end;

implementation

uses NullableInterceptorUnit, NullableArrayInterceptorUnit;

{ NullableObjectAttribute }

constructor NullableObjectAttribute.Create(Clazz: TClass;
  IsRequired: boolean = False);
begin
  Inherited Create(IsRequired);
  FClass := Clazz;
end;

{ BaseJSONNullableAttribute }

constructor BaseJSONNullableAttribute.Create(IsRequired: boolean = False);
begin
  CreateCommon;
  FIsRequired := IsRequired;
end;

constructor BaseJSONNullableAttribute.CreateCommon;
begin
  Inherited Create(ctObject, rtString, TNullableInterceptor);
end;

{ NullableArrayAttribute }

constructor NullableArrayAttribute.Create(IsRequired: boolean);
begin
  FIsRequired := IsRequired;
  Inherited Create(ctObject, rtStrings, TNullableArrayInterceptor);
end;

end.
