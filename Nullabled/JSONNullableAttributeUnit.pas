unit JSONNullableAttributeUnit;

interface

uses
  REST.Json.Types, REST.JsonReflect;

type
  BaseJSONNullableAttribute = class abstract(JsonReflectAttribute)
  private
    FIsRequired: boolean;
  protected
    constructor CreateCommon; virtual;
  public
    /// <summary>
    /// Constructor of JSONNullableAttribute
    /// <param name="IsRequired"> Required attribute or not. </param>
    /// </summary>
    constructor Create(IsRequired: boolean = False); overload;

    property IsRequired: boolean read FIsRequired;
  end;

  JSONNullableStringAndNumberAttribute = class abstract(BaseJSONNullableAttribute)
  protected
    constructor CreateCommon; override;
  end;

  JSONNullableObjectAttribute = class (BaseJSONNullableAttribute)
  private
    FClass: TClass;
  protected
    constructor CreateCommon; override;
  public
    /// <summary>
    /// Constructor of JSONNullableAttribute
    /// <param name="IsRequired"> Required attribute or not. </param>
    /// </summary>
    constructor Create(Clazz: TClass; IsRequired: boolean = False); reintroduce;
  end;

  JSONNullableStringAttribute = class (JSONNullableStringAndNumberAttribute)
  end;

  JSONNullableNumberAttribute = class (JSONNullableStringAndNumberAttribute)
  end;

  JSONNullableBooleanAttribute = class (BaseJSONNullableAttribute)
  protected
    constructor CreateCommon; override;
  end;

implementation

uses NullableInterceptorUnit;

{ JSONNullableObjectAttribute }

constructor JSONNullableObjectAttribute.Create(Clazz: TClass;
  IsRequired: boolean);
begin
  FClass := Clazz;
  Inherited Create(IsRequired);
end;

constructor JSONNullableObjectAttribute.CreateCommon;
begin
  Inherited Create(ctObject, rtObject, TNullableObjectInterceptor);
end;

{ BaseJSONNullableAttribute }

constructor BaseJSONNullableAttribute.Create(IsRequired: boolean);
begin
  CreateCommon;
  FIsRequired := IsRequired;
end;

constructor BaseJSONNullableAttribute.CreateCommon;
begin
  Inherited Create();
end;

{ JSONNullableStringAndNumberAttribute }

constructor JSONNullableStringAndNumberAttribute.CreateCommon;
begin
  Inherited Create(ctObject, rtString, TNullableNumberAndStringInterceptor);
end;

{ JSONNullableBooleanAttribute }

constructor JSONNullableBooleanAttribute.CreateCommon;
begin
//  Inherited;
  Inherited Create(ctObject, rtObject, TNullableNumberAndStringInterceptor);
end;

end.
