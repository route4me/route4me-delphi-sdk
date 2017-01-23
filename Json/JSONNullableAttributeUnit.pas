unit JSONNullableAttributeUnit;

interface

uses
  {$IF CompilerVersion < 27.0}
  Data.DBXJSONReflect
  {$ELSE}
  REST.Json.Types,
  REST.JsonReflect
  {$IFEND}
  ;

{$IF CompilerVersion < 27.0}
type
  JsonReflectAttribute = JsonReflect;
{$IFEND}

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
    FClass: TClass;
    FIsRequired: boolean;
  public
    constructor Create(Clazz: TClass; IsRequired: boolean = False); reintroduce;
    property Clazz: TClass read FClass;
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

constructor NullableArrayAttribute.Create(Clazz: TClass; IsRequired: boolean);
begin
  Inherited Create(ctObject, rtStrings, TNullableArrayInterceptor);

  FIsRequired := IsRequired;
  FClass := Clazz;
end;

end.
