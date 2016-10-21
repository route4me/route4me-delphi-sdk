unit HttpQueryMemberAttributeUnit;

interface

uses
  REST.Json.Types, REST.JsonReflect;

type
  HttpQueryMemberAttribute = class abstract(TCustomAttribute)
  private
    FName: String;
    FIsRequired: boolean;
    FDefaultValue: String;
  public
    /// <summary>
    /// Constructor of JSONNullableAttribute
    /// <param name="IsRequired"> Required attribute or not. </param>
    /// <param name="DefaultValue"> Default value of required attribute. </param>
    /// </summary>
    constructor Create(Name: String; IsRequired: boolean = False; DefaultValue: String = '');

    property Name: String read FName;
    property IsRequired: boolean read FIsRequired;
    property DefaultValue: String read FDefaultValue;
  end;

implementation

{ BaseJSONNullableAttribute }

constructor HttpQueryMemberAttribute.Create(Name: String;
  IsRequired: boolean = False; DefaultValue: String = '');
begin
  Inherited Create;

  FName := Name;
  FIsRequired := IsRequired;
  FDefaultValue := DefaultValue;
end;

end.
