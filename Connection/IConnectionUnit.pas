unit IConnectionUnit;

interface

uses Classes, GenericParametersUnit, DataObjectUnit, UtilsUnit, CommonTypesUnit;

type
  IConnection = interface
    ['{393B451A-B5C8-4A79-8745-AB0AD2310E9A}']
    procedure SetProxy(Host: String; Port: integer; Username, Password: String);

    function Get(Url: String; Data: TGenericParameters;
      ResultClassType: TClass; out ErrorString: String): TObject;
    function Post(Url: String; Data: TGenericParameters;
      ResultClassType: TClass; out ErrorString: String): TObject; overload;
    function Post(Url: String; Data: TGenericParameters; Stream: TStream;
      ResultClassType: TClass; out ErrorString: String): TObject; overload;
    function Post(Url: String; Data: TGenericParameters;
      PossibleResultClassType: TClassArray; out ErrorString: String): TObject; overload;
    function Put(Url: String; Data: TGenericParameters;
      ResultClassType: TClass; out ErrorString: String): TObject;
    function Delete(Url: String; Data: TGenericParameters;
      ResultClassType: TClass; out ErrorString: String): TObject;
  end;

implementation

end.
