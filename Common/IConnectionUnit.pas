unit IConnectionUnit;

interface

uses GenericParametersUnit, DataObjectUnit, UtilsUnit;

type
  IConnection = interface
    ['{393B451A-B5C8-4A79-8745-AB0AD2310E9A}']
//    function Post(Data: TGenericParameters; Url: String; out ErrorString: String): boolean; overload;
    function Post(Url: String; Data: TGenericParameters;
      ResultClassType: TClass; out ErrorString: String): TObject;
  end;

implementation

end.
