unit RegisterWebinarRequestUnit;

interface

uses
  REST.Json.Types, SysUtils,
  GenericParametersUnit, CommonTypesUnit, EnumsUnit;

type
  TRegisterWebinarRequest = class(TGenericParameters)
  private

    [JSONName('email_address')]
    FEMail: String;

    [JSONName('first_name')]
    FFirstName: String;

    [JSONName('last_name')]
    FLastName: String;

    [JSONName('phone_number')]
    FPhone: String;

    [JSONName('company_name')]
    FCompany: String;

    [JSONName('member_id')]
    FMemberId: integer;

    [JSONName('webiinar_date')]
    FDate: String;
  public
    constructor Create(EMail, FirstName, LastName, Phone, Company: String;
      MemberId: integer; Date: TDateTime); reintroduce;
  end;

implementation

constructor TRegisterWebinarRequest.Create(
  EMail, FirstName, LastName, Phone, Company: String;
  MemberId: integer; Date: TDateTime);
begin
  Inherited Create;

  FEMail := EMail;
  FFirstName := FirstName;
  FLastName := LastName;
  FPhone := Phone;
  FCompany := Company;
  FMemberId := MemberId;

  DateTimeToString(FDate, 'yyyy-mm-dd hh:mm:ss', Date);
end;

end.
