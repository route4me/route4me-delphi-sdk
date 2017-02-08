unit GeocodingActionsUnit;

interface

uses
  SysUtils, BaseActionUnit, CommonTypesUnit, BulkGeocodingRequestUnit,
  GeocodingUnit, EnumsUnit, DirectionPathPointUnit, GeocodingAddressUnit;

type
  TGeocodingActions = class(TBaseAction)
  private
    function ParseXml(XmlString: String): TGeocodingList;
  public
    /// <summary>
    /// Forward geocoding is the process of converting place name information into latitude and longitude values
    /// </summary>
    function ForwardGeocodeAddress(Address: String; out ErrorString: String): TGeocoding;

    /// <summary>
    /// Forward geocoding is the process of converting place name information into latitude and longitude values
    /// </summary>
    function ForwardGeocodeAddresses(Addresses: TAddressInfoArray;
      out ErrorString: String): TGeocodingList;

    /// <summary>
    /// With the reverse geocoding you can retrieve an address name from a geographical location point (latitude, longitude).
    /// Using this method, you can get the nearest locations to a specific address name.
    /// You can also get the larger scale objects (such as street addresses, places,
    /// neighbourhoods, county, state or country) which include a specified address.
    /// </summary>
    function ReverseGeocodeAddress(Location: TDirectionPathPoint;
      out ErrorString: String): TGeocodingList;

    /// <summary>
    /// Single address geocoding refers to the process of getting a geographic address by address name sent with HTTP GET data.
    /// </summary>
    function GetSingleAddress(Pk: integer; out ErrorString: String): TGeocodingAddress;

    /// <summary>
    /// This example refers to the process of getting all addresses.
    /// </summary>
    function GetAddresses(out ErrorString: String): TGeocodingAddressList; overload;

    /// <summary>
    /// This example refers to the process of getting a limited number of the addresses. The limitation parameters are: offset and limit.
    /// </summary>
    function GetAddresses(Limit, Offset: integer;
      out ErrorString: String): TGeocodingAddressList; overload;

    /// <summary>
    /// This example refers to the process of getting all addresses containing a specified zip code.
    /// </summary>
    function GetZipCodes(ZipCode: String; out ErrorString: String): TGeocodingAddressList; overload;

    /// <summary>
    /// This example refers to the process of getting a limited number of addresses containing a specified zip code.
    /// </summary>
    function GetZipCodes(ZipCode: String; Limit, Offset: integer;
      out ErrorString: String): TGeocodingAddressList; overload;

    /// <summary>
    /// This example refers to the process of getting all addresses containing a specified zip code and house number.
    /// </summary>
    function GetZipCodeAndHouseNumber(ZipCode: String; HouseNumber: String;
      out ErrorString: String): TGeocodingAddressList; overload;

    /// <summary>
    /// This example refers to the process of getting a limited number of addresses containing a specified zip code and house number.
    /// </summary>
    function GetZipCodeAndHouseNumber(ZipCode: String; HouseNumber: String;
      Limit, Offset: integer; out ErrorString: String): TGeocodingAddressList; overload;
  end;

implementation

{ TGeocodingActions }

uses
  Xml.XMLDoc, Xml.XMLIntf,
  SettingsUnit, GenericParametersUnit, UtilsUnit,
  NullableBasicTypesUnit;


{ TGeocodingActions }

function TGeocodingActions.ForwardGeocodeAddress(Address: String;
  out ErrorString: String): TGeocoding;
var
  Request: TGenericParameters;
  XmlString: TSimpleString;
  Parsed: TGeocodingList;
begin
  Result := nil;

  Request := TGenericParameters.Create;
  try
    Request.AddParameter('addresses', Address);
    Request.AddParameter('format', TFormatDescription[TFormatEnum.Xml]);

    XmlString := FConnection.Post(TSettings.EndPoints.Geocoding, Request,
      TSimpleString, ErrorString) as TSimpleString;
    try
      if (XmlString <> nil) then
      begin
        Parsed := ParseXml(XmlString.Value);
        try
          if (Parsed.Count > 0) then
          begin
            if (Parsed[0].Type_.IsNotNull) and (Parsed[0].Type_.Value = 'invalid') then
            begin
              ErrorString := 'Forward Geocode Address fault';
              Exit;
            end;

            Result := Parsed[0];
            Parsed.OwnsObjects := False;
            Parsed.Remove(Result);
            Parsed.OwnsObjects := True;
          end;
        finally
          FreeAndNil(Parsed);
        end;
      end;
    finally
      FreeAndNil(XmlString);
    end;

    if (Result = nil) and (ErrorString = EmptyStr) then
      ErrorString := 'Forward Geocode Address fault';
  finally
    FreeAndNil(Request);
  end;
end;

function TGeocodingActions.GetAddresses(
  out ErrorString: String): TGeocodingAddressList;
var
  Request: TGenericParameters;
  Url: String;
begin
  Request := TGenericParameters.Create;
  try
    Url := Format('%s/', [TSettings.EndPoints.RapidAddressSearch]);

    Result := FConnection.Get(Url, Request,
      TGeocodingAddressList, ErrorString) as TGeocodingAddressList;

    if (Result = nil) and (ErrorString = EmptyStr) then
      ErrorString := 'Get Addresses fault';
  finally
    FreeAndNil(Request);
  end;
end;

function TGeocodingActions.GetZipCodes(ZipCode: String;
  out ErrorString: String): TGeocodingAddressList;
var
  Request: TGenericParameters;
  Url: String;
begin
  Request := TGenericParameters.Create;
  try
    Url := Format('%s/zipcode/%s/', [TSettings.EndPoints.RapidAddressSearch, ZipCode]);

    Result := FConnection.Get(Url, Request,
      TGeocodingAddressList, ErrorString) as TGeocodingAddressList;

    if (Result = nil) and (ErrorString = EmptyStr) then
      ErrorString := 'Get Zip Codes fault';
  finally
    FreeAndNil(Request);
  end;
end;

function TGeocodingActions.ForwardGeocodeAddresses(Addresses: TAddressInfoArray;
  out ErrorString: String): TGeocodingList;
var
  Request: TBulkGeocodingRequest;
  XmlString: TSimpleString;
  List: TGeocodingList;
  i: integer;
  IsInvalid: boolean;
begin
  Result := TGeocodingList.Create;

  Request := TBulkGeocodingRequest.Create;
  try
    for i := 0 to High(Addresses) do
      Request.AddAddress(Addresses[i]);

    XmlString := FConnection.Post(TSettings.EndPoints.BulkGeocoding, Request,
      TSimpleString, ErrorString) as TSimpleString;
    try
      if (XmlString <> nil) then
      begin
        try
          List := ParseXml(XmlString.Value);
          try
            if (List.Count > 0) then
            begin
              IsInvalid := True;
              for i := 0 to List.Count - 1 do
                if (List[0].Type_.IsNotNull) and (List[0].Type_.Value <> 'invalid') then
                begin
                  IsInvalid := False;
                  Break;
                end;
              if IsInvalid then
                Exit;

              List.OwnsObjects := False;
              Result.AddRange(List);
            end;
          finally
            FreeAndNil(List);
          end;
        except
          ErrorString := XmlString.Value;
          Exit;
        end;
      end;
    finally
      FreeAndNil(XmlString);
    end;

    if (Result = nil) and (ErrorString = EmptyStr) then
      ErrorString := 'Forward Geocode Addresses fault';
  finally
    FreeAndNil(Request);
  end;
end;

function TGeocodingActions.GetAddresses(Limit, Offset: integer;
  out ErrorString: String): TGeocodingAddressList;
var
  Request: TGenericParameters;
  Url: String;
begin
  Request := TGenericParameters.Create;
  try
    Url := Format('%s/%d/%d/', [TSettings.EndPoints.RapidAddressSearch, Offset, Limit]);

    Result := FConnection.Get(Url, Request,
      TGeocodingAddressList, ErrorString) as TGeocodingAddressList;

    if (Result = nil) and (ErrorString = EmptyStr) then
      ErrorString := 'Get Addresses fault';
  finally
    FreeAndNil(Request);
  end;
end;

function TGeocodingActions.GetSingleAddress(Pk: integer;
  out ErrorString: String): TGeocodingAddress;
var
  Request: TGenericParameters;
  Url: String;
begin
  Request := TGenericParameters.Create;
  try
    Url := Format('%s/%d/', [TSettings.EndPoints.RapidAddressSearch, Pk]);

    Result := FConnection.Get(Url, Request,
      TGeocodingAddress, ErrorString) as TGeocodingAddress;

    if (Result = nil) and (ErrorString = EmptyStr) then
      ErrorString := 'Get Single Address fault';
  finally
    FreeAndNil(Request);
  end;
end;

function TGeocodingActions.GetZipCodeAndHouseNumber(ZipCode,
  HouseNumber: String; Limit, Offset: integer;
  out ErrorString: String): TGeocodingAddressList;
var
  Request: TGenericParameters;
  Url: String;
begin
  Request := TGenericParameters.Create;
  try
    Url := Format('%s/service/%s/%s/%d/%d/',
      [TSettings.EndPoints.RapidAddressSearch, ZipCode, HouseNumber, Offset, Limit]);

    Result := FConnection.Get(Url, Request,
      TGeocodingAddressList, ErrorString) as TGeocodingAddressList;

    if (Result = nil) and (ErrorString = EmptyStr) then
      ErrorString := 'Get Zip Code And House Number fault';
  finally
    FreeAndNil(Request);
  end;
end;

function TGeocodingActions.GetZipCodes(ZipCode: String; Limit, Offset: integer;
  out ErrorString: String): TGeocodingAddressList;
var
  Request: TGenericParameters;
  Url: String;
begin
  Request := TGenericParameters.Create;
  try
    Url := Format('%s/zipcode/%s/%d/%d/',
      [TSettings.EndPoints.RapidAddressSearch, ZipCode, Offset, Limit]);

    Result := FConnection.Get(Url, Request,
      TGeocodingAddressList, ErrorString) as TGeocodingAddressList;

    if (Result = nil) and (ErrorString = EmptyStr) then
      ErrorString := 'Get Zip Codes fault';
  finally
    FreeAndNil(Request);
  end;
end;

function TGeocodingActions.ParseXml(XmlString: String): TGeocodingList;
var
  Doc: IXMLDocument;
  Node, DestinationNode: IXmlNode;
  Item: TGeocoding;
  i: integer;
begin
  Result := TGeocodingList.Create;

  Doc := TXMLDocument.Create(nil);
  try
    Doc.LoadFromXML(XmlString);
    Node := Doc.ChildNodes.FindNode('destinations');
    if (Node = nil) then
      Exit;

    for i := 0 to Node.ChildNodes.Count - 1 do
    begin
      if (Node.ChildNodes[i].NodeName <> 'destination') then
        Continue;

      DestinationNode := Node.ChildNodes[i];

      Item := TGeocoding.Create;

      if DestinationNode.HasAttribute('destination') then
        Item.Destination := DestinationNode.Attributes['destination'];

      if DestinationNode.HasAttribute('lat') then
        Item.Latitude := TUtils.StrToFloat(DestinationNode.Attributes['lat']);

      if DestinationNode.HasAttribute('lng') then
        Item.Longitude := TUtils.StrToFloat(DestinationNode.Attributes['lng']);

      if DestinationNode.HasAttribute('confidence') then
        Item.SetConfidenceAsString(DestinationNode.Attributes['confidence']);

      if DestinationNode.HasAttribute('type') then
        Item.Type_ := DestinationNode.Attributes['type'];

      if DestinationNode.HasAttribute('original') then
        Item.Original := DestinationNode.Attributes['original'];

      Result.Add(Item);
    end;
  finally
    Doc := nil;
  end;
end;

function TGeocodingActions.GetZipCodeAndHouseNumber(ZipCode,
  HouseNumber: String; out ErrorString: String): TGeocodingAddressList;
var
  Request: TGenericParameters;
  Url: String;
begin
  Request := TGenericParameters.Create;
  try
    Url := Format('%s/service/%s/%s/',
      [TSettings.EndPoints.RapidAddressSearch, ZipCode, HouseNumber]);

    Result := FConnection.Get(Url, Request,
      TGeocodingAddressList, ErrorString) as TGeocodingAddressList;

    if (Result = nil) and (ErrorString = EmptyStr) then
      ErrorString := 'Get Zip Code And House Number fault';
  finally
    FreeAndNil(Request);
  end;
end;

function TGeocodingActions.ReverseGeocodeAddress(
  Location: TDirectionPathPoint; out ErrorString: String): TGeocodingList;
var
  Request: TGenericParameters;
  LocationParam: String;
  XmlString: TSimpleString;
  List: TGeocodingList;
  i: integer;
  IsInvalid: boolean;
begin
  Result := TGeocodingList.Create;

  Request := TGenericParameters.Create;
  try
    LocationParam := TUtils.FloatToStrDot(Location.Latitude.Value) + ',' +
      TUtils.FloatToStrDot(Location.Longitude.Value);
    Request.AddParameter('addresses', LocationParam);
    Request.AddParameter('format', TFormatDescription[TFormatEnum.Xml]);

    XmlString := FConnection.Post(TSettings.EndPoints.Geocoding, Request,
      TSimpleString, ErrorString) as TSimpleString;
    try
      if (XmlString <> nil) then
      begin
        List := ParseXml(XmlString.Value);
        try
          if (List.Count > 0) then
          begin
            IsInvalid := True;
            for i := 0 to List.Count - 1 do
              if (List[0].Type_.IsNotNull) and (List[0].Type_.Value <> 'invalid') then
              begin
                IsInvalid := False;
                Break;
              end;
            if IsInvalid then
              Exit;

            List.OwnsObjects := False;
            Result.AddRange(List);
          end;
        finally
          FreeAndNil(List);
        end;
      end;
    finally
      FreeAndNil(XmlString);
    end;

    if (Result = nil) and (ErrorString = EmptyStr) then
      ErrorString := 'Reverse Geocode Address fault';
  finally
    FreeAndNil(Request);
  end;
end;

end.
