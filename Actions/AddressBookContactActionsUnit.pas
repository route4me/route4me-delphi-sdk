unit AddressBookContactActionsUnit;

interface

uses
  System.Generics.Collections,
  AddressBookContactUnit, AddressBookParametersUnit, ConnectionUnit,
  IConnectionUnit, BaseActionUnit, SettingsUnit;

type

  TAddressBookContactActions = class(TBaseAction)
  private
  public
    function Add(Contact: TAddressBookContact;
      out errorString: String): boolean;
    function Update(Contact: TAddressBookContact;
      out errorString: String): boolean;
    function Remove(AddressId: String): boolean; overload;
    function Remove(AddressId: TArray<String>): boolean; overload;
    function Get(AddressBookParameters: TAddressBookParameters;
      out Total: integer; out ErrorString: String): TObjectList<TAddressBookContact>;
  end;

implementation

{ TAddressBookContact }

function TAddressBookContactActions.Remove(AddressId: String): boolean;
begin
  Result := Remove([AddressId]);
end;

function TAddressBookContactActions.Add(Contact: TAddressBookContact;
  out errorString: String): boolean;
begin
  Result := FConnection.Post(Contact, TSettings.AddressBook, errorString);

(* Responce:
{
	"created_timestamp": 1464269324,
	"address_id": 7254355,
	"address_group": "",
	"address_alias": "301 MARKET SHELL",
	"address_1": "17205 RICHMOND TNPK, MILFORD, VA, 22514",
	"address_2": "",
	"member_id": 1,
	"first_name": "Gela",
	"last_name": "Gorason",
	"address_email": "ggora@gmail.com",
	"address_phone_number": "8046335852",
	"address_city": "Tbilisi",
	"address_state_id": "0",
	"address_country_id": "0",
	"address_zip": "00167",
	"cached_lat": 38.024654,
	"cached_lng": -77.338814,
	"curbside_lat": 38.024654,
	"curbside_lng": -77.338814,
	"address_custom_data": {
		"sales rep id": "545",
		"sales rep name": "Kellye Foster",
		"retailer id": "173907"
	},
	"in_route_count": 0,
	"last_visited_timestamp": 0,
	"last_routed_timestamp": 0,
	"local_time_window_start": null,
	"local_time_window_end": null,
	"local_time_window_start_2": null,
	"local_time_window_end_2": null,
	"service_time": 0,
	"local_timezone_string": "",
	"color": "",
	"address_icon": null
}
*)

{      AddressBookContact result = GetJsonObjectFromAPI<AddressBookContact>(contact,
                                                           R4MEInfrastructureSettings.AddressBook,
                                                           HttpMethodType.Post,
                                                           out errorString);
 }
end;

function TAddressBookContactActions.Get(
  AddressBookParameters: TAddressBookParameters; out Total: integer;
  out ErrorString: String): TObjectList<TAddressBookContact>;
begin
  Result := TObjectList<TAddressBookContact>.Create;

(* Responce:
{
	"results": [{
		"created_timestamp": 1453928592,
		"address_id": 5608503,
		"address_group": "",
		"address_alias": "AHOLD",
		"address_1": "10 Technology Dr, Hudson, MA, 1749",
		"address_2": "",
		"member_id": 224171,
		"first_name": "",
		"last_name": "",
		"address_email": "",
		"address_phone_number": "",
		"address_city": "",
		"address_state_id": null,
		"address_country_id": null,
		"address_zip": "",
		"cached_lat": 42.37628,
		"cached_lng": -71.564345,
		"curbside_lat": 42.37628,
		"curbside_lng": -71.564345,
		"address_custom_data": {
			"store": "180600000489",
			"average hours per week": "3.1",
			"fsc": "Kleinendorst, Kirsten",
			"store number": "489",
			"store name": "AHOLD 489 [5637284682]",
			"3 zip": "a17",
			"weight": 0,
			"cost": 0,
			"revenue": 0,
			"cube": 0,
			"pieces": 0
		},
		"in_route_count": 0,
		"last_visited_timestamp": 0,
		"last_routed_timestamp": 0,
		"local_time_window_start": null,
		"local_time_window_end": null,
		"local_time_window_start_2": null,
		"local_time_window_end_2": null,
		"service_time": null,
		"local_timezone_string": null,
		"color": null,
		"address_icon": null
	},
	{
		"created_timestamp": 1448386827,
		"address_id": 4573562,
		"address_group": "",
		"address_alias": "AHOLD",
		"address_1": "10 Technology Dr, Hudson, MA, 1749",
		"address_2": "",
		"member_id": 177496,
		"first_name": "",
		"last_name": "",
		"address_email": "",
		"address_phone_number": "",
		"address_city": "",
		"address_state_id": null,
		"address_country_id": null,
		"address_zip": "",
		"cached_lat": 42.37628,
		"cached_lng": -71.564345,
		"curbside_lat": 42.37628,
		"curbside_lng": -71.564345,
		"address_custom_data": {
			"store": "180600000489",
			"ytd visits": "4",
			"ytd billing": "$384.00",
			"ytd bonuses": "",
			"months w\/bonus": "",
			"primary client": "Blackhawk",
			"fsc": "Kleinendorst, Kirsten",
			"store number": "489",
			"store name": "AHOLD 489 [5637284682]",
			"status": "Active",
			"weight": 0,
			"cost": 0,
			"revenue": 0,
			"cube": 0,
			"pieces": 0
		},
		"in_route_count": 0,
		"last_visited_timestamp": 0,
		"last_routed_timestamp": 0,
		"local_time_window_start": null,
		"local_time_window_end": null,
		"local_time_window_start_2": null,
		"local_time_window_end_2": null,
		"service_time": null,
		"local_timezone_string": null,
		"color": null,
		"address_icon": null
	},
	{
		"created_timestamp": 1448386051,
		"address_id": 4558122,
		"address_group": "",
		"address_alias": "BJ's Wholesale Clubs",
		"address_1": "901 Technology Center Drive, Stoughton, MA, 2072",
		"address_2": "",
		"member_id": 177496,
		"first_name": "",
		"last_name": "",
		"address_email": "",
		"address_phone_number": "",
		"address_city": "",
		"address_state_id": null,
		"address_country_id": null,
		"address_zip": "",
		"cached_lat": 42.146947,
		"cached_lng": -71.065288,
		"curbside_lat": 42.146947,
		"curbside_lng": -71.065288,
		"address_custom_data": {
			"store": "102900000034",
			"ytd visits": "1",
			"ytd billing": "$27.50",
			"ytd bonuses": "",
			"months w\/bonus": "",
			"primary client": "Euro-Pro Operating LLC",
			"fsc": "Kleinendorst, Kirsten",
			"store number": "34",
			"store name": "BJ's Wholesale Clubs 34",
			"status": "Active",
			"weight": 0,
			"cost": 0,
			"revenue": 0,
			"cube": 0,
			"pieces": 0
		},
		"in_route_count": 0,
		"last_visited_timestamp": 0,
		"last_routed_timestamp": 0,
		"local_time_window_start": null,
		"local_time_window_end": null,
		"local_time_window_start_2": null,
		"local_time_window_end_2": null,
		"service_time": null,
		"local_timezone_string": null,
		"color": null,
		"address_icon": null
	},
	{
		"created_timestamp": 1450730656,
		"address_id": 4963296,
		"address_group": "",
		"address_alias": "CSM, INC",
		"address_1": "300 TECHNOLOGY DRIVE, MALVERN, 19355",
		"address_2": "",
		"member_id": 38392,
		"first_name": "",
		"last_name": "",
		"address_email": "",
		"address_phone_number": "",
		"address_city": "",
		"address_state_id": null,
		"address_country_id": null,
		"address_zip": "",
		"cached_lat": 40.061413,
		"cached_lng": -75.53828,
		"curbside_lat": 40.061413,
		"curbside_lng": -75.53828,
		"address_custom_data": {
			"size": "6.00",
			"lifts": "1.00",
			"cur rte": "2202",
			"cust name": "CSM, INC",
			"weight": 0,
			"cost": 0,
			"revenue": 0,
			"cube": 0,
			"pieces": 0
		},
		"in_route_count": 2,
		"last_visited_timestamp": 0,
		"last_routed_timestamp": 1450970254,
		"local_time_window_start": null,
		"local_time_window_end": null,
		"local_time_window_start_2": null,
		"local_time_window_end_2": null,
		"service_time": null,
		"local_timezone_string": null,
		"color": null,
		"address_icon": null
	},
	{
		"created_timestamp": 1448386012,
		"address_id": 4556878,
		"address_group": "",
		"address_alias": "Kohls",
		"address_1": "501 TECHNOLOGY CENTER DR, STOUGHTON, MA, 02072-4720",
		"address_2": "",
		"member_id": 177496,
		"first_name": "",
		"last_name": "",
		"address_email": "",
		"address_phone_number": "",
		"address_city": "",
		"address_state_id": null,
		"address_country_id": null,
		"address_zip": "",
		"cached_lat": 42.153782,
		"cached_lng": -71.066685,
		"curbside_lat": 42.153782,
		"curbside_lng": -71.066685,
		"address_custom_data": {
			"store": "100400000766",
			"ytd visits": "3",
			"ytd billing": "$154.67",
			"ytd bonuses": "",
			"months w\/bonus": "",
			"primary client": "Kohls Accessories",
			"fsc": "Kleinendorst, Kirsten",
			"store number": "766",
			"store name": "Kohls 766",
			"status": "Active",
			"weight": 0,
			"cost": 0,
			"revenue": 0,
			"cube": 0,
			"pieces": 0
		},
		"in_route_count": 0,
		"last_visited_timestamp": 0,
		"last_routed_timestamp": 0,
		"local_time_window_start": null,
		"local_time_window_end": null,
		"local_time_window_start_2": null,
		"local_time_window_end_2": null,
		"service_time": null,
		"local_timezone_string": null,
		"color": null,
		"address_icon": null
	},
	{
		"created_timestamp": 1448385983,
		"address_id": 4556101,
		"address_group": "",
		"address_alias": "Wal-Mart",
		"address_1": "71 TECHNOLOGY DR, IRVINE, CA, 92618",
		"address_2": "",
		"member_id": 177496,
		"first_name": "",
		"last_name": "",
		"address_email": "",
		"address_phone_number": "",
		"address_city": "",
		"address_state_id": null,
		"address_country_id": null,
		"address_zip": "",
		"cached_lat": 33.657683,
		"cached_lng": -117.740069,
		"curbside_lat": 33.657683,
		"curbside_lng": -117.740069,
		"address_custom_data": {
			"store": "100300005687",
			"ytd visits": "3",
			"ytd billing": "$276.85",
			"ytd bonuses": "",
			"months w\/bonus": "",
			"primary client": "Hanes Brands, Inc.",
			"fsc": "Szaroletta, Jennifer",
			"store number": "5687",
			"store name": "Wal-Mart Supercenter 5687",
			"status": "Active",
			"weight": 0,
			"cost": 0,
			"revenue": 0,
			"cube": 0,
			"pieces": 0
		},
		"in_route_count": 0,
		"last_visited_timestamp": 0,
		"last_routed_timestamp": 0,
		"local_time_window_start": null,
		"local_time_window_end": null,
		"local_time_window_start_2": null,
		"local_time_window_end_2": null,
		"service_time": null,
		"local_timezone_string": null,
		"color": null,
		"address_icon": null
	}],
	"total": 6
}
*)
end;

function TAddressBookContactActions.Remove(AddressId: TArray<String>): boolean;
begin

(* Responce:
{"status":true}
*)
end;

function TAddressBookContactActions.Update(
  Contact: TAddressBookContact; out errorString: String): boolean;
begin
(* Responce:
{
	"created_timestamp": 1462362515,
	"address_id": 6879135,
	"address_group": "",
	"address_alias": "301 MARKET SHELL",
	"address_1": "17205 RICHMOND TNPK, MILFORD, VA, 22514",
	"address_2": "",
	"member_id": 1,
	"first_name": "Modified",
	"last_name": "Gorason",
	"address_email": "ggora@gmail.com",
	"address_phone_number": "8046335852",
	"address_city": "Tbilisi",
	"address_state_id": "0",
	"address_country_id": "0",
	"address_zip": "00167",
	"cached_lat": 38.024654,
	"cached_lng": -77.338814,
	"curbside_lat": 38.024654,
	"curbside_lng": -77.338814,
	"address_custom_data": [],
	"in_route_count": 0,
	"last_visited_timestamp": 0,
	"last_routed_timestamp": 0,
	"local_time_window_start": null,
	"local_time_window_end": null,
	"local_time_window_start_2": null,
	"local_time_window_end_2": null,
	"service_time": 0,
	"local_timezone_string": "",
	"color": "",
	"address_icon": null
}
*)
end;

end.
