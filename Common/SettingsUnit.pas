unit SettingsUnit;

interface

type

  TEndPoints = record
    const Main = 'https://www.route4me.com';

    const ShowRoute = Main + '/route4me.php';
    const Optimization = Main + '/api.v4/optimization_problem.php';
    const Route = Main + '/api.v4/route.php';
    const ResequenceRoute = Main + '/api.v3/route/reoptimize_2.php';
    const ShareRoute = Main + '/actions/route/share_route.php';
    const MergeRouteEndPoint = Main + '/actions/merge_routes.php';
    const SetGps = Main + '/track/set.php';
    const Authenticate = Main + '/actions/authenticate.php';
    const GetUsers = Main + '/api/member/view_users.php';
    const UserLicense = Main + '/api/member/user_license.php';
    const RegisterAccount = Main + '/actions/register_action.php';
    const Users = Main + '/api.v4/user.php';
    const Vehicles = Main + '/api/vehicles/view_vehicles.php';
    const VerifyDeviceLicense = Main + '/api/device/verify_device_license.php';
    const RegisterWebinar = Main + '/actions/webinar_register.php';
    const ValidateSession = Main + '/datafeed/session/validate_session.php';
    const AddRouteNotes = Main + '/actions/addRouteNotes.php';
    const ActivityFeed = Main + '/api.v4/activity_feed.php';
    const GetActivities = Main + '/api/get_activities.php';
    const Geocoding = Main + '/api/geocoder.php';
    const BulkGeocoding = Main + '/actions/upload/json-geocode.php';
    const GetAddress = Main + '/api.v4/address.php';
    const DuplicateRoute = Main + '/actions/duplicate_route.php';
    const MoveRouteDestination = Main + '/actions/route/move_route_destination.php';
    const AddressBook = Main + '/api.v4/address_book.php';
    const Address = Main + '/api.v4/address.php';
    const MarkAddressAsVisited = Main + '/actions/address/update_address_visited.php';
    const MarkAddressAsDeparted = Main + '/api/route/mark_address_departed.php';
    const Avoidance = Main + '/api.v4/avoidance.php';
    const Order = Main + '/api.v4/order.php';
    const Territory = Main + '/api.v4/territory.php';
    const GetDeviceLocation = Main + '/api/track/get_device_location.php';
    const TrackingStatus = Main + '/api.v4/status.php';
    const ConfigurationSettings = Main + '/api.v4/configuration-settings.php';
    const FileUploading = Main + '/actions/upload/upload.php';
    const CsvXlsGeocode = Main + '/actions/upload/csv-xls-geocode.php';
    const CsvXlsPreview = Main + '/actions/upload/csv-xls-preview.php';


    const RapidAddressSearch = 'https://rapid.route4me.com/street_data';
  end;

  /// <summary>
  /// Route4Me infrastructure settings
  /// Api version 4 hosts constants
  /// </summary>
  TSettings = record
  public
    const ApiVersion = '4';


    /// <summary>
    ///   Default timeout - 30 minutes
    /// </summary>
    const DefaultTimeOutMinutes = 30;

    class var EndPoints: TEndPoints;
  end;


implementation

end.
