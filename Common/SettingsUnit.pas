unit SettingsUnit;

interface

type
  /// <summary>
  /// Route4Me infrastructure settings
  /// Api version 4 hosts constants
  /// </summary>
  TSettings = class
  public
    const ApiVersion = '4';

    const MainHost = 'https://www.route4me.com';
    const ApiHost = MainHost + '/api.v4/optimization_problem.php';
    const ShowRouteHost = MainHost + '/route4me.php';
    const RouteHost = MainHost + '/api.v4/route.php';
    const SetGpsHost = MainHost + '/track/set.php';
    const GetUsersHost = MainHost + '/api/member/view_users.php';
    const AddRouteNotesHost = MainHost + '/actions/addRouteNotes.php';
    const ActivityFeedHost = MainHost + '/api.v4/activity_feed.php';
    const GetAddress = MainHost + '/api.v4/address.php';
    const DuplicateRoute = MainHost + '/actions/duplicate_route.php';
    const MoveRouteDestination = MainHost + '/actions/route/move_route_destination.php';
    const AddressBook = MainHost + '/api.v4/address_book.php';
    const Avoidance = MainHost + '/api.v4/avoidance.php';
    const Order = MainHost + '/api.v4/order.php';

    /// <summary>
    ///   Default timeout - 30 minutes
    /// </summary>
    const DefaultTimeOut = 30;
  end;

implementation

end.
