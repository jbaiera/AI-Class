<?

include('../login_info.php');

// $db_username set in login_info file, provides username for database
// $db_password set in login_info file, provides password for database
// $db_host     set in login_info file, provides hostname for database
// $db_name     set in login_info file, provides name of database

session_start();

$db = new mysqli($db_host, $db_username, $db_password, $db_name);

if ($db->connect_errno != 0) {
    die('Error connecting to database.');
}

$_SESSION['debug'] = false;

?>

