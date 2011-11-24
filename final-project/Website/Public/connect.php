<?

include('../../../../../login_info.php');

// $db_username set in login_info file, provides username for database
// $db_password set in login_info file, provides password for database
// $db_host     set in login_info file, provides hostname for database
// $db_name     set in login_info file, provides name of database

session_start();

$connection = mysql_connect($db_host, $db_username, $db_password);

if (!$connection) {
    die('Error connecting to database.');
}

if (!mysql_select_db($db_name)) {
    die('Error selecting database.');
}

$_SESSION['debug'] = false;

?>

