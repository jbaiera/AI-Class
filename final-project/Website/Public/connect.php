<?

include('../../../../../login_info.php');

// $db_username set in login_info file, provides username for database
// $db_password set in login_info file, provides password for database
// $db_host     set in login_info file, provides hostname for database
// $db_name     set in login_info file, provides name of database

mysql_connect($db_host, $db_username, $db_password) or die('Error connecting to database.');
mysql_select_db($db_name) or die('Error selecting database.');

?>

