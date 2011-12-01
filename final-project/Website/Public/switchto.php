<?

include('connect.php');

if (isset($_SESSION['admin_mode']) && $_SESSION['admin_mode'] == true && isset($_GET['user_id'])) {
    $_SESSION['user_id'] = $_GET['user_id'];
}

header('Location: index.php');

?>

