<?

include('connect.php');

if (isset($_SESSION['user_id']))
    header("Location: index.php");

$username = '';
$password = '';

if (isset($_POST['username']))
    $username = $_POST['username'];
if (isset($_POST['password']))
    $password = $_POST['password'];

$hashed_password = hash('sha256', $username . ':' . $password);

$query = $db->prepare("SELECT id FROM login WHERE password = ? and username = ?");
$query->bind_param("ss", substr($hashed_password,0,60), $username);
$query->execute();
$query->bind_result($rows);
$query->fetch();
$query->close();

if (count($rows) > 1) {
    echo ("FATAL ERROR: contact ntietz@gmail.com");
} else if (isset($_POST['username']) && $rows > 0) {
    $_SESSION['user_id'] = $rows;
    header('Location: index.php');
}

?>

<html>
<head>
    <title>AI Final - Login</title>
</head>
<body>

    <h1>Login</h1>

    <form action="login.php" method="POST">
    <table>
        <tr>
            <td>Username:
            <td><input type="text" name="username" value="<? echo $username; ?>" />
        <tr>
            <td>Password:
            <td><input type="password" name="password" value="<? echo $password; ?>" />
        <tr>
            <td>
            <td><input type="submit" />
    </table>
    </form>

</body>
</html>

