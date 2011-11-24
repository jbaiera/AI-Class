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

echo ($hashed_password);

//$prepared_statment = $connection->prepare('select * from login where ');

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

