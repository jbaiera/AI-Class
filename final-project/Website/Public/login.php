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

$query = $db->prepare("SELECT * FROM login WHERE password = ?");
$query->bind_param("s", $hashed_password);
$query->execute();
$query->bind_result($rows);
$query->fetch();
$query->close();

//var_dump($rows);


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

