<?

include('connect.php');

if (isset($_SESSION['user_id']))
    header("Location: index.php");

$username = '';
$password = '';
$confirm  = '';

$error_message = '';

if (isset($_POST['username']))
    $username = $_POST['username'];
if (isset($_POST['password']))
    $password = $_POST['password'];
if (isset($_POST['confirm']))
    $confirm  = $_POST['confirm'];

if (isset($_POST['username'])) {
    $stmt = $db->prepare("SELECT username FROM login WHERE username = ?");
    $stmt->bind_param("s", $username);
    $stmt->execute();
    $stmt->bind_result($rows);
    $stmt->fetch();
    $stmt->close();

    if (strlen($username) == 0) {
        $error_message = "Error: username cannot be blank.";
    } else if (strlen($password) == 0) {
        $error_message = "Error: password cannot be blank.";
    } else if ($password != $confirm) {
        $error_message = "Error: passwords do not match.";
    } else if (count($rows) != 0) {
        $error_message = "Error: username already registered. Contact ntietz@gmail.com to reset your password.";
    }
}

$hashed_password = hash('sha256', $username . ':' . $password);

if (isset($_POST['username']) && strlen($error_message) == 0)
{
    $stmt = $db->prepare("INSERT INTO login (username, password) VALUES (?, ?)");
    $stmt->bind_param("ss", $username, $hashed_password);
    $stmt->execute();
    $stmt->bind_result($foo);
    $stmt->fetch();
    $stmt->close();

    $error_message = "Success! <a href='login.php'>Log in.</a>";
}

?>

<html>
<head>
    <title>AI Final - Registration</title>
</head>
<body>

    <h1>Register</h1>

    <? echo $error_message; ?>

    <form action="register.php" method="POST">
    <table>
        <tr>
            <td>Username:
            <td><input type="text" name="username" value="<? echo $username; ?>" />
        <tr>
            <td>Password:
            <td><input type="password" name="password" value="<? echo $password; ?>" />
        <tr>
            <td>Confirm password:
            <td><input type="password" name="confirm" value="<? echo $confirm; ?>" />
        <tr>
            <td>
            <td><input type="submit" />
    </table>
    </form>

</body>
</html>

