<?

include('connect.php');

if (! isset($_SESSION['user_id']))
{
    echo('Please log in <a href="login.php">here</a>. New users should <a href="register.php">register</a>.');
}
else
{
    echo('You can log out <a href="logout.php">here</a>.');
    if ($_SESSION['user_id'] == 1)
        $_SESSION['admin_mode'] = true;
}

?>

<html>
<head>
    <title>AI Final Project - Index</title>
</head>
<body>
    <h1>Options:</h1>
    <ul>
        <li><a href="newgame.php">Start a new game</a></li>
        <li><a href="resumegame.php">Resume a game</a></li>
    </ul>
</body>
</html>

