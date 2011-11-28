<?

include('connect.php');

if (! isset($_SESSION['user_id']))
{
    echo('Please log in <a href="login.php">here</a>.');
}
else
{
    echo('You can log out <a href="logout.php">here</a>.');
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

