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
    <h1>Games to resume:</h1>
    <a href="resumegame.php">(temporary link for testing</a>
</body>
</html>

