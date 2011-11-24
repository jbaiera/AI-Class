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

