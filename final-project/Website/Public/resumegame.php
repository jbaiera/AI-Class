<?

include("connect.php");

if (! isset($_SESSION['user_id']))
{
    header('Location: index.php');
}

?>

<html>
<head>
    <title>Let's Play - Reversi</title>
    <meta charset="UTF-8" />
    <meta name="keywords"   content="Student AI Project, artificial intellegence, games, reversi" />
    <meta name"description" content="Play Reversi against the Machine!" />

    <link rel="stylesheet" type="text/css" href="css/style.css" />
</head>

<body>
    <div class="maincontainer">
        <p> Board Goes Here </p>
    </div>
</body>
</html>
