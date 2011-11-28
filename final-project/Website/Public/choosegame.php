<?php

include('connect.php');

if (! isset($_SESSION['user_id'])
    header("Location: index.php");


function displayGames() {
    echo("<form action='resumegame.php' method='get'>");

    // fetch all games for the user id

    echo("</form>");
}

?>

<html>
<head>
    <title>AI Final - Choose a Game</title>
</head>
<body>
    <h1>Games:</h1>
    <?php displayGames(); ?>

