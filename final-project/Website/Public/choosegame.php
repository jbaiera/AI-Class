<?php

include('connect.php');

if (! isset($_SESSION['user_id']))
    header("Location: index.php");

$user_id = $_SESSION['user_id'];

function displayGames() {
    global $db;

    echo("<form action='resumegame.php' method='get'>");
    
    $stmt = $db->prepare("SELECT game_id, player_1, player_2, game_type FROM games WHERE player_1 = ? or player_2 = ?");
    $stmt->bind_param("ii", $user_id, $user_id);
    $stmt->execute();
    $stmt->bind_result($game_id, $player_1, $player_2, $game_type);
    $stmt->fetch();

    echo($game_type);
    echo($game_id);
    echo($player_1);
    echo($player_2);
    echo($game_id);

    $stmt->close();

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

