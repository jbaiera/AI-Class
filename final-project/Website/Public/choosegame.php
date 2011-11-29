<?php

include('connect.php');

if (! isset($_SESSION['user_id']))
    header("Location: index.php");

$user_id = $_SESSION['user_id'];

function displayGames() {
    global $db, $user_id;

    echo("<ul>");

    $stmt = $db->prepare("SELECT game_id, player_1, player_2, game_type FROM games WHERE player_1 = ? or player_2 = ?");
    $stmt->bind_param("ii", $user_id, $user_id);
    $stmt->execute();
    $stmt->bind_result($game_id, $player_1, $player_2, $game_type);
    $stmt->store_result();

    $num_rows = $stmt->num_rows();

    for ($i = 0; $i < $num_rows; $i++) {
        $stmt->fetch();

        if ($user_id == $player_1) {
            $color = "black";
            $human = 1;
            $opponent = $player_2;
        } else {
            $color = "white";
            $human = 2;
            $opponent = $player_1;
        }

        if ($player_1 == $player_2) {
            $color = 'both';
            $against = ' for practice';
        } else {
            $against = ' against ';
            if ($opponent = -1) {
                $against .= "greedy AI";
            } else {
                $against .= "unknown player";
            }
        }

        echo("<li>");
        echo("<a href='resumegame.php?game_id=$game_id'>Game as " . $color . $against . "</a>");
    }

    $stmt->close();

    echo("</ul>");
}

?>

<html>
<head>
    <title>AI Final - Choose a Game</title>
</head>
<body>
    <h1>Games:</h1>
    <?php displayGames(); ?>

