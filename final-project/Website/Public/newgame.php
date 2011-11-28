<?php

include('connect.php');

// ADD LOGIC FOR MAKING NEW GAME HERE

/* games table:
    game_id     : auto_increment
    game_type   : "reversi"
    player_1    : either player or opponent (which is -1 for greedy, player for practice)
    player_2    : either player or opponent (which is -1 for greedy, player for practice)
    start_time  : (null)
    end_time    : (null)
    game_state  : begins as initialGrid

*/

if (isset($_POST['opponent'])) {
    $player = $_SESSION['user_id'];

    $oppString = $_POST['opponent'];
    if ($oppString == "greedy") {
        $opponent = -1;
    } else if ($oppString == "practice") {
        $opponent = $player;
    }

    if ($_POST['player'] == '1') {
        $player_1 = $player;
        $player_2 = $opponent;
    } else {
        $player_1 = $opponent;
        $player_2 = $player;
    }

    // ADD LOGIC TO INSERT GAME IN TABLE

    header('Location: resumegame.php');
}

var_dump($_POST);

?>

<html>
<head>
    <title>AI Final Project - New Game</title>
</head>
<body>
    <h1>New game:</h1>
    <form action="newgame.php" method="post">
    <table>
        <tr>
            <td>Opponent
            <td><select name="opponent">
                    <option value="greedy">Greedy</option>
                    <option value="practice">Practice</option>
                </select>
        <tr>
            <td>Player
            <td><select name="player">
                    <option value="1">Black</option>
                    <option value="2">White</option>
        <tr>
            <td>
            <td><input type="submit" />
    </table>
    </form>
</body>
</html>

