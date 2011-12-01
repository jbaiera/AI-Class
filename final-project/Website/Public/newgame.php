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

if (! isset($_SESSION['user_id']) )
    header('Location: index.php');

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

    $game_type = "reversi";
    $game_state = "[[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,1,2,0,0,0],[0,0,0,2,1,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0]]";
    $to_move = 1; //player 1 always moves first
    $winner = 0;

    $stmt = $db->prepare("INSERT INTO games (game_type, player_1, player_2, board_state, to_move, winner) VALUES (?, ?, ?, ?, ?, ?)");
    $stmt->bind_param("siisii", $game_type, intval($player_1), intval($player_2), $game_state, $to_move, $winner);
    $stmt->execute();
    $stmt->bind_result($foo);
    $stmt->fetch();
    $stmt->close();

    header('Location: resumegame.php');
}

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

