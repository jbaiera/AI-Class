<?php

include("connect.php");

if (! isset($_SESSION['user_id'])) {
    header('Location: index.php');
} else if (!isset($_GET['game_id'])) {
    header('Location: choosegame.php');
}

$user_id = $_SESSION['user_id'];
$game_id = $_GET['game_id'];

// TODO: add check to ensure that the game belongs to the user
$stmt = $db->prepare("SELECT game_id, player_1, player_2, board_state, to_move FROM games WHERE player_1 = ? or player_2 = ? and game_id = ?");
$stmt->bind_param("ssi", $user_id, $user_id, $game_id);
$stmt->execute();

$num_rows = $stmt->num_rows();

if ($num_rows == 0) {
    // no such game found
    header('Location: choosegame.php');
} else if ($num_rows > 1) {
    // shit, we have too many games! no error handling, just run away
    header('Location: choosegame.php');
}





$ai_location = '../..';

//initialize the board
for ($i = 0; $i < 8; $i++)
    for ($j = 0; $j < 8; $j++)
        $board[$i][$j] = 0;

//board testing
$board[3][4] = 1;
$board[4][5] = 2;
$board[5][6] = 3;

function displayGameBoard($boardstate)
{
    for ($i = 0; $i < 8; $i++)
    {
        echo('<tr>');
        for ($j = 0; $j < 8; $j++)
        {
            echo('<td>');
            if ($boardstate[$i][$j] == 0) {
                $imgsrc = 'src="img/blank.jpeg"';
            } else if ($boardstate[$i][$j] == 1) {
                $imgsrc = 'src="img/black.jpeg"';
            } else if ($boardstate[$i][$j] == 2) {
                $imgsrc = 'src="img/white.jpeg"';
            } else if ($boardstate[$i][$j] == 3) {
                $imgsrc = 'src="img/valid.jpeg"';
            }
            echo("<img class=\"space\" height=\"50\" width=\"50\" $imgsrc />");
        }
    }
}


?>

<html>
<head>
    <title>AI Final Project - Resume Play</title>
    <meta charset="UTF-8" />
    <meta name="keywords"   content="Student AI Project, artificial intellegence, games, reversi" />
    <meta name"description" content="Play Reversi!" />

    <link rel="stylesheet" type="text/css" href="css/style.css" />
</head>

<body>
    <div class="maincontainer">
        <div class="boardcontainer">
            <div class="boardoutline">
                <h3 class="boardname">Reversi</h3>
                <table class="reversiboard" border="1">
                    <?php displayGameBoard($board); ?>
                </table>
            </div>
        </div>
        <div class="rightside">
        </div>
    </div>
</body>
</html>

