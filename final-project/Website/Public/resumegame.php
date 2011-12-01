<?php

include("connect.php");

if (! isset($_SESSION['user_id'])) {
    header('Location: index.php');
} else if (!isset($_GET['game_id'])) {
    header('Location: choosegame.php');
}

$user_id = $_SESSION['user_id'];
$game_id = $_GET['game_id'];

$stmt = $db->prepare("SELECT player_1, player_2, board_state, to_move FROM games WHERE (player_1 = ? or player_2 = ?) and game_id = ?");
$stmt->bind_param("ssi", $user_id, $user_id, $game_id);
$stmt->execute();
$stmt->bind_result($player_1, $player_2, $board_state, $to_move);
$stmt->store_result();
$stmt->fetch();

$num_rows = $stmt->num_rows();

if ($num_rows == 0) {
    // no such game found
    header('Location: choosegame.php');
} else if ($num_rows > 1) {
    // shit, we have too many games! no error handling, just run away
    header('Location: choosegame.php');
}

if ($player_1 == $player_2) {
    $player = $to_move;
    $opponenet = ($to_move == 1)?2:1;
} else if ($user_id == $player_1) {
    $player = 1;
    $opponent = 2;
} else {
    $player = 2;
    $opponent = 1;
}

if ($player == $to_move) {
    $my_move = true;
} else {
    $my_move = false;
}

$program_location = '../..';

$valid_moves_string = shell_exec("$program_location/findvalid $game_id $player");

$blackscore = 0;
$whitescore = 0;

//initialize the board
for ($i = 0; $i < 8; $i++)
    for ($j = 0; $j < 8; $j++) {
        $board[$i][$j] = $board_state[2 + 2*$j + 18*$i];
        if ($board[$i][$j] == 1) {
            $blackscore += 1;
        } else if ($board[$i][$j] == 2) {
            $whitescore += 1;
        }
    }

$num_valid_moves = (strlen($valid_moves_string) - 1) / 6;

for ($i = 0; $i < $num_valid_moves; $i++) {
    // magic! this is from interpreting the string findvalid prints out
    $x = $valid_moves_string[6*$i + 2];
    $y = $valid_moves_string[6*$i + 4];
    $board[$x][$y] = 3;
}

if (isset($_GET['xpos']) && isset($_GET['ypos'])) {
    $x = $_GET['xpos'];
    $y = $_GET['ypos'];
    if ($board[$x][$y] == 3 && $my_move == true) {
        shell_exec("$program_location/humanplay \"($x,$y)\" \"$to_move\" \"$game_id\"");
        header("Location: resumegame.php?game_id=$game_id");
    } else {
        // ignore it
    }
}


function displayGameBoard($boardstate)
{
    global $my_move, $game_id;

    for ($i = 0; $i < 8; $i++)
    {
        echo('<tr>');
        for ($j = 0; $j < 8; $j++)
        {
            if ($boardstate[$i][$j] == 3 && $my_move == true) {
                $add_link = true;
            } else {
                $add_link = false;
            }

            echo("<td>");
            
            if ($add_link == true) {
                echo("<a href='resumegame.php?game_id=$game_id&xpos=$i&ypos=$j'>");
            }

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

            if ($add_link == true)
                echo("</a>");
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
        <h3>Scoreboard</h3>
        <p>Black: <?php echo($blackscore); ?></p>
        <p>White: <?php echo($whitescore); ?></p>
        <p>It is <?php if ($to_move == 1) echo("black"); else echo("white"); ?> to move.</p>
        </div>
    </div>
</body>
</html>

