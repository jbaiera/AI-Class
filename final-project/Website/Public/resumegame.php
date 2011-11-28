<?

include("connect.php");

if (! isset($_SESSION['user_id']))
{
    header('Location: index.php');
}

$ai_location = '../..';

// echo (system("pwd"));

for ($i = 0; $i < 8; $i++)
    for ($j = 0; $j < 8; $j++)
        $board[$i][$j] = 0;

$board[3][4] = 1;
$board[4][5] = 2;
$board[5][6] = 3;

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

        <table align="center" border="1">

            <?php

            for ($i = 0; $i < 8; $i++)
            {
                echo('<tr>');
                for ($j = 0; $j < 8; $j++)
                {
                    echo('<td>');
                    if ($board[$i][$j] == 0) {
                        echo('<img src="img/blank.jpeg" />');
                    } else if ($board[$i][$j] == 1) {
                        echo('<img src="img/black.jpeg" />');
                    } else if ($board[$i][$j] == 2) {
                        echo('<img src="img/white.jpeg" />');
                    } else if ($board[$i][$j] == 3) {
                        echo('<img src="img/valid.jpeg" />');
                    }
                }
            }

            ?>

        </table>

    </div>
</body>
</html>

