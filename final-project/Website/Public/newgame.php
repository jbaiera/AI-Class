<?php

include('connect.php');

// ADD LOGIC FOR MAKING NEW GAME HERE


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
            <td><select>
                    <option value="greedy">Greedy</option>
                    <option value="practice">Practice</option>
                </select>
        <tr>
            <td>
            <td><input type="submit" />
    </table>
    </form>
</body>
</html>

