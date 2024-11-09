## __Project Description__

Goal: Make a Tic-Tac-Toe console application in any programming language I desire.

This project is designed to demonstrate the implementation of Tic-Tac-Toe as a console-based game in Clojure, with features such as player vs. computer gameplay, move validation, and statistics tracking.

#### __MVP Goals__
  
The minimum viable product (MVP) for this project includes the following features:

    Take a users name as input, and hold it in state throughout all the games that a player decides to continue playing.

    Basic move validation: Ensure that players can only place their marks in unoccupied spaces. Player entry may only be integers between 0 and 2. [0 1 2]

    Win condition checks: The game ends when either player wins or when the board is full (draw). In this case the computer has been optimized to win or draw every game.

    Simple text-based interface: The game board and current game status are displayed in the console.

    Game replay option: After each game, players can choose to play again. Statistics for the matches are saved in local storage and the user is provided a running tally of their current statistics.

#### __Stretch Goals__

    ✅Game replay option: After each game, players can choose to play again. Statistics for the matches are saved in local storage and the user is provided a running tally of their current statistics.

    ✅Game replay option: After each game, players can choose to play again. Statistics for the matches are saved in local storage and the user is provided a running tally of their current statistics.

    Long term records in a database?: One optional stretch goal could be to save player statistics and records into a cloud or local database. It could take the form of a sql database, csv, json/edn file, or any other preferred storage medium.
    
    SPA (Single Page Web Application): It would be nice to jump from a console application to a web browser and have an ineractive user interface to play the game through. If I was to expand this application I would use Helix CLJS to display the front end on the browser.
  
#### __How to Play__

    Start the Game: Run the application in your console.

    Input Moves: When it's your turn, input the row and column numbers (0, 1, or 2) where you want to place your mark.

    Game Progression: The game will continue until one player wins or the game is a draw. (Tip: Only the computer can win.)

    Replay: After each game, you will be asked if you want to play again. Type "yes" to play another round or "no" to exit the game.

#### __Technologies Used__

    Clojure: The programming language used to build this application. Clojure is a functional, general-purpose language that runs on the Java Virtual Machine (JVM).

    Console: The game runs in the terminal/console and displays the game state using text output.

    Minimax algorithm (for AI opponent): A decision-making algorithm used to evaluate the best possible move for the AI.

      Clojure.Test: I used the core clojure testing library to include some basic test cases for all of my functions.

#### __Installation__

    Ensure you have Clojure installed on your machine. You can follow the official installation guide.

    Clone this repository to your local machine:

git clone https://github.com/your-username/tic-tac-toe.git

cd tic-tac-toe

Run the game using:

    clojure -m tic-tac-toe.core

    This will start the Tic-Tac-Toe game in your terminal.

##### __Other Potential Future Improvements__

    Multiplayer: Extend the game to support network play where players can play from different machines.

    Game Logging: Implement logging to track all moves and decisions made during each game session for future analysis.

###### Thanks for checking out my application!