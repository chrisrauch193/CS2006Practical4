# CS2006Practical4
CS2006 Practical 4: Haskell 2

The running instructions for Gomoku are as follows: 

Before running any of the commands, run cabal build.
There are 2 modes which the user can play, local mode, or in network multiplayer.

To play local mode run: 

cabal run gomoku 

There are multiple command line options which are available to start the game. 

The command flags are as follows: 

For the local game, all flags can be used apart from -i for IP and -p for the Port number. 

  -t INTEGER    --target=INTEGER  The target for the game.  Positive integer. Default to 5.
  -s INTEGER    --size=INTEGER    Size of board.  Positive integer. Default to 19.
  -c COLOUR     --colour=COLOUR   The player colour.  Black or White. Default is Human Player plays first and the game always starts with Black playing first 
  -a BOOLEAN    --AI=BOOLEAN      Whether to play an AI.  True or False. Defaults to True
  -d DIFFICULTY --AI Type=DIFFICULTY   Type of AI to play against.  Basic Defensive or Aggressive.
  -r RULE       --rule=RULE       The variation of Gomoku to play.  the options are: Standard, Handicap, or Pente. Default game mode is Standard. Handicap adds the rule of three and three and four and four to the player playing as black.
  -l FLOAT      --limit=FLOAT     Time limit. Insert a time limit for the human players moves Positive floating point number. Standard limit is 20 seconds. 
  -g BOOLEAN    --enable time=BOOLEAN  Enable time.  True or False.
  -i STRING     --IP=STRING       IP address for multiplayer. Argument for the Client
  -p STRING     --port=STRING     Port for multiplayer. Argument for the Client and Server, Default is 12345.
  -h            --help            Show help.

E.g. an example game could be setup as follows with command flags (Double Dashes after the executable name to allow command line flags). 

cabal run gomoku -- -s 10 -t 4 -a False -r Pente 

Plays a local game between 2 humans, on a 10 by 10 board, where the target is 4 pieces in a row, and the version of the game being played is Pente. 

The other option is to play a network multiplayer. 

The command flags which are compatible with the network multiplayer are: 
-t to change the target, 
-s to change the size of the board.
-i to change the IP address which the client connects to 
-i to change the Port on which the client connects, or the port on which the server starts. 

There are 2 executables which need to be run for network multiplayer to run. 

The server needs to be started to allow 2 clients to connect to it to allow them to play against each other. 

To start the server, run: 

cabal run server -- -p [PORT NUMBER IF DIFFERENT TO STANDARD]

To then connect to the server, with 2 different clients run:

cabal run client -- -i [IPADDRESS] -p [PORT NUMBER IF NOT STANDARD] 

If they have successfully connected, you will get an message saying that you have recieved the initial board on the command line. If you are the first client to connect, you play as black, and then when the second client connects, they play as White, under standard game rules. 


When in game, there are also ingame options which can be utilised: 
u : is undo the last move you played
n : starts a new game with the same settings 
p : pauses the game 
s : saves the current game to file 
l : loads the game from the save file if one exists
h : gives a hint as to where to play

You can only use hints in Network Play


 
 


