# Land of Stuff
The Land of Stuff is a multiplayer online role playing game written in FreeBASIC ([freebasic.net](http://freebasic.net)). Players explore a vast world solo or with friends, battling monsters, gaining loot and completing story quests.

![Land of Stuff](http://wanganzhou.com/images/los/screen26.png)

Full game info at [http://wanganzhou.com/land-of-stuff.html](http://wanganzhou.com/land-of-stuff.html)

## Building From Source
You'll need the fbc FreeBASIC compiler (available at [freebasic.net/get](http://freebasic.net/get)). The GUI programs, client.bas and mapedit.bas, should be compiled with options -s gui and -lang deprecated; for server.bas and archive.bas, compile with -lang deprecated. So to compile all four tools, use this:

    fbc -s gui -lang deprecated client.bas
    fbc -s gui -lang deprecated mapedit.bas
    fbc -lang deprecated server.bas
    fbc -lang deprecated archive.bas

## Running
If you're playing from your own computer, simply run server.exe to start the server, then client.exe to play the game.

If you want to play online:
* If you're hosting the game session, run server.exe. Make sure port 5650 is unblocked on your router.
* If you're connecting to someone else's game session, you need to change the first line of config.ini to point to the host's IP address. Then run client.exe to play the game.

## Customizing
Note that you must run archive.exe after making any changes in the Data\ folder in order to save them in data.los. Otherwise, they will be overwritten when the client unpacks data.los. This is so that you can distribute your customizations easily by simply packing data.los, client.exe and config.ini.

## Using the Map Editor (mapedit.exe)
* Arrow keys - pan around  
* Number keys - change the currently selected tile to this tile (0 through 9)  
* n - New map  
* s - Save map  
* c - New NPC  
* p - Set starting point of players to the currently selected point  
* t - Place a "special" non-numeric tile  
* e - Create enemy at current location  
* d - Delete enemy at current location  
* v - View entire map as a mini-map (enables mini-map mode)  

While in mini-map mode, you have some new commands at your disposal:  
* g - Teleport to location  
* c - Fill a whole area with a certain tile by dragging  
* t - Randomly scatter a tile type onto an area by dragging (e.g. for trees)  
* x - Exit minimap mode  
