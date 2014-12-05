# SiMBTA

## How to Run the Program

1. Compile the program using
`erlc *.erl`
This ensures that all files are up to date before running.
2. Open the erlang shell using erl.
3. Run the following command:
`main:run(“IN.txt”).`
The argument is a string of the file to read for input, which may be changed.
4. Allow the program to run. Once it terminates, the output is stored in outFile.txt.
*In the event that the program does not terminate, remain calm; you have encountered a Heisenbug. Terminate the simulation with Ctrl+C, and abort. Run the simulation again and you are likely to encounter success.*
5. Run a webserver in the main directory of the project. Python 2 makes this easy using the command:
`python -m SimpleHTTPServer`
*Note: The output file will not be able to be read via Javascript unless the file is run on a web server. Do not simply open index.html. And be sure not to run the server in the Viz/ directory.*
6. Open the Viz folder in a browser (most likely at `http://localhost:8000/Viz/`). You may now view the visualization and its associated data.

### Sample Files Provided:
`test.txt` - small sample input file
`IN.txt` - larger sample input file

## Files Overview

- carto.erl: Presents a “map” of the T-lines (only Red line currently) for the other modules to use.
- clock.erl: Maintains the simulation’s concept of time and notifies objects watching when it changes.
- main.erl: Parses input specifications for and runs simulation.
- output.erl: Handles output to send to results file for simulation.
- passenger.erl: Imitates a passenger taking the T. It is either in a station or on a train while in existence.
- station.erl: Imitates a station of the T. It notifies passengers in itself when trains arrive and manages trains passing through.
- train.erl: Acts as a train on the T. It notifies passengers on itself and the stations when arriving at the stations.
- Viz folder
  - index.html: HTML file to display the visualization. Contains the canvas, average form, and other static elements.
  - main.css: Stylesheet to set page layout. Used very minimally as most display code is done using the canvas API.
  - main.js: Contains all code pertaining to loading the file, parsing it, and then displaying the visualization itself.
	Other files contained in the directory are libraries used to support this code.