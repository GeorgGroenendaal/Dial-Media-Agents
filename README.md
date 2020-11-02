# Media agents in DIAL

## Instructions on how to run the code
Download the latest version of Netlogo from https://ccl.northwestern.edu/netlogo/references.shtml.
Our implementation is on the master branch in code/code/ and is called Dial1.0.nlogo.
Once you open NetLogo, you can open the file Dial1.0.nlogo to run our program. 
In order to initialise the simulation, press the "setup" button. To start the simulation, press "go".

## Original implementation

Link to the DIAL implementation by Dykstra:
https://www.comses.net/codebases/3418/releases/1.0.0/


## Documentation

Documentation is now in a seperate subrepository hosted on overleaf. Fetch all subrepositories using:

    # When cloning for the first time use the --recursive flag
    git clone --recursive git@github.com:GeorgGroenendaal/DMAS-Media-agents-and-social-outcomes.git
    
    # When already cloned use
    cd project_root
    git submodule update --init --recursive

    # After update the subrepository like this
    cd documentation
    git pull
    cd ..
    git add .submodules
    git commit

## Setup
In the folder DMAS-Media-agents-and-social-outcomes/code/ you can find two netlogo files: Dial1.0.nlogo (the orginal Dysktra implementation) and Final.nlogo (Our contribution)
For running the Final.nlogo file you have to install netlogo 6.1.1

**NOTE** Right now docs are only available to members of the oerleaf repository, after finishing of the project this will be made available.

