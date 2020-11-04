# Media agents in DIAL

## Instructions on how to run the code
Download the latest version of Netlogo from https://ccl.northwestern.edu/netlogo/6.1.1/
Our implementation is on the master branch in code/ and is called Finale.nlogo
Once you open NetLogo, you can open the file Finale.nlogo to run our program. 
In order to initialise the simulation, press the "setup" button. To start the simulation, press "go".

## Original implementation

Link to the DIAL implementation by Dykstra:
https://www.comses.net/codebases/3418/releases/1.0.0/


## Documentation

Documentation is now in a seperate subrepository hosted on overleaf. Fetch all subrepositories using:

    # When cloning for the first time use the --recursive flag
    git clone --recursive git@github.com:GeorgGroenendaal/DMAS-Media-agents-and-social-outcomes.git
    
    # When already cloned use
    cd DMAS-Media-agents-and-social-outcomes
    
    ## Setup
    1. Download and install Netlogo version 6.1.1 https://ccl.northwestern.edu/netlogo/6.1.1/
    2. cd to DMAS-Media-agents-and-social-outcomes/code/
    3. Run Final.nlogo to start the simulation
    
    ## How it works
    The model starts with a random distribution agents with random evidence and importance values. 
    The evidence ranges from -1 to 1. 1 means pro a proposition (white); -1 is contra (black) and 0 (blue) is no clue. 
    Importance ranges from 0 to 1 Peoples move in the direction of a patch that is most similar to their opinion or paint the world. 
    Similarity is defined by the carthesian distance: sqrt( (e1 - e2)^2 + (i1 - i2)^2). 
    People make announcements and the patches in the neighbourhood of the announcers (environment), remember that information for some time.
    
    Media agent can also make announcements, the main difference with the peope agent is that they are not attackble and therefore not able to argue. 
    The position of an media agent is fixed (brown grid). Also, the initial reputation of a media agent is much larger than an people agent.
    They have also a higher reach than the normal agent. The the media agent can be initialized with the number-of-media slider.
    
    
    ## How to use it
    - The NUMBER slider sets the number of peoples. 
    - The LOUDNESS slider determines the distance an announcement travels away from the announcer.
    - The SETUP button initializes the model, and GO runs the model.
    - While running the world only shows the evidence values. To se the importance of the message: press SHOW-IMPORTANCE to see those values for patches and agents. 
    Press SHOW-EVIDENCE toswitch back to the evidence presentation. 
    Hint: Not pressing SHOW-EVIDENCE followed by the GO button repaints the patches with the evidence color, but the turtles keep the importance representation.        
    - The ALARM monitor should always be 0. If the value is different then there is code executed, in the announce procedure, that should not have been executed. 
    Maybe this is useful for you too Corinna.
    
    - LOUDNESS - determines the distance an announcement travels away from the announcer.
    - STEPSIZE - the maximum distance an agent travels in one round.
    - VISUAL-MEMORY - determines how far an agent look foor the best place to go. 
    - UNDIRECTEDNESS - the wobling angle, i.e. the inverse of the chance to get to the best place. 
    - CHANCE-WALK - determines the chance an agent makes a step instead of doing something else.
    - CHANCE-ANNOUNCE - determines the chance an agent makes an announcement. The only other option is to walk in the direction of similar patch. 
    - CHANCE-LEARN-BY-NEIGHBOUR - the agent copies the evidence and importance values from a neighbour. 
    - CHANCE-LEARN-BY-MEMORY - the agent copies the evidence and importance values from the patch it is standing on.
    - CHANCE-MUTATION - a random change of the evidence and importance values of an agent.
    - FORGETSPEED - decreases the time a patch remembers it’s contents.
    - NEUTRAL-IMPORTANCE - The default background importance of the patches that forgotten all information. 
        If this value is 0, the agent with opposite opinions may prefer each other over a neutral patch because of the distance in importance.
    
    ## MEDIA-AGENT PROPERTIES
    - MEDIA-OPINION-MEAN - the mean of the distribution of media opinion/evidence from which we sample individual media opinions. 
    (It’s not the mean of the media opinion once we have sampled them. The sample mean may deviate from the population mean)
    - MEDIA-OPINION-STD - The spread of media opinion/evidence from which we sample
    - MEDIA-IMPACT - A user-defined parameter that modifies the influence of media agents on people agents. 
        Its values vary from “no influence at all” to “very strong influence”.
    - PERCEIVED-BIAS-STD - The standard deviation of the perceived media bias distribution from which we sample values for PMD for individual agents
   
