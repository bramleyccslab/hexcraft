## Hexcraft project

### Contents

- python
    - mcmc.py: this is the file for the Metropolis-Hastings model for the general sequence learning process.
    - generate.py: some trial code that I used to generate some sequences from a given library.
    - square_game.py: the code for manipulating a square board. A Python replica of a previous master's project.
    - hex_game_1.py: the first version of a hex game.
    - hex_game_2.py: the second (and the current) version of a hex game, with objective shape as goal and can judge whether the objective is reached.
    - solver.py: a script for searching all the possible action chains that could solve a problem within a certain chain length.
- js
    - demo and demo_nocache are original demo sites created by Neil 
    - testing.html: modified version of no_cache in development for use as experiment. Among other things, in this file the caches and their corresponding testing trials are defined. 
    - task.js: actual hexcraft task. The logic behind all actions etc is set up here. 

    List of primitive actions (can also be seen visually in instructions_mockup):
    - A = put a piece in the middle of the board
    - D = delete a piece in the middle of the board 
    - Z = put a cornery shape 
    - X = put a diagonal bar 
    - W = shifts all pieces one space to the west 
    - E = shifts to North-East 
    - S = shifts to South-East 
    - F = flips pieces from SE to NW (and vice versa)
    - R = reflects pieces (same as Flip but the original pieces stay in their places)
    - K / Space = rotates the board clockwise (is coded as 'K' in the actual code)
    - L / Enter = Ends the trial 

 - R
    - demos: this is a file I mostly used to input action sequences to see how they are reflected on the board. There is also a greedy solver that I haven't touched yet. 
    - generate_testsets: generates random caches and then test sets using these caches 
    - custom_cache_test: edited generate_testsets that generates based on custom caches (defined as a manual list of primitives)
    - hex_setup: sets up the board and actions. Actions match the js ones.
        - f$[1] "AddUnit" corresponds to "A"
        - f$[2] "RemoveUnit" corresponds to "D"
        - f$[3] "AddBar" corresponds to "X"
        - f$[4] "AddCorner" corresponds to "Z"
        - f$[5] "RotateClockwise" corresponds to "K"/"Space"
        - f$[6] "Flip" corresponds to "F"
        - f$[7] "Reflect" corresponds to "R"
        - f$[8] "ShiftNE" corresponds to "E"
        - f$[9] "ShiftSE" corresponds to "S"
        - f$[10] "ShiftW" corresponds to "W"


### Experiment 1 Design

Caches and testing trials: 
- There are three caches (pictures can be found in the 'caches' folder):
    - dabone (sort of looks like a bone, achieved via X-K-X)
    - hazard (sort of looks like the hazard symbol, Z-S-A)
    - triangle (A-S-A-E-A)
- Since the triangle is a bit long, there is also another one (TBC) that would also be 3 keys long:
    - Dinopaw (X-Z-D)
- For each cache there are two puzzles that make use of these caches. One medium and one hard:
    - dabone: medium: Z[XKX]W; hard: [XKX]KS[XKX]R
    - hazard: medium: [ZSA][ZSA]R; hard: [ZSA]EE[ZSA]SWXWEK
    - triangle: medium: [ASAEA]E[ASAEA]K; hard: [ASAEA]S[ASAEA]E[ASAEA]
    - dinopaw: medium: [XZD]SRKR; hard: XKSS[XZD]RKK
- These puzzles (and one way to solve them) can be seen in each list that corresponds to their cache

When running the html file now, the sequence of the puzzles goes like this:
- dabone, medium dabone, hard dabone
- hazard, medium hazard, hard hazard
- triangle, medium triangle, hard triangle 

For each puzzle, after three unsuccessful tries the user can move on to the next one. 

## Contributors
 - Haozhe Sun (sunhaozhepy; sunhaozhe050722a@gmail.com)
 - Ana Belan (a.belan@sms.ed.ac.uk)
 - Bonan Zhao (zhaobn; b.zhao@ed.ac.uk)
 - Neil Bramley (neilbramley; neil.bramley@ed.ac.uk)