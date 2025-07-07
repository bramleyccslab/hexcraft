Info for the javascript part of the hexcraft stuf

The R files are Neil's code that I have been working on a bit. 
- demos: this is a file I mostly used to input action sequences to see how they are reflected on the board. There is also a greedy solver that I haven't touched yet. 
- generate_testsets: generates random caches and then test sets using these caches 
- custom_cache_test: edited generate_testsets that generates based on custom caches (defined as a manual list of primitives)
- hex_setup: sets up the board and actions. Actions match the js ones.


javascript stuff: 
- demo and demo_nocache are Neil's code 
- testing.html: modified no_cache used for the experiment. Among other things, in this file the caches and their corresponding testing trials are defined. 
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
- Space = rotates the board clockwise (is coded as 'K' in the actual code)
- Enter = locks in the answer 

Caches and testing trials: 
- There are three caches (pictures can be found in the 'caches' folder):
	- dabone (sort of looks like a bone, achieved via X-Space-X)
	- hazard (sort of looks like the hazard symbol, Z-S-A)
	- triangle (A-S-A-E-A)
- Since the triangle is a bit long, there is also another one (yet unconfirmed) that would also be 3 keys long:
	- Dinopaw (X-Z-D)
- For each cache there are two puzzles that are loosely based on using these caches: one medium and one hard
- These puzzles (and how to solve them) can be seen in each list that corresponds to their cache
	- Each puzzle is basically coded as an action sequence that can be used to solve them 

When running the html file now, the sequence of the puzzles goes like this:
- dabone, medium dabone, hard dabone
- hazard, medium hazard, hard hazard
- triangle, medium triangle, hard triangle 

For each puzzle, after three unsuccessful tries the user can move on to the next one. 
