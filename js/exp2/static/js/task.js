var N = 4 //Hexagon radius (excluding centre, other stuff should scale with this)


//TODO: UPDATE PRIMITIVE KEY MAPPINGS

var empty_state = Array.apply(null, Array(2*N+1))
.map(function () { });
for (var i=0; i<empty_state.length; i++)
{
    empty_state[i] = new Array(2*N+1).fill(0);//Array.apply(null, Array(2*N+1))
    //.map(function () { });
}
var state = _.cloneDeep(empty_state);//Tracks the current active board state of the user
var old_state = _.cloneDeep(empty_state);//Tracks the active board state of the user at t-1
var locked_state = _.cloneDeep(empty_state);//Tracks the locked-in board state of the user
var target = _.cloneDeep(empty_state);//Contains the target state
var target_lock_tmp = _.cloneDeep(empty_state);//For procedurally generating the target state if it involves locking steps
var spillage = 0;//tracks false positives in solutions
var key_times = []; //list for recording time when keys were pressed 
var key_between_times = []; //list for time diffs between key presses
var level_result = []; //list to record the trial that the user has completed and fail / complete
var number_correct = 0;
var trial_result = [];
var action_history = [];
var state_history = [];
var completed = false;

var d //The canvas identifier

var cdv = {NE:[+1, -1, 0], E:[+1, 0, -1], SE:[0, +1, -1], 
    SW:[-1, +1, 0], W:[-1, 0, +1], NW:[0, -1, +1]}//Cube-coordinate directional vectors

var actions = []; //Store the sequence of actions the user performs

//so these are premade caches 
// var library = [[32, 32, 32, 87, 32, 32, 32], //East
//                [32,32,32,32,32,87,32], //NW
//                [32,87,32,32,32,32,32],//SW
//                [65, 87, 65, 69, 82, 69, 65, 83],//Corner65, 87, 65, 69, 82, 65, 83]
//                [65, 82, 65, 83, 69, 83, 69, 65, 82],//Bar65, 87, 82, 65, 83, 83, 69, 69, 65, 87, 82
//                []]; //Store the action sequences they cache

var results = {subjectwise:{},
                tutorials:[[],[],[],[],[],[],[],[],[]],
                trials:[[],[],[],[],[],[],[],[]],
                targets:[[],[],[],[],[],[],[],[]],
                state_history_tutorials:[[],[],[],[],[],[],[],[],[]],
                state_history_tests:[[],[],[],[],[],[],[],[]],};

var pattern = [];
var mode = '';
//For procedural generation etc ORIGINAL: ['s','a','d','w','e','j','i','k','m','n','h','u']; "^[sadwejikmnhuq]*$"
var action_keys = [];
var action_keycodes = [];
var primitive_keycodes = [];
var display_array = [];
var cachable_keycodes = [];
var mid_cache = false;
var action_displays = ['W','A','D','F', '&#8629; ','&#8736;',
                       'E','R','S','Z','X'];
var action_keys =     ['W','A','D','F','L','K', //so L = Enter and K = space 
                       'E','R','S','Z','X'];
if (cache)
{
    primitive_keycodes = [87, 65, 68, 70, 13, 32];
    cachable_keycodes = [69,82,83,90, 88, 67];

    // action_keys =     ['W','A','D','F','L','K'];
    action_keycodes = [ 87, 65, 68, 70, 13, 32];//76, 75
    // action_displays = ['W','A','D','Z', '&#8629;','&#8736;'];

} else {
    primitive_keycodes = [87, 65, 68, 70, 13, 32,
                       69,82,83,90, 88];

    action_keycodes = [87, 65, 68, 70, 13, 32,
                       69,82,83,90, 88];//76, 75

}

const VALID_CODES = new Set(action_keycodes);

const regex = new RegExp("^[QWERASDFZXLK]*$");//For preventing any other button being pressed in input field
// qwerasdfzxclk

//Get any url parameters
var $_GET = {},
    args = location.search.substr(1).split(/&/);
for (var i=0; i<args.length; ++i) {
    var tmp = args[i].split(/=/);
    if (tmp[0] != "") {
        $_GET[decodeURIComponent(tmp[0])] = decodeURIComponent(tmp.slice(1).join("").replace("+", " "));
    }
}

function Start(fpattern)
{

    key_times = [];
    key_between_times = [];
    //trial_result = [{tutorial: tutorial_phase, trial: trialNumber, result: false, challenge: pattern} ]; WHY DO THIS HERE?

    pattern = fpattern;

    state = _.cloneDeep(empty_state);
    old_state = _.cloneDeep(empty_state);
    locked_state = _.cloneDeep(empty_state);
    target = _.cloneDeep(empty_state);
    target_lock_tmp = _.cloneDeep(empty_state);
    spillage = 0;

    actions = [];            
    display_array = [];

    const existingCanvas = document.querySelector('#board-and-cache canvas');
    if (existingCanvas) {
        existingCanvas.remove();
    }



    //Create the canvas
    d = new ROT.Display({width:17, height:9, spacing:3, layout:"hex"});
    var tmp = document.getElementById("board-and-cache");
    tmp.insertBefore(d.getContainer(), document.getElementById("caches"));

    if ($_GET['depth'] === undefined)
    {
        var gen_depth = 20;
    } else {
        var gen_depth = Number($_GET['depth']);//TODO UNSTRING?
    }
    mode = 'pattern';    //?то есть тут указывается паттерн или процедурал?  

    if ($_GET['pattern'] !== undefined)
    {
        if (regex.test($_GET['pattern'].toUpperCase()))
        {
            pattern = $_GET['pattern'].toUpperCase();
            mode = 'challenge';
        }
    }

    if (mode=='procedural')
    {
        target = ProceduralPattern(gen_depth);
    } else {
        target = ChallengePattern(pattern);
        //'djdjdwuuuedjdjdwnnedjdjdwkkk' -previously
        //fxfxfswrwrwrtfxfxfscctfxfxfseeet
        //xkxkxdrrrlxkxkxdsslxkxkxdeeel -- e.g. three circles XKXKXDRRRLXKXKXDSSLXKXKXDEEEL
        //zfzsslzfzccclzfzrrrlzfzkeelzfzkkwwwlzfzkkkkeerrl circle of bones
    }

    //results.condition.push(condition);
    // if (cache)
    // {
    //     results.library_history.push(_.cloneDeep(library));
    // }
    //results.library.push(_.cloneDeep(library));

    //Plot the hexagons onto it
    for (var y = 0; y < (2*N+1); y++) {
        for (var x = y%2; x < 2*(2*N+1); x += 2) {

        //console.log('xy', x, y, 'centered', prx[x], pry[y], oddr_to_cube(Math.floor(prx[x]/2),pry[y]));
        // Note the coordinates, every odd row is offset to the right doubling the number of x coordinates needed

            var these_cube_coords = oddr_to_cube(Math.floor((x-2*N)/2),y-N);

        //If we're inside the hexagon
            if(Math.abs(these_cube_coords[0])<5 & Math.abs(these_cube_coords[1])<5 & Math.abs(these_cube_coords[2])<5)
            {
                //Uncomment to view coordinates
                // var raw_xy_string = x.toString()+','+y.toString();
                // var xy_string = (x-2*N).toString()+','+(y-N).toString();
                var qrs_string = these_cube_coords[0].toString()+','+these_cube_coords[1].toString();//+','+these_cube_coords[2].toString();
                if (target[these_cube_coords[0]+N][these_cube_coords[1]+N]==1)
                {
                    d.draw(x, y, null, null, "#9e9");
                } else {
                    d.draw(x, y, null, null, "#fff");
                }

                if(these_cube_coords[0]==0 & these_cube_coords[1]==0 & these_cube_coords[2]==0){
                    // d.draw(x, y, '+', 'black', 'white');
                    if (target[these_cube_coords[0]+N][these_cube_coords[1]+N]==1)
                    {
                        d.draw(x, y, '+', 'black', "#9e9");
                    } else {
                        d.draw(x, y, '+', 'black', "#fff");
                    }
                }
                // d.draw(x, y-0.2, raw_xy_string, "#f99");
                // d.draw(x, y+0.2, qrs_string, "#9f9");
            }

        }
    }

    // //input.
    // function hexKeyListener(e) {
    //     //Workaround to avoid spacebar scrolling to bottom of page
    //     if (e.keyCode == 32 | e.keyCode == 13) {
    //         e.preventDefault();
    //     }
    //     var code = e.keyCode || e.which;

    //     if (code>=97 && code<=122)
    //     {
    //         code = code-32;//Hack to convert lower case keycodes to upper case
    //     }
    //     var ch = String.fromCharCode(code);
    //     console.log(ch);
    //     console.log("Key char is: " + ch + " Code is: " + code);
    //     Action(code);
    // };
    
    // if (cache)
    // {
    //     //Functionality for using caches
    //     $(".cache-btn").click(function(){
    //         var tmp = this.id
    //         //console.log('clicked', this.id, tmp.charAt(tmp.length-1));
    //         var which_cache = Number(tmp.charAt(tmp.length-1));

    //         // console.log('hihi', actions, which_cache, cachable_keycodes,
    //             // cachable_keycodes[which_cache], actions.includes(cachable_keycodes[which_cache]));

    //         if (!actions.includes(cachable_keycodes[which_cache]))
    //         {
    //             library[which_cache] = _.cloneDeep(actions);
    //             actions = [cachable_keycodes[which_cache]];

    //             UpdateCacheVis(which_cache);
    //             UpdateStringVis();
    //             results.library_history.push(_.cloneDeep(library));
    //             results.library_update_at.push(results.action_history.length);
    //         } else {
    //             alert('Cannot cache a sequence that contains itself!')
    //         }
    //     });

    //     for (let i=0; i<library.length; i++)
    //     {
    //         UpdateCacheVis(i);
    //     }
    // }

    //key listener
    window.removeEventListener("keydown", hexKeyListener);
    window.addEventListener("keydown", hexKeyListener);

    window.focus();

}


function Action(keycode, this_state=state, real=true, midcache = false)
{
    // console.log('action triggered', keycode);
    //https://www.cs.cmu.edu/~pattis/15-1XX/common/handouts/ascii.html

    // ENTER=13     // l=108 L=76
    // SPACE=32   // k=107 K=75

    //If 'real' is false then we are using this for procedurally generating 
    //a target rather than interactively generating a solution


    //if (keycode==81){Undo();}


    if (keycode==76 | keycode==13){Lock(this_state, real);} 
    if (keycode==75 | keycode==32){RotateClockwise(this_state, real);}

    if (keycode==87){Shift('W', this_state, real);}//w

    if (keycode==65){AddUnit(this_state, real);}//a
    if (keycode==68){RemoveUnit(this_state, real);}//d
    if (keycode==70){Flip(axis=1, this_state, real);}


    if (cache)// & real
    {
        if (keycode==69){UseCache(0, this_state, real);}//Shift('E', this_state, real);}//e->EAST
        if (keycode==82){UseCache(1, this_state, real);}//Shift('NE', this_state, real);}//r->NW
        if (keycode==83){UseCache(2, this_state, real, real, real);}//AddCorner(this_state, real);}//s
        if (keycode==90){UseCache(3, this_state, real, real);}//AddBar(this_state, real);}//z
        if (keycode==88){UseCache(4, this_state, real);}//Shift('SW', this_state, real);}//x
        if (keycode==67){UseCache(5, this_state, real);}//Shift('SE', this_state, real);}//c
    } else {
        if (keycode==69){Shift('NE', this_state, real);}    //changed to NE
        if (keycode==82){HexReflect(axis=1, this_state, real);}    //changed to Reflect
        if (keycode==83){Shift('SE', this_state, real);}    //changed to SE
        if (keycode==90){AddCorner(this_state, real);}
        if (keycode==88){AddBar(this_state, real);}
        //if (keycode==67){Shift('SE', this_state, real);}  //unneeded
    }

    if (real & !midcache & (primitive_keycodes.indexOf(keycode)>-1 | [81,75,76].indexOf(keycode)>-1))
    {
        //console.log('got here');
        actions.push(keycode);
        
        action_history.push(keycode);
        state_history.push(_.cloneDeep(this_state));

        //Update the visual (player's) state
        Update();
    }

}



function Update()
{
    // Loop over the raw xy locations
    for (var y = 0; y < 9; y++)
    {
        for (var x = y%2; x < 18; x += 2)
        {
            //Convert to cube coordinate
            var these_cube_coords = oddr_to_cube(Math.floor((x-2*N)/2),y-N);
            // oddr_to_cube(Math.floor(prx[x]/2),pry[y]);

            //If we're inside the hexagon
            if(Math.abs(these_cube_coords[0])<5 & Math.abs(these_cube_coords[1])<5 & Math.abs(these_cube_coords[2])<5)
            {
                //If the square is already locked in...
                if (locked_state[these_cube_coords[0]+N][these_cube_coords[1]+N]==1)
                {
                    //And if the locked in state is correct
                    if (target[these_cube_coords[0]+N][these_cube_coords[1]+N]==1)
                    {
                        d.draw(x, y, null, null, "#090"); //Strong Green
                    } else {
                        //Otherwise
                        d.draw(x, y, null, null, "#900");//Red
                    }
                    if(these_cube_coords[0]==0 & these_cube_coords[1]==0 & these_cube_coords[2]==0){
                    // d.draw(x, y, '+', 'black', 'white');
                    if (target[these_cube_coords[0]+N][these_cube_coords[1]+N]==1)
                    {
                        d.draw(x, y, '+', 'black', "#090");
                    } else {
                        d.draw(x, y, '+', 'black', "#900");
                    }
                }

                } else {
                    //If state is not already not locked in but is part of the target
                    if (target[these_cube_coords[0]+N][these_cube_coords[1]+N]==1)
                    {
                        d.draw(x, y, null, null, "#9e9"); //Light green
                    } else {
                        //Otherwise if unoccupied, non target and not locked in it is white
                       d.draw(x, y, null, null, "white");
                    }

                    if(these_cube_coords[0]==0 & these_cube_coords[1]==0 & these_cube_coords[2]==0){
                    // d.draw(x, y, '+', 'black', 'white');
                    if (target[these_cube_coords[0]+N][these_cube_coords[1]+N]==1)
                    {
                        d.draw(x, y, '+', 'black', "#9e9");
                    } else {
                        d.draw(x, y, '+', 'black', "#fff");
                    }
                }
                }
                
                // If the state is actively occupied
                if (state[these_cube_coords[0]+N][these_cube_coords[1]+N]==1)
                {
                    //And part of the pattern
                    if (target[these_cube_coords[0]+N][these_cube_coords[1]+N]==1)
                    {

                        d.draw(x, y, "•", "black", "#595");//"#595"
                    } else {
                        d.draw(x, y, "•", "black",  "#999");
                    }
                } 
            }
        }
    }

    UpdateStringVis();
}

function UpdateStringVis()
{
    display_array = [];
    // console.log(action_keycodes.concat(cachable_keycodes));
    for (let i=0; i<actions.length; i++)
    {
        for (let j=0; j<action_displays.length; j++)
        {
            if (actions[i]==action_keycodes.concat(cachable_keycodes)[j])
            {
                // console.log(i, j, action_displays[j]);
                display_array.push(action_displays[j])
            }
        }
    }
    //console.log('updatestringvis triggered', display_array.join(''));
    $('#main_focus').html(display_array.join(''));
}

function UpdateCacheVis(cache_n)
{
    var tmp = [];
    for (let i=0; i<library[cache_n].length; i++)
    {
        for (let j=0; j<action_displays.length; j++)
        {
            if (library[cache_n][i]==action_keycodes.concat(cachable_keycodes)[j])
            {
                // console.log(i, j, action_displays[j]);
                tmp.push(action_displays[j])
            }
        }
    }
    //console.log('update cache vis triggered', tmp.join(''));

    $("#cache" + String(cache_n)).html(tmp.join(''));
}

function cube_to_oddr(q,r)
{
    var col = q + (r - (r&1)) / 2;
    var row = r;
    return [col, row];
}

function oddr_to_cube(x,y)
{
    var q = x - (y - (y&1)) / 2;
    var r = y;
    var s = -q-r
    return [q, r, s];
}

function AddUnit(this_state)
{
    //Clone the current state (for undo)
    old_state = _.cloneDeep(this_state);

    //Add unit in centre
    this_state[N][N] = 1;
}

function RemoveUnit(this_state)
{
    //Clone the current state (for undo)
    old_state = _.cloneDeep(this_state);


    //Add unit in centre
    this_state[N][N] = 0;
}

function AddBar(this_state)
{
    //Clone the current state (for undo)
    old_state = _.cloneDeep(this_state);

    //Add horizontal bar
    this_state[N][N-1] = 1;
    this_state[N][N] = 1;
    this_state[N][N+1] = 1;
}

function AddCorner(this_state)
{
    //Clone the current state (for undo)
    old_state = _.cloneDeep(this_state);

    //Add horizontal bar
    this_state[N+1][N] = 1;
    this_state[N][N] = 1;
    this_state[N-1][N+1] = 1;
}


function Shift(dir, this_state)
{
    //Clone the current state
    old_state = _.cloneDeep(this_state);

    var sv = cdv[dir];//Shift vector

    //Loop over all the axial coordinates in the hexagon
    for (var q=-N; q<=N; q++)
    {
        for (var r=-N; r<=N; r++)
        {
            //Calculate s
            var s = -q-r

            //Calculate updated position
            var qprime = q+sv[0]
            var rprime = r+sv[1]
            var sprime = s+sv[2]
            //console.log('qrs', q,r,s, 'qrsprime', qprime, rprime,sprime);
            //If new position is still on the map
            if (Math.abs(qprime)<5 & Math.abs(rprime)<5 & Math.abs(sprime)<5)
            {
                //Write into the new position
                this_state[qprime+N][rprime+N] = old_state[q+N][r+N];
            }
            
            //Zero the cells left behind
            if (sv[0]!=0 & q==(-N*sv[0]))
            {
                // console.log('q zeroing', q, r)
                this_state[q+N][r+N]=0;
            }
            if (sv[1]!=0 & r==(-N*sv[1]))
            {
                // console.log('r zeroing', q, r)
                this_state[q+N][r+N]=0;
            }
            if (sv[2]!=0 & s==(-N*sv[2]))
            {
                // console.log('s zeroing', q, r)
                this_state[q+N][r+N]=0;
            }
        }
    }   

}



function RotateClockwise(this_state, real)
{
    old_state = _.cloneDeep(this_state);
    for (var q=-N; q<=N; q++)
    {
        for (var r=-N; r<=N; r++)
        {
            s=-q-r
            //120 degree rotations
            // var q_rot= r;
            // var r_rot = s;
            // var s_rot = q;
            
            //60 degree rotations
            //xx, yy, zz = -yy, -zz, -xx
            var q_rot= -r;
            var r_rot = -s;
            var s_rot = -q;
            if (Math.abs(q)<=N & Math.abs(r)<=N & Math.abs(s)<=N)
            {
                //Stops things spinning out of the  hex and growing the array dims?
                this_state[q_rot+N][r_rot+N] = old_state[q+N][r+N]; 
            }

        }
    }
}




function Flip(axis=1, this_state)
{
    
    // old_bo = _.cloneDeep(board_orientation);
    // board_orientation[0]=old_bo[0];
    // board_orientation[1]=old_bo[2];
    // board_orientation[2]=old_bo[1];

    old_state = _.cloneDeep(this_state);
    for (var q=-N; q<=N; q++)
    {
        for (var r=-N; r<=N; r++)
        {
            s=-q-r;
            if (axis==1)
            {
                q_flip=s;
                r_flip=r;
                s_flip=q;
                
            } else if (axis==2)
            {
                q_flip=r;
                r_flip=q;
                s_flip=s;
            } else if (axis==3)
            {
                q_flip=q;
                r_flip=s;
                s_flip=r;


            }
           
            if (Math.abs(q)<=N & Math.abs(r)<=N & Math.abs(s)<=N)
            {
                //Stops things spinning out of the  hex and growing the array dims?
                this_state[q_flip+N][r_flip+N] = old_state[q+N][r+N]; 
            }

        }
    }
}

function HexReflect(axis=1, this_state)
{
    old_state = _.cloneDeep(this_state);
    for (var q=-N; q<=N; q++)
    {
        for (var r=-N; r<=N; r++)
        {
            s=-q-r;
            if (axis==1)
            {
                q_flip=s;
                r_flip=r;
                s_flip=q;
                
            } else if (axis==2)
            {

                q_flip=r;
                r_flip=q;
                s_flip=s;
                
            } else if (axis==3)
            {
                q_flip=q;
                r_flip=s;
                s_flip=r;

            }
           
            if (Math.abs(q)<=N & Math.abs(r)<=N & Math.abs(s)<=N)
            {
                //Stops things spinning out of the  hex and growing the array dims?
                this_state[q_flip + N][r_flip + N] = this_state[q_flip + N][r_flip + N] || old_state[q + N][r + N];
            }

        }
    }
}



// function RotateAnticlockwise(this_state, real)
// {
//     old_state = _.cloneDeep(this_state);
//     for (var q=-N; q<=N; q++)
//     {
//         for (var r=-N; r<=N; r++)
//         {
//             s=-q-r
//             var q_rot= s;
//             var r_rot = q;
//             var s_rot = r;
//             console.log(q,r,s,q_rot, r_rot, s_rot);
//             if (Math.abs(q)<=N & Math.abs(r)<=N & Math.abs(s)<=N)
//             {
//                 this_state[q_rot+N][r_rot+N] = old_state[q+N][r+N]; 
//             }

//         }
//     }
// }

function Undo()
{
    //console.log('undoing', state, old_state);
    state = _.cloneDeep(old_state);//Revert to previous state
    actions.pop();//And remove the latest letter from the list
    // (should only be called in interactive mode)
}

var attemptCount = 0;  
var inputLocked = false;


function Lock(this_state, real) 
{
    old_state = _.cloneDeep(this_state);
    var match = true;
    spillage = 0;

    for (var q=-N; q<=N; q++)
    {
        for (var r=-N; r<=N; r++)
        {
            var s = -q-r;
            if (this_state[q+N][r+N]==1)
            {
                if (real)
                {
                    locked_state[q+N][r+N]=1;
                } else {
                    target_lock_tmp[q+N][r+N]=1;
                }

                this_state[q+N][r+N]=0;
            }

            if (real & Math.abs(q)<=N & Math.abs(r)<=N & Math.abs(s)<=N)
            {
                if (locked_state[q+N][r+N] !== target[q+N][r+N])    
                {
                   //console.log('nonmatch', q,r);
                   
                    match = false; 
                }

                if (locked_state[q+N][r+N]==1 & target[q+N][r+N]==0)
                {
                    spillage++
                }
            }
        }
    }

    // console.log("Match =", match);
    // console.log("Locked state:");
    // console.log(JSON.stringify(locked_state));
    // console.log("Target:");
    // console.log(JSON.stringify(target));


    if (real)
    {
        inputLocked = true;
        attemptCount++;

        if (match) {completed=true;}

        if (tutorial_phase)
        {
            var tmp = tutCounter-1;
        } else 
        {
            var tmp = trialNumber;
        }

        const existingControls = document.getElementById("control-buttons");
        if (existingControls) existingControls.remove();

        const btnContainer = document.createElement("div");
        btnContainer.id = "control-buttons";
        btnContainer.style.marginTop = "1em";
        btnContainer.style.alignSelf = "center";
        btnContainer.style.marginBottom = "1em";

        const feedback = document.createElement("p");
        //feedback.textContent = match ? "Puzzle solved. You have earned £0.10!" : "Puzzle not solved, please try again.";
        
        const currentTrial = jsPsych.getCurrentTrial();
        //const isTutorial = currentTrial.type === jsPsychHtmlKeyboardResponse && currentTrial.stimulus.includes("Tutorial");

        if (completed) {
            feedback.textContent = tutorial_phase
                ? "Tutorial step complete."
                : "Puzzle solved. You have earned £0.20!";
        } else {
            feedback.textContent = tutorial_phase
                ? "Tutorial step not complete. Please try again."
                : "Puzzle not solved, please try again.";
        }

        key_between_times.push({key: key_times[0].key, interval: key_times[0].time});   //for the first key pressed it just includes the key code and the output of performance.now

        for (let i = 1; i < key_times.length; i++) {    //for the other ones it calculates the differences in milliseconds
            key_between_times.push({
                key: key_times[i].key,
                interval: key_times[i].time - key_times[i - 1].time
            });
        }

        trial_result = {trial: tmp, attempt: attemptCount, actions:_.cloneDeep(action_history), keys:_.cloneDeep(key_between_times), result:false};
        
        btnContainer.appendChild(feedback);

        const tryAgainBtn = document.createElement("button");
        const moveOnBtn = document.createElement("button");
        tryAgainBtn.textContent = "Try Again";

        tryAgainBtn.onclick = function ()
        {
            inputLocked = false;
            trial_result.result = false;
            ResetBoard();
        };
        btnContainer.appendChild(tryAgainBtn);


        if (completed || (attemptCount >= 5 && !tutorial_phase)) {
            if (match) {
                trial_result.result = true;

                // if (!tutorial_phase){
                //     number_correct = number_correct +1;
                // }
            } else{
                trial_result.result = false;
            }
            btnContainer.appendChild(moveOnBtn);
        }

            
            moveOnBtn.textContent = "Move On";
            moveOnBtn.style.marginLeft = "1em";
        
            moveOnBtn.onclick = function ()
            {
                jsPsych.finishTrial();
                attemptCount = 0;

                if (!tutorial_phase && completed){
                    number_correct = number_correct +1;
                }

                completed = false;
                ResetBoard();
            };



        document.body.prepend(btnContainer);
        
        if (tutorial_phase)
        {
          results.tutorials[tmp].push(_.cloneDeep(trial_result));
          results.state_history_tutorials[tmp].push(_.cloneDeep(state_history));
        } else {
          results.targets[tmp].push(_.cloneDeep(target));//Store the target pattern
          results.trials[tmp].push(_.cloneDeep(trial_result));
          results.state_history_tests[tmp].push(_.cloneDeep(state_history));
        }

        console.log('pushed trial results');

    }//Real
}//Lock

function ResetBoard() {
    state = _.cloneDeep(empty_state);
    old_state = _.cloneDeep(empty_state);
    locked_state = _.cloneDeep(empty_state);
    actions = [];
    spillage = 0;
    display_array = [];
    action_history = [];
    state_history = [];
    key_times = [];
    key_between_times = [];

    Update();  
    //document.getElementById("main_focus").innerHTML = "";
  
    const container = document.getElementById("progressContainer");

    container.innerHTML = "";
    for (let i = 0; i < NUM_SEGMENTS; i++) {
        const segment = document.createElement("div");
        Object.assign(segment.style, SEGMENT_STYLE);
        segment.classList.add("segment");
        container.appendChild(segment);
    }
    currentIndex = 0;
    
    const controls = document.getElementById("control-buttons");
    if (controls) controls.remove();

}


// function RandomPattern()
// {
//     var target_pattern = _.cloneDeep(state);

//     for (var x=0; x<(2*N+1); x++)
//     {
//         for (var y=0; y<(2*N+1); y++)
//         {
//             var these_cube_coords = oddr_to_cube(Math.floor((x-2*N)/2),y-N);
//             //oddr_to_cube(Math.floor(prx[x]/2),pry[y]);
//             if (Math.abs(these_cube_coords[0])<=N & Math.abs(these_cube_coords[1])<=N & Math.abs(these_cube_coords[2])<=N)
//             {
//                 if (Math.random()>.5)
//                 {
//                     target_pattern[x][y]=1;
//                 }
//             }
//         }  
//     }
//     return target_pattern;
// }


function ChallengePattern(sequence)
{
    var target_pattern = _.cloneDeep(state);

    var seq_arr = Array.from(sequence);
    for (var step =0; step<sequence.length; step++)
    {
        var this_key = seq_arr.shift();
        var tmp = action_keys.indexOf(this_key);
        

        Action(primitive_keycodes.concat(cachable_keycodes)[tmp], target_pattern, false);
    }

    for (var q=-N; q<=N; q++)
    {
        for (var r=-N; r<=N; r++)
        {
            if (target_lock_tmp[q+N][r+N]==1)
            {
                target_pattern[q+N][r+N]=1;
            } 
        }
    }
    //console.log('target pattern ', target_pattern); 
    return target_pattern;
}


// function ProceduralPattern(depth)
// {
//     var target_pattern = _.cloneDeep(state);
//     var generation_procedure = [];
//     for (var step =0; step<depth; step++)
//     {
//         this_key = ROT.RNG.getItem(action_keycodes.concat(cachable_keycodes));
//         //Remove the cachable keycode concatenation to use prior
//         generation_procedure.push(this_key);
//         Action(this_key, target_pattern, false);
//     }

//     for (var q=-N; q<=N; q++)
//     {
//         for (var r=-N; r<=N; r++)
//         {
//             if (target_lock_tmp[q+N][r+N]==1)
//             {
//                 target_pattern[q+N][r+N]=1;
//             } 
//         }
//     }
//     //console.log('gen proc', generation_procedure, action_keycodes.concat(primitive_keycodes));
//     var tmp_key_array = [];
//     for (var i=0; i<generation_procedure.length; i++)
//     {
//         var tmp= action_keycodes.concat(cachable_keycodes).indexOf(generation_procedure[i]);
//         tmp_key_array.push(action_keys[tmp]);
//         // console.log(tmp_key_array, tmp);
//     }
//     pattern = tmp_key_array.join('');

//     return target_pattern;
// }



// function UseCache(n, this_state, this_real)
// {    
//    //Read the cached pattern TODO KEEP THIS NOT IN THE STRING
//    var cache_pattern = _.cloneDeep(library[n]);//Array.from($('#cache' + n).text());
//    //console.log('before expanding:', cache_pattern);

//    //Get rid of any nesting before the display process begins
   
//    while (cache_pattern.includes(cachable_keycodes[0]) |
//           cache_pattern.includes(cachable_keycodes[1]) | 
//           cache_pattern.includes(cachable_keycodes[2]) | 
//           cache_pattern.includes(cachable_keycodes[3]) | 
//           cache_pattern.includes(cachable_keycodes[4]) | 
//           cache_pattern.includes(cachable_keycodes[5]) )
//    {
//         for (var i=0; i<cache_pattern.length; i++)
//         {
//             for (var j=0; j<cachable_keycodes.length; j++)
//             {
//                 if (cache_pattern[i]==cachable_keycodes[j])
//                 {
//                     cache_pattern.splice(i, 1);
//                     insertArrayAt(cache_pattern, i, library[j]);
//                     break;
//                 };
//             }
//         }
//    }
//    //How long is the fully expanded string?
//    var cpl = cache_pattern.length;

//    //Feed the string to the Action function with short timeouts so you can see it play out
//    //console.log('after expanding:', cache_pattern);


//    while (cache_pattern.length>0)
//    {
//         cur_keycode = cache_pattern.shift();
//         Action(cur_keycode, this_state=this_state, real=this_real, midcache = true);
//    }
//    if (this_real)
//    {
//        Update();
//        actions.push(cachable_keycodes[n]);
//        UpdateStringVis();
//    }

//     // if (cache_pattern.length>0)
//     // {
//     //     console.log('used cache: ', n);
//     //     var i = 1;                  //  set your counter to 1
//     //     var i_max = cache_pattern.length;
        
//     //     function ActLoop() {         //  create a loop function
//     //         setTimeout(function() {   //  call a setTimeout when the loop is called
//     //             cur_keycode = cache_pattern.shift();
//     //             Action(cur_keycode);   //  your code here
//     //             i++;                    //  increment the counter
//     //             if (i <= i_max) {           //  if the counter is less then original expanded pattern length, 
//     //               ActLoop();             //  .. call the loop function again 
//     //             }  else {
//     //                 actions.splice(actions.length - cpl, cpl);
//     //                 //actions.push(cachable_keycodes[n]);
//     //                 UpdateStringVis();
//     //                 //$('#main_focus').text(actions.join(''));//

//     //                 console.log(n, 'original actions', actions, 'removed', tmp);
//     //             }                     //  ..  setTimeout()
//     //         }, 50)
//     //     }

//     //     ActLoop();
//     // }   

// } 


function insertArrayAt(array, index, arrayToInsert) {
    Array.prototype.splice.apply(array, [index, 0].concat(arrayToInsert));
}

function save_data()    // TODO: highlighting this too 
{
    var now = new Date();
    results_str = JSON.stringify(results);

    //jspsych_results_str = JSON.stringify(jsPsych.data.get());
    // console.log('full results: ', results);

    jQuery.ajax({
        url: './static/php/save_data.php', //breaking this thing for now to test
        type:'POST',
        data:{results:results_str},   //, jsPsychResults:jspsych_results_str
        success:function(data)
        {
            console.log('Sent data to database');
            //alert('Nailed it in: ' + actions.length + ' actions, with ' + spillage + ' mistakes!');
            
        },
        error:function(xhr, status, error)
        {
            //Just print out what comes back if it doesn't work
            console.log(xhr, status, error);
            //alert('Data not saved! Please email bramleylab@ed.ac.uk with the following information: ', xhr, status, error);
            save_data_fallback();
        }
    })
}

function save_data_fallback() {

    var fallback_data = JSON.stringify(results);
    var fallback_filename = prolific_id + "_FALLBACK.csv";  

    jQuery.ajax({
        url: './static/php/save_data_to_csv.php',
        type: 'POST',
        data: {filename: fallback_filename, filedata: fallback_data},
        success: function(data) {
            console.log('Fallback save successful');
        },
        error: function(xhr, status, error) {
            console.error('Fallback save also failed:', xhr, status, error);
            //alert('Unable to save your data. Please email bramleylab@ed.ac.uk');
        }
    });
}



function Hint1() {
    $('#hint1').show();
    $('#hintbtn1').hide();
}

function Hint2() {
    $('#hint2').show();
    $('#hintbtn2').hide();
}

function hexKeyListener(e) {
        if (inputLocked) {
        //console.log("input locked");
        return;
        }


        //Workaround to avoid spacebar scrolling to bottom of page
        if (e.keyCode == 32 | e.keyCode == 13) {
            e.preventDefault();
        }
        var code = e.keyCode || e.which;

        if (action_history.length==0 && code==13)
        {
            alert('You must take some actions first!');
            return;
        }

        if (code>=97 && code<=122)
        {
            code = code-32;//Hack to convert lower case keycodes to upper case
        }
        var ch = String.fromCharCode(code);

        if(ch == " "){  //workaround so that space's ch is 'K' instead of ' ' for readability
            ch = "K";
        };

        if(ch == "\r"){  //workaround so that enter's ch is 'L' instead of ' ' for readability
            ch = "L";
        };

        if (!VALID_CODES.has(code)) return;

        var stamp = performance.now();  //this should record timestamp at which the key was pressed 
        key_times.push({key:ch, time:stamp});
        // console.log('key: ', ch, 'time: ', stamp);

        //console.log(ch);
        // console.log("Key char is: " + ch + " Code is: " + code);

        if(action_history.length == 29){
            //Lock(state, true);
            code=13;
        }

        Action(code);
    };


// Tutorial stuff

function tutorialText(step) {
    switch(step){
        case 1:
            var tutText = '<p style="font-size: 1em; text-align: left; margin-bottom: 20px;">First, put a single piece in the middle of the Board by pressing <b>A</b> on your keyboard.</p>' + 
            '<p style="font-size: 1em;  text-align: left;">Then, press <b>Enter</b> to lock it in and move on.</p>';
            break;
        case 2:
            var tutText = '<p style="font-size: 1em; text-align: left; margin-bottom: 20px;">Pressing <b>W</b> moves all pieces on the Board one space to the <b>West</b>.</p>' +
            '<p style="font-size: 1em; text-align: left; margin-bottom: 20px;">Pressing <b>E</b> moves all pieces on the Board one space to the <b>North-East</b>.' +
            '</p><p style="font-size: 1em; text-align: left; margin-bottom: 20px;">Pressing <b>S</b> moves all pieces on the Board one space to the <b>South-East</b>.' +
            '</p><p style="font-size: 1em; text-align: left; margin-bottom: 20px;">Press <b>A</b> then <b>W,E,S</b> to positino a tile one space to the <b>South-West</b> of the centre.</p>';
            break;
        case 3:
            var tutText = '<p style="font-size: 1em; text-align: left;">Pressing <b>Z</b> puts a <b>Corner shape</b> in the middle of the Board. Place it and lock it in to move on.</p>';
            break;
        case 4:
            var tutText = '<p style="font-size: 1em; text-align: left;">Pressing <b>X</b> puts a <b>diagonal bar</b> in the middle of the Board. Place it and lock it in to move on.</p>';
            break;
        case 5:
            var tutText = '<p style="font-size: 1em; text-align: left; margin-bottom: 20px;">The placed pieces can overlap with each other.</p>' +
            '<p style="font-size: 1em;text-align: left; ">Put both a <b>Corner</b> and a <b>Bar</b> to combine them.</p>';
            break;
        case 6:
            var tutText = '<p style="font-size: 1em; text-align: left; margin-bottom: 20px;">Tiles can be <b>deleted</b> from the middle of the board by pressing <b>D</b>.</p>' +
            '<p style="font-size: 1em; text-align: left;">Place a <b>Bar</b> and <b>delete</b> its middle tile to move on.';
            break;
        case 7:
            var tutText = '<p style="font-size: 1em; text-align: left; margin-bottom: 20px;">Pressing <b>Space</b> <b>rotates</b> all the pieces clockwise.</p>' +
            '<p style="font-size: 1em; text-align: left;">Place a <b>Corner</b> and <b>Rotate</b> it until it matches the target shape.</p>';
            break;
        case 8:
            var tutText = '<p style="font-size: 1em; text-align: left; margin-bottom: 20px;">Pressing <b>F</b> makes the board <b>Flip</b> from West to East (and vice-versa).</p>' +
            '<p style="font-size: 1em; text-align: left;">Place a <b>Corner</b> and <b>Flip</b> it to match the target shape.</p>';
            break;
        case 9:
            var tutText = '<p style="font-size: 1em; text-align: left; margin-bottom: 20px;">Pressing <b>R</b> serves a similar function to <b>Flip</b>, ' +
            'but instead the board <b>Reflects</b>, so the original pieces are copied on the other side of the board.</p>' +
            '<p style="font-size: 1em; text-align: left;">Place a <b>Piece</b> and <b>Move</b> it to match one of the target locations, then <b>Reflect</b> the board.</p>';
            break;
    }
    document.getElementById("tutText").innerHTML = tutText;
}



function shuffle(array) {
  for (let i = array.length - 1; i > 0; i--) {
    let j = Math.floor(Math.random() * (i + 1));
    [array[i], array[j]] = [array[j], array[i]];
  }
}