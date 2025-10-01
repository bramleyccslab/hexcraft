var N = 4 //Hexagon radius (excluding centre, other stuff should scale with this)

//TODO: UPDATE PRIMITIVE KEY MAPPINGS
console.log('cache?', cache);

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

var d //The canvas identifier

var cdv = {NE:[+1, -1, 0], E:[+1, 0, -1], SE:[0, +1, -1], 
    SW:[-1, +1, 0], W:[-1, 0, +1], NW:[0, -1, +1]}//Cube-coordinate directional vectors

var actions = []; //Store the sequence of actions the user performs
var library = [[32, 32, 32, 119, 32, 32, 32], //East
               [32,32,32,32,32,119,32], //NE
               [32,119,32,32,32,32,32],//SW
               [65, 119, 65, 101, 114, 101, 65, 115],//Corner65, 119, 65, 101, 114, 65, 115]
               [65, 114, 65, 115, 101, 115, 101, 65, 114],//Bar65, 119, 114, 65, 115, 115, 101, 101, 65, 119, 114
               []]; //Store the action sequences they cache

var results = {target:[], performance:[], action_history:[], state_history:[], library_history:[], library_update_at:[0]};
//TMP BOB FOR DEMO
// library = [[32, 32, 119, 32, 32, 32,32], //SE
//                [32,32,32,32,119,32,32], //NW
//                [32,119,32,32,32,32,32],//SW
//                [65, 119, 65, 119, 65, 101, 65, 101, 65, 114, 65],//Corner65, 119, 65, 101, 114, 65, 115]
//                [122, 100, 119, 100, 114, 100],//Bar65, 119, 114, 65, 115, 115, 101, 101, 65, 119, 114
//                []];
var pattern = [];
var mode = '';
//For procedural generation etc ORIGINAL: ['s','a','d','w','e','j','i','k','m','n','h','u']; "^[sadwejikmnhuq]*$"
var action_keys = [];
var action_keycodes = [];
var primitive_keycodes = [];
var display_array = [];
var cachable_keycodes = [];
var mid_cache = false;
var action_displays = ['w','A','d','f', '&#8629; ','&#8736;',
                       'e','r','s','z','x','c'];
var action_keys =     ['w','A','d','f','l','k',
                       'e','r','s','z','x','c'];
if (cache)
{
    primitive_keycodes = [119, 65, 100, 102, 13, 32];
    cachable_keycodes = [101,114,115,122, 120, 99];

    // action_keys =     ['w','A','d','f','l','k'];
    action_keycodes = [ 119, 65, 100, 102, 13, 32];//108, 107
    // action_displays = ['w','A','d','z', '&#8629;','&#8736;'];

    console.log(cachable_keycodes);

} else {
    primitive_keycodes = [119, 65, 100, 102, 13, 32,
                       101,114,115,122, 120, 99];

    action_keycodes = [119, 65, 100, 102, 13, 32,
                       101,114,115,122, 120, 99];//108, 107




}

const regex = new RegExp("^[qwerasdfzxclk]*$");//For preventing any other button being pressed in input field

//Get any url parameters
var $_GET = {},
    args = location.search.substr(1).split(/&/);
for (var i=0; i<args.length; ++i) {
    var tmp = args[i].split(/=/);
    if (tmp[0] != "") {
        $_GET[decodeURIComponent(tmp[0])] = decodeURIComponent(tmp.slice(1).join("").replace("+", " "));
    }
}

function Start()
{
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
    mode = 'procedural';

    if ($_GET['pattern'] !== undefined)
    {
        if (regex.test($_GET['pattern']))
        {
            pattern = $_GET['pattern'];
            mode = 'challenge';
        }
    }

    if (mode=='procedural')
    {
        target = ProceduralPattern(gen_depth);
    } else {
        target = ChallengePattern(pattern);
        console.log(pattern);
        //'djdjdwuuuedjdjdwnnedjdjdwkkk' -previously
        //fxfxfswrwrwrtfxfxfscctfxfxfseeet
        //xkxkxdrrrlxkxkxdsslxkxkxdeeel -- e.g. three circles
        //zfzsslzfzccclzfzrrrlzfzkeelzfzkkwwwlzfzkkkkeerrl circle of bones
    }

    console.log(gen_depth, 'mode', mode, ' ', pattern);
    results.target = _.cloneDeep(target);//Store the target pattern

    if (cache)
    {
        results.library_history.push(_.cloneDeep(library));
    }

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
                // d.draw(x, y-0.2, raw_xy_string, "#f99");
                // d.draw(x, y+0.2, qrs_string, "#9f9");
            }

        }
    }

    //input.
    window.addEventListener("keypress", function(e) {
        //Workaround to avoid spacebar scrolling to bottom of page
        if (e.which == 32) {
            e.preventDefault();
        }
        var code = e.charCode;
        var ch = String.fromCharCode(code);
        console.log(ch);
        console.log("Key char is: " + ch + " Code is: " + code);
        Action(code);
    });
    
    if (cache)
    {
        //Functionality for using caches
        $(".cache-btn").click(function(){
            var tmp = this.id
            console.log('clicked', this.id, tmp.charAt(tmp.length-1));
            var which_cache = Number(tmp.charAt(tmp.length-1));

            //  $('#cache' + this.id.slice(-1)).text()
            library[which_cache] = _.cloneDeep(actions);
            actions = [cachable_keycodes[which_cache]];
            //library[which_cache].push(actions.pop());
            UpdateCacheVis(which_cache);
            UpdateStringVis();
            results.library_history.push(_.cloneDeep(library));
            results.library_update_at.push(results.action_history.length);
        // var cache_pattern = Array.from($('#cache' + this.id.slice(-1)).text());
        });
    }

    for (let i=0; i<library.length; i++)
    {
        UpdateCacheVis(i);
    }

}


function Action(keycode, this_state=state, real=true, midcache = false)
{
    // console.log('action triggered', keycode);
    //https://www.cs.cmu.edu/~pattis/15-1XX/common/handouts/ascii.html

    // ENTER=13     // l=108
    // SPACE=32   // k=107

    //If 'real' is false then we are using this for procedurally generating 
    //a target rather than interactively generating a solution
    if (keycode==113){Undo();}


    if (keycode==108 | keycode==13){Lock(this_state, real);}
    if (keycode==107 | keycode==32){RotateClockwise(this_state, real);}

    if (keycode==119){Shift('W', this_state, real);}//w

    if (keycode==65){AddUnit(this_state, real);}//a
    if (keycode==100){RemoveUnit(this_state, real);}//SW
    if (keycode==102){Flip(axis=1, this_state, real);}


    if (cache & real)
    {
        if (keycode==101){UseCache(0);}//Shift('E', this_state, real);}//e->EAST
        if (keycode==114){UseCache(1);}//Shift('NE', this_state, real);}//r->NW
        if (keycode==115){UseCache(2);}//AddCorner(this_state, real);}//s
        if (keycode==122){UseCache(3);}//AddBar(this_state, real);}//z
        if (keycode==120){UseCache(4);}//Shift('SW', this_state, real);}//x
        if (keycode==99){UseCache(5);}//Shift('SE', this_state, real);}//c
    } else {
        if (keycode==101){Shift('E', this_state, real);}
        if (keycode==114){Shift('NW', this_state, real);}
        if (keycode==115){Shift('SW', this_state, real);}
        if (keycode==122){AddCorner(this_state, real);}
        if (keycode==120){AddBar(this_state, real);}
        if (keycode==99){Shift('SE', this_state, real);}
    }

    if (real & !midcache & (primitive_keycodes.indexOf(keycode)>-1 | [113,107,108].indexOf(keycode)>-1))
    {
        console.log('got here');
        actions.push(keycode);
        
        results.action_history.push(_.cloneDeep(actions));
        results.state_history.push(_.cloneDeep(this_state));

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

                } else {
                    //If state is not already not locked in but is part of the target
                    if (target[these_cube_coords[0]+N][these_cube_coords[1]+N]==1)
                    {
                        d.draw(x, y, null, null, "#9e9"); //Light green
                    } else {
                        //Otherwise if unoccupied, non target and not locked in it is white
                       d.draw(x, y, null, null, "white");
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
    console.log('updatestringvis triggered', display_array.join(''));
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
    console.log('update cache vis triggered', tmp.join(''));

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

function AddUnit(this_state, real)
{
    //Clone the current state (for undo)
    old_state = _.cloneDeep(this_state);

    //Add unit in centre
    this_state[N][N] = 1;
}

function RemoveUnit(this_state, real)
{
    //Clone the current state (for undo)
    old_state = _.cloneDeep(this_state);


    //Add unit in centre
    this_state[N][N] = 0;
}

function AddBar(this_state, real)
{
    //Clone the current state (for undo)
    old_state = _.cloneDeep(this_state);

    //Add horizontal bar
    this_state[N][N-1] = 1;
    this_state[N][N] = 1;
    this_state[N][N+1] = 1;
}

function AddCorner(this_state, real)
{
    //Clone the current state (for undo)
    old_state = _.cloneDeep(this_state);

    //Add horizontal bar
    this_state[N+1][N] = 1;
    this_state[N][N] = 1;
    this_state[N-1][N+1] = 1;
}


function Shift(dir, this_state, real)
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
            // console.log('qrs', q,r,s, 'qrsprime', qprime, rprime,sprime);
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

function Flip(axis=1, this_state, real)
{
    old_state = _.cloneDeep(this_state);
    for (var q=-N; q<=N; q++)
    {
        for (var r=-N; r<=N; r++)
        {
            s=-q-r;
            if (axis==1)
            {
                q_flip=q;
                r_flip=s;
                s_flip=r;
                
            } else if (axis==2)
            {
                q_flip=s;
                r_flip=r;
                s_flip=q;
            } else if (axis==3)
            {
                q_flip=r;
                r_flip=q;
                s_flip=s;
            }
           
            if (Math.abs(q)<=N & Math.abs(r)<=N & Math.abs(s)<=N)
            {
                //Stops things spinning out of the  hex and growing the array dims?
                this_state[q_flip+N][r_flip+N] = old_state[q+N][r+N]; 
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
    console.log('undoing', state, old_state);
    state = _.cloneDeep(old_state);//Revert to previous state
    actions.pop();//And remove the latest letter from the list
    // (should only be called in interactive mode)
}

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
                if (locked_state[q+N][r+N]==0 & target[q+N][r+N]==1)
                {
                   console.log('nonmatch', q,r);
                    match = false; 
                }

                if (locked_state[q+N][r+N]==1 & target[q+N][r+N]==0)
                {
                    spillage++
                }
            }
        }
    }

    if (real)
    {
        if (match)
        {
            save_data();
        }
    }

}

function RandomPattern()
{
    var target_pattern = _.cloneDeep(state);

    for (var x=0; x<(2*N+1); x++)
    {
        for (var y=0; y<(2*N+1); y++)
        {
            var these_cube_coords = oddr_to_cube(Math.floor((x-2*N)/2),y-N);
            //oddr_to_cube(Math.floor(prx[x]/2),pry[y]);
            if (Math.abs(these_cube_coords[0])<=N & Math.abs(these_cube_coords[1])<=N & Math.abs(these_cube_coords[2])<=N)
            {
                if (Math.random()>.5)
                {
                    target_pattern[x][y]=1;
                }
            }
        }  
    }
    return target_pattern;
}


function ChallengePattern(sequence)
{
    var target_pattern = _.cloneDeep(state);

    var seq_arr = Array.from(sequence);
    for (var step =0; step<sequence.length; step++)
    {
        var this_key = seq_arr.shift();
        var tmp = action_keys.indexOf(this_key);
        console.log('keycode', this_key, tmp, primitive_keycodes.concat(cachable_keycodes));

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

    return target_pattern;
}


function ProceduralPattern(depth)
{
    var target_pattern = _.cloneDeep(state);
    var generation_procedure = [];
    for (var step =0; step<depth; step++)
    {
        this_key = ROT.RNG.getItem(action_keycodes.concat(cachable_keycodes));
        //Remove the cachable keycode concatenation to use prior
        generation_procedure.push(this_key);
        Action(this_key, target_pattern, false);
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
    console.log('gen proc', generation_procedure);
    var tmp_key_array = [];
    for (var i=0; i<generation_procedure.length; i++)
    {
        var tmp= action_keycodes.concat(primitive_keycodes).indexOf(generation_procedure[i]);
        tmp_key_array.push(action_keys[tmp])
    }
    pattern = tmp_key_array.join('');

    return target_pattern;
}



function UseCache(n)
{    
   //Read the cached pattern TODO KEEP THIS NOT IN THE STRING
   var cache_pattern = _.cloneDeep(library[n]);//Array.from($('#cache' + n).text());
   console.log('before expanding:', cache_pattern);

   //Get rid of any nesting before the display process begins
   
   while (cache_pattern.includes(cachable_keycodes[0]) |
          cache_pattern.includes(cachable_keycodes[1]) | 
          cache_pattern.includes(cachable_keycodes[2]) | 
          cache_pattern.includes(cachable_keycodes[3]) | 
          cache_pattern.includes(cachable_keycodes[4]) | 
          cache_pattern.includes(cachable_keycodes[5]) )
   {
        for (var i=0; i<cache_pattern.length; i++)
        {
            for (var j=0; j<cachable_keycodes.length; j++)
            {
                if (cache_pattern[i]==cachable_keycodes[j])
                {
                    cache_pattern.splice(i, 1);
                    insertArrayAt(cache_pattern, i, library[j]);
                    break;
                };
            }
        }
   }
   //How long is the fully expanded string?
   var cpl = cache_pattern.length;

   //Feed the string to the Action function with short timeouts so you can see it play out
   console.log('after expanding:', cache_pattern);


   while (cache_pattern.length>0)
   {
        cur_keycode = cache_pattern.shift();
        Action(cur_keycode, this_state=state, real=true, midcache = true);
   }
   Update();
   actions.push(cachable_keycodes[n]);
   UpdateStringVis();
    // if (cache_pattern.length>0)
    // {
    //     console.log('used cache: ', n);
    //     var i = 1;                  //  set your counter to 1
    //     var i_max = cache_pattern.length;
        
    //     function ActLoop() {         //  create a loop function
    //         setTimeout(function() {   //  call a setTimeout when the loop is called
    //             cur_keycode = cache_pattern.shift();
    //             Action(cur_keycode);   //  your code here
    //             i++;                    //  increment the counter
    //             if (i <= i_max) {           //  if the counter is less then original expanded pattern length, 
    //               ActLoop();             //  .. call the loop function again 
    //             }  else {
    //                 actions.splice(actions.length - cpl, cpl);
    //                 //actions.push(cachable_keycodes[n]);
    //                 UpdateStringVis();
    //                 //$('#main_focus').text(actions.join(''));//

    //                 console.log(n, 'original actions', actions, 'removed', tmp);
    //             }                     //  ..  setTimeout()
    //         }, 50)
    //     }

    //     ActLoop();
    // }   

} 


function insertArrayAt(array, index, arrayToInsert) {
    Array.prototype.splice.apply(array, [index, 0].concat(arrayToInsert));
}

function save_data()
{
    var now = new Date();

    results.performance = {steps:actions.length, errors:spillage, cache:cache};
    results_str = JSON.stringify(results);

    jQuery.ajax({
        url: './static/php/save_data.php',
        type:'POST',
        data:{results:results_str},
        success:function(data)
        {
            console.log('Sent data to database');
            alert('Nailed it in: ' + actions.length + ' actions, with ' + spillage + ' mistakes!');
        },
        error:function(xhr, status, error)
        {
            //Just print out what comes back if it doesn't work
            console.log(xhr, status, error);
        }
    })
}

function Hint1() {
    $('#hint1').show();
    $('#hintbtn1').hide();
}

function Hint2() {
    $('#hint2').show();
    $('#hintbtn2').hide();
}

