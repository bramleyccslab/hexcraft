var N = 4 //Hexagon radius (excluding centre, other stuff should scale with this)

//TODO: UPDATE PRIMITIVE KEY MAPPINGS
console.log('cache?', cache);

var empty_state = Array.apply(null, Array(2 * N + 1))
    .map(function () { });
for (var i = 0; i < empty_state.length; i++) {
    empty_state[i] = new Array(2 * N + 1).fill(0);//Array.apply(null, Array(2*N+1))
    //.map(function () { });
}
var state = _.cloneDeep(empty_state);//Tracks the current active board state of the user
var old_state = _.cloneDeep(empty_state);//Tracks the active board state of the user at t-1
var locked_state = _.cloneDeep(empty_state);//Stores the locked-in board state of the user (end of trial)
var prelocked_state = _.cloneDeep(empty_state);//Stores the locked-in board state of the user (end of trial)

var target = _.cloneDeep(empty_state);//Contains the target state
//var target_lock_tmp = _.cloneDeep(empty_state);//For procedurally generating the target state if it involves locking steps
var spillage = 0;//tracks false positives in solutions
var board_orientation = [2, -4, 2];
var d //The canvas identifier
var player = 1;//
var players = 3;//
var player_colours = Array.from({ length: players }, (_, i) =>
    `hsl(${Math.round(15 + i * 360 / players) % 360}, 70%, 60%)`
);

var cdv = {
    NE: [+1, -1, 0], E: [+1, 0, -1], SE: [0, +1, -1],
    SW: [-1, +1, 0], W: [-1, 0, +1], NW: [0, -1, +1]
}//Cube-coordinate directional vectors

var actions = []; //Store the sequence of actions the user performs
var spec_actions = []; //Store the sequence of actions the user performs before they commit to them
var library = Array(players);
for (var i = 0; i < players; i++) {
    library[i] = _.cloneDeep([[111, 111],
    [97, 119, 97, 119, 115, 97],
    [97, 119, 97, 119],//
    [98, 119, 98, 98]]);
    //Pre-populated libraries for testing. Each library is a list of action sequences,
    // each action sequence is a list of keycodes.
    // Currently 4 caches per player, with some example sequences in them.
}
var lib_public = [false, false, true, true];

var results = { target: [], performance: [], action_history: [], state_history: [], player_history: [], complete_action_history: [], complete_state_history: [], library_history: [], library_update_at: [0] };

var attemptCount = 0;
var pattern = [];
var mode = '';
//For procedural generation etc ORIGINAL: ['s','a','d','w','e','j','i','k','m','n','h','u']; "^[sadwejikmnhuq]*$"
var action_keys = [];
var action_keycodes = [];
// var primitive_keycodes = [];
var display_array = [];
var cachable_keycodes = [];
var mid_cache = false;
var action_displays = ['a', 'd', 'o', 'r', 'w', 'b', 'c', 'e', 's'];//What order?

var keysPressed = {};

if (cache) {
    // 87=W  119=w
    // 65=A 97=a
    // 68=D 100=d
    // 70=F 102=f
    // 13=ENTER
    // 32=SPACE -> 111-o
    // 69=E 101=e
    // 82=R 114=r
    // 83=S 115=s
    // 90=Z 122=z
    // 88=X 120=x
    // 67=C 99=ca
    // 86=V 118=v

    action_keycodes = [97, 100, 111, 114, 119]; //Hard coded actions  
    cachable_keycodes = [98, 99, 101, 115]; //Keys that can be mapped to the library. Currently b,c,e,s. Could be extended if desired.
    //console.log(cachable_keycodes);

} else {
    action_keycodes = [97, 100, 111, 114, 119, 98, 99, 101, 115];
}

const regex = new RegExp("^[WERASDFZXCLK]*$");//For preventing any other button being pressed in input field
// qwerasdfzxclk

//Get any url parameters
var $_GET = {},
    args = location.search.substr(1).split(/&/);
for (var i = 0; i < args.length; ++i) {
    var tmp = args[i].split(/=/);
    if (tmp[0] != "") {
        $_GET[decodeURIComponent(tmp[0])] = decodeURIComponent(tmp.slice(1).join("").replace("+", " "));
    }
}

function Start() {
    //Name player 1 at the top
    html_text = `Ready <b><span style="color: ${player_colours[player - 1]}">Player ${player}</span></b>`;
    $('#player-header').html(html_text);

    $('#restart-task').prop('disabled', true);  // disable
    $('#restart-action').prop('disabled', true);
    $('#submit-action').prop('disabled', true);

    $('#cache-btn2v').prop('disabled', true);
    $('#cache-btn2va').prop('disabled', true);
    $('#cache-btn3v').prop('disabled', true);
    $('#cache-btn3va').prop('disabled', true);


    //Create the canvas
    d = new ROT.Display({ width: 17, height: 9, spacing: 3, layout: "hex" });
    var tmp = document.getElementById("board-and-cache");
    tmp.insertBefore(d.getContainer(), document.getElementById("caches"));

    if ($_GET['depth'] === undefined) {
        var gen_depth = 10;
    } else {
        var gen_depth = Number($_GET['depth']);//TODO UNSTRING?
    }
    mode = 'procedural';

    if ($_GET['pattern'] !== undefined) {
        if (regex.test($_GET['pattern'].toUpperCase())) {
            pattern = $_GET['pattern'].toUpperCase();
            mode = 'challenge';
        }
    }

    if (mode == 'procedural') {
        target = ProceduralPattern(gen_depth);
    } else {
        target = ChallengePattern(pattern);
        console.log(pattern);
        //'djdjdwuuuedjdjdwnnedjdjdwkkk' -previously
        //fxfxfswrwrwrtfxfxfscctfxfxfseeet
        //xkxkxdrrrlxkxkxdsslxkxkxdeeel -- e.g. three circles XKXKXDRRRLXKXKXDSSLXKXKXDEEEL
        //zfzsslzfzccclzfzrrrlzfzkeelzfzkkwwwlzfzkkkkeerrl circle of bones
    }

    console.log(gen_depth, 'mode', mode, ' ', pattern);
    results.target = _.cloneDeep(target);//Store the target pattern

    if (cache) {
        results.library_history.push(_.cloneDeep(library));
    }

    board_orientation = [2, -4, 2];

    //Plot the hexagons onto it
    for (var y = 0; y < (2 * N + 1); y++) {
        for (var x = y % 2; x < 2 * (2 * N + 1); x += 2) {

            // console.log('xy', x, y, 'centered', oddr_to_cube(Math.floor((x-2*N)/2),y-N));
            // Note the coordinates, every odd row is offset to the right doubling the number of x coordinates needed

            var these_cube_coords = oddr_to_cube(Math.floor((x - 2 * N) / 2), y - N);

            //If we're inside the hexagon
            if (Math.abs(these_cube_coords[0]) < 5 & Math.abs(these_cube_coords[1]) < 5 & Math.abs(these_cube_coords[2]) < 5) {
                //Uncomment to view coordinates
                // var raw_xy_string = x.toString()+','+y.toString();
                // var xy_string = (x-2*N).toString()+','+(y-N).toString();
                var qrs_string = these_cube_coords[0].toString() + ',' + these_cube_coords[1].toString();//+','+these_cube_coords[2].toString();
                // If this is part of the target pattern, colour it light green, otherwise white.
                if (target[these_cube_coords[0] + N][these_cube_coords[1] + N] == 1) {
                    d.draw(x, y, null, null, "#9e9");
                    if (these_cube_coords[0] == 0 & these_cube_coords[1] == 0) {
                        // Crosshair in centre (if no target there)
                        d.draw(x, y, '+', 'black', '#9e9');
                    }

                    if (these_cube_coords[0] == board_orientation[0] &
                        these_cube_coords[1] == board_orientation[1] &
                        these_cube_coords[2] == board_orientation[2])//these_cube_coords[0]==these_cube_coords[2])
                    {
                        console.log(these_cube_coords, these_cube_coords[1], -N, these_cube_coords[1] == -N);
                        d.draw(x, y, '•', '#e55', '#9e9');
                    }


                } else {
                    d.draw(x, y, null, null, "#fff");
                    if (these_cube_coords[0] == 0 & these_cube_coords[1] == 0) {
                        // Crosshair in centre (if no target there)
                        d.draw(x, y, '+', 'black', '#fff');
                    }


                    if (these_cube_coords[0] == board_orientation[0] &
                        these_cube_coords[1] == board_orientation[1] &
                        these_cube_coords[2] == board_orientation[2]) {
                        d.draw(x, y, '•', '#e55', '#fff');
                    }
                }
                // d.draw(x, y-0.2, raw_xy_string, "#f99");
                // d.draw(x, y+0.2, qrs_string, "#9f9");


            }

        }
    }



    // document.addEventListener('keydown', (event) => {
    //    keysPressed[event.key] = true;
    // });


    // document.addEventListener('keyup', (event) => {
    //     delete this.keysPressed[event.key];

    //     // if (!keysPressed.Shift)
    //     // {
    //     Update();
    //     // }

    //  });


    //input.
    window.addEventListener("keypress", function (e) {


        //Workaround to avoid spacebar scrolling to bottom of page
        if (e.which == 32 | e.which == 13) {
            e.preventDefault();
        }
        var code = e.charCode;

        if (code >= 65 && code <= 90) {
            code = code + 32;//Hack to convert upper case keycodes to lower case
        }
        var ch = String.fromCharCode(code);
        console.log(ch);
        console.log("Key char is: " + ch + " Code is: " + code);

        if (action_keycodes.includes(code) || cachable_keycodes.includes(code)) {
            Action(code);
        }

        // if (keysPressed.Shift)
        // {
        //    // console.log('prevented action keysPressed', keysPressed, keysPressed.Shift);
        //     speculative_state = _.cloneDeep(state);
        //     Action(code, this_state=speculative_state, real=false, midcache=false, speculative=true);
        // } else{

        // }

    });

    if (cache) {
        //Functionality for using caches
        $(".cache-btn").click(function () {
            var tmp = this.id
            console.log('clicked', this.id, tmp.charAt(tmp.length - 1));
            var which_cache = Number(tmp.charAt(tmp.length - 1));

            console.log('updating cache', actions, spec_actions, player, which_cache, cachable_keycodes,
                cachable_keycodes[which_cache], actions.includes(cachable_keycodes[which_cache]));

            if (!spec_actions.includes(cachable_keycodes[which_cache])) {
                if (lib_public[which_cache]) {
                    for (var i = 0; i < players; i++) {
                        library[i][which_cache] = _.cloneDeep(spec_actions);
                    }

                } else {
                    library[player - 1][which_cache] = _.cloneDeep(spec_actions);
                }

                //spec_actions = [cachable_keycodes[which_cache]];//Immediately replaces the seq with the cache-key?


                //UpdateStringVis();
                results.library_history.push(_.cloneDeep(library));
                results.library_update_at.push(results.complete_action_history.length);
                results.complete_action_history.push('CACHE' + which_cache + 'P' + player);1

                UpdateCacheVis(which_cache, player);
                Clear();
            } else {
                alert('You cannot cache a sequence that contains itself!')
            }
        });

        for (let i = 0; i < library[player - 1].length; i++) {
            UpdateCacheVis(i, player);
        }
    }


}

function Submit() {
    //Current player locks in their contribution and play moves on
    actions = _.cloneDeep(actions.concat(spec_actions))
    results.action_history.push(_.cloneDeep(spec_actions));
    results.complete_action_history.push('SUBMIT'+player);
    results.state_history.push(_.cloneDeep(state));
    results.player_history.push(player);
    prelocked_state = _.cloneDeep(state);
    spec_actions = [];

    player = player % players + 1;//Toggle player 1-players
    //document.getElementById("player-header").textContent = "Ready Player " + player;
    html_text = `Ready <b><span style="color: ${player_colours[player - 1]}">Player ${player}</span></b>`;
    $('#player-header').html(html_text);

    console.log('actions', results.action_history, 'states', results.state_history, spec_actions, actions);
    Update();

    if (cache) {
        for (let i = 0; i < library[player - 1].length; i++) {
            UpdateCacheVis(i, player);
        }
    }

    //$('#restart-task').prop('disabled', true);
    $('#restart-action').prop('disabled', true);
    $('#submit-action').prop('disabled', true);
    $('#pass-action').prop('disabled', false);

    Check(state);
}

function Clear() {
    //Current player clears their contribution and reverts to how it was when they received control
    results.complete_action_history.push('RESET'+player);
    spec_actions = [];
    state = _.cloneDeep(prelocked_state);
    Update();

    $('#restart-task').prop('disabled', true);
    $('#restart-action').prop('disabled', true);
    $('#submit-action').prop('disabled', true);
    $('#pass-action').prop('disabled', false);
}

function Action(keycode, this_state = state, real = true, midcache = false, speculative = true) {
    // console.log('action triggered', keycode);
    //https://www.cs.cmu.edu/~pattis/15-1XX/common/handouts/ascii.html

    // ENTER=13     // l=108 L=76
    // SPACE=32   // k=107 K=75A

    //If 'real' is false then we are using this for procedurally generating 
    //a target rather than interactively generating a solution
    // if (keycode==81){Undo();}


    // if (keycode==108 | keycode==13){Lock(this_state, real);} //L/ENTER LOCK
    if (keycode == 75 | keycode == 111) { RotateClockwise(this_state); } //K/SPACE rotate

    if (keycode == 119) { Shift('W', this_state); }//w

    if (keycode == 97) { AddUnit(this_state); }//a
    if (keycode == 100) { RemoveUnit(this_state); }//d
    // if (keycode==70){Flip(axis=1, this_state);}//f
    if (keycode == 114) { HexReflect(axis = 1, this_state); }    //r

    if (cache)// & real
    {
        if (keycode == 98) { UseCache(0, this_state, real, player); }//b        
        if (keycode == 99) { UseCache(1, this_state, real, player); }//c
        if (keycode == 101) { UseCache(2, this_state, real, player); }//e
        if (keycode == 115) { UseCache(3, this_state, real, player); }//s
        // if (keycode==122){UseCache(2, this_state);}//z
        // if (keycode==120){UseCache(3, this_state);}//z
        //if (keycode==118){UseCache(5, this_state);}//v
    } else {
        if (keycode == 101) { Shift('NE', this_state); }    //e
        if (keycode == 115) { Shift('SE', this_state); }    //s
        if (keycode == 98) { AddBar(this_state); }//b
        if (keycode == 99) { AddCorner(this_state); }//c
        //if (keycode==67){Shift('SE', this_state, real);}  //unneeded
    }

    // console.log('got here', keycode);
    if (real & !midcache)// & (primitive_keycodes.indexOf(keycode)>-1 | [75,108].indexOf(keycode)>-1))
    {
        $('#restart-task').prop('disabled', false);
        $('#restart-action').prop('disabled', false);
        $('#submit-action').prop('disabled', false);
        $('#pass-action').prop('disabled', true);

        // console.log('got here inside', keycode);
        spec_actions.push(keycode);

        results.complete_action_history.push(keycode);
        results.complete_state_history.push(_.cloneDeep(this_state));

        //Update the visual (player's) state
        Update();
    }

    if (real & midcache)
    {
        results.complete_action_history.push('I'+keycode);
    }

    // if (speculative)
    // {
    //     // console.log('speculative behaviour');
    //     SpeculativeUpdate(this_state);

    //     results.complete_action_history.push(_.cloneDeep(actions));
    //     results.complete_state_history.push(_.cloneDeep(this_state));
    // }
}



function Update() {
    // Loop over the raw xy locations
    for (var y = 0; y < 9; y++) {
        for (var x = y % 2; x < 18; x += 2) {
            //Convert to cube coordinate
            var these_cube_coords = oddr_to_cube(Math.floor((x - 2 * N) / 2), y - N);
            // oddr_to_cube(Math.floor(prx[x]/2),pry[y]);

            //If we're inside the hexagon
            if (Math.abs(these_cube_coords[0]) < 5 & Math.abs(these_cube_coords[1]) < 5 & Math.abs(these_cube_coords[2]) < 5) {
                //If the square is already locked in...
                if (locked_state[these_cube_coords[0] + N][these_cube_coords[1] + N] == 1) {
                    //And if the locked in state is correct
                    if (target[these_cube_coords[0] + N][these_cube_coords[1] + N] == 1) {
                        d.draw(x, y, null, null, "#090"); //Strong Green
                        if (these_cube_coords[0] == 0 & these_cube_coords[1] == 0) {
                            // + in centre (if no target there)
                            d.draw(x, y, '+', 'black', '#090');
                        }
                    } else {
                        //Otherwise
                        d.draw(x, y, null, null, "#900");//Red
                        if (these_cube_coords[0] == 0 & these_cube_coords[1] == 0) {
                            // + in centre (if no target there)
                            d.draw(x, y, '+', 'black', '#900');
                        }
                    }

                } else {
                    //If state is not already not locked in but is part of the target
                    if (target[these_cube_coords[0] + N][these_cube_coords[1] + N] == 1) {
                        d.draw(x, y, null, null, "#9e9"); //Light green

                        if (these_cube_coords[0] == 0 & these_cube_coords[1] == 0) {
                            // + in centre (if target there too)
                            d.draw(x, y, '+', 'black', "#9e9");
                        }

                        if (these_cube_coords[0] == board_orientation[0] &
                            these_cube_coords[1] == board_orientation[1] &
                            these_cube_coords[2] == board_orientation[2])//these_cube_coords[0]==these_cube_coords[2])
                        {
                            console.log(these_cube_coords, these_cube_coords[1], -N, these_cube_coords[1] == -N);
                            d.draw(x, y, '•', '#e55', '#9e9');
                        }

                    } else {
                        //Otherwise if unoccupied, non target and not locked in it is white
                        d.draw(x, y, null, null, "white");

                        if (these_cube_coords[0] == 0 & these_cube_coords[1] == 0) {
                            // + in centre (if no target there)
                            d.draw(x, y, '+', 'black', 'white');
                        }

                        if (these_cube_coords[0] == board_orientation[0] &
                            these_cube_coords[1] == board_orientation[1] &
                            these_cube_coords[2] == board_orientation[2])//these_cube_coords[0]==these_cube_coords[2])
                        {
                            console.log(these_cube_coords, these_cube_coords[1], -N, these_cube_coords[1] == -N);
                            d.draw(x, y, '•', '#e55', 'white');
                        }
                    }
                }



                // If the state is actively occupied
                if (state[these_cube_coords[0] + N][these_cube_coords[1] + N] == 1) {
                    //And part of the pattern
                    if (target[these_cube_coords[0] + N][these_cube_coords[1] + N] == 1) {

                        d.draw(x, y, ".", "black", "#595");//"#595"

                        if (these_cube_coords[0] == 0 & these_cube_coords[1] == 0) {
                            // + in centre (if target there too)
                            d.draw(x, y, '+', 'black', "#595");
                        }

                        if (these_cube_coords[0] == board_orientation[0] &
                            these_cube_coords[1] == board_orientation[1] &
                            these_cube_coords[2] == board_orientation[2])//these_cube_coords[0]==these_cube_coords[2])
                        {
                            console.log(these_cube_coords, these_cube_coords[1], -N, these_cube_coords[1] == -N);
                            d.draw(x, y, '•', '#e55', '#595');
                        }

                    } else {
                        d.draw(x, y, ".", "black", "#999");

                        if (these_cube_coords[0] == 0 & these_cube_coords[1] == 0) {
                            // + in centre (if target there too)
                            d.draw(x, y, '+', 'black', "#999");
                        }

                        if (these_cube_coords[0] == board_orientation[0] &
                            these_cube_coords[1] == board_orientation[1] &
                            these_cube_coords[2] == board_orientation[2])//these_cube_coords[0]==these_cube_coords[2])
                        {
                            console.log(these_cube_coords, these_cube_coords[1], -N, these_cube_coords[1] == -N);
                            d.draw(x, y, '•', '#e55', '#999');
                        }
                    }
                }
            }


        }
    }

    UpdateStringVis();
}

// function SpeculativeUpdate(speculative_state)
// {
//     // Loop over the raw xy locations
//     for (var y = 0; y < 9; y++)
//     {
//         for (var x = y%2; x < 18; x += 2)
//         {
//             //Convert to cube coordinate
//             var these_cube_coords = oddr_to_cube(Math.floor((x-2*N)/2),y-N);
//             // oddr_to_cube(Math.floor(prx[x]/2),pry[y]);

//             //If we're inside the hexagon
//             if(Math.abs(these_cube_coords[0])<5 & Math.abs(these_cube_coords[1])<5 & Math.abs(these_cube_coords[2])<5)
//             {

//                 //If state is part of the target
//                 if (target[these_cube_coords[0]+N][these_cube_coords[1]+N]==1)
//                 {
//                     d.draw(x, y, null, null, "#9e9"); //Light green
//                 } else {
//                     //Otherwise if unoccupied, non target and not locked in it is white
//                     d.draw(x, y, null, null, "white");
//                 }

//                 // If the state is actively occupied
//                 if (speculative_state[these_cube_coords[0]+N][these_cube_coords[1]+N]==1)
//                 {
//                     //And part of the pattern
//                     if (target[these_cube_coords[0]+N][these_cube_coords[1]+N]==1)
//                     {

//                         d.draw(x, y, "•", "white", "#595");//"#595"
//                     } else {
//                         d.draw(x, y, "•", "white",  "#999");
//                     }
//                 } 
//             }
//         }
//     }

//     console.log('speculative state drawn');
// }

function UpdateStringVis() {
    display_array = [`<span>`,];
    // console.log(action_keycodes.concat(cachable_keycodes));
    for (let i = 0; i < results.action_history.length; i++) {
        display_array.push(`<span style="color: ${player_colours[results.player_history[i] - 1]}">`)
        for (let j = 0; j < results.action_history[i].length; j++) {
            for (let k = 0; k < action_displays.length; k++) {
                if (results.action_history[i][j] == action_keycodes.concat(cachable_keycodes)[k]) {
                    console.log('test', i, k, action_displays[k]);
                    display_array.push(action_displays[k])
                }
            }
        }
        display_array.push(`</span>`)
    }
    display_array.push(`</span>`);
    console.log('updatestringvis triggered', display_array.join(''));
    $('#locked-in-actions').html(display_array.join(''));


    spec_display_array = [`<span style="color: ${player_colours[player - 1]}">`];
    // console.log(action_keycodes.concat(cachable_keycodes));
    for (let i = 0; i < spec_actions.length; i++) {
        for (let j = 0; j < action_displays.length; j++) {
            if (spec_actions[i] == action_keycodes.concat(cachable_keycodes)[j]) {
                console.log('test', i, j, action_displays[j]);
                spec_display_array.push(action_displays[j])
            }
        }
    }

    spec_display_array.push(`</span>`);
    console.log('spec display array', spec_display_array.join(''));
    $('#current-actions').html(spec_display_array.join(''));
}



function UpdateCacheVis(cache_n, player) {
    var tmp = [];

    if (!lib_public[cache_n]) {
        tmp.push(`<span style="color: ${player_colours[player - 1]}">`);
    }
    for (let i = 0; i < library[player - 1][cache_n].length; i++) {
        for (let j = 0; j < action_displays.length; j++) {
            if (library[player - 1][cache_n][i] == action_keycodes.concat(cachable_keycodes)[j]) {
                // console.log(i, j, action_displays[j]);
                tmp.push(action_displays[j])
            }
        }
    }

    if (!lib_public[cache_n]) {
        tmp.push(`</span>`);
    }
    console.log('update cache vis triggered', tmp.join(''));

    $("#cache" + String(cache_n)).html(tmp.join(''));
}

function cube_to_oddr(q, r) {
    var col = q + (r - (r & 1)) / 2;
    var row = r;
    return [col, row];
}

function oddr_to_cube(x, y) {
    var q = x - (y - (y & 1)) / 2;
    var r = y;
    var s = -q - r
    return [q, r, s];
}

function AddUnit(this_state) {
    //Clone the current state (for undo)
    old_state = _.cloneDeep(this_state);

    //Add unit in centre
    this_state[N][N] = 1;
}

function RemoveUnit(this_state) {
    //Clone the current state (for undo)
    old_state = _.cloneDeep(this_state);


    //Add unit in centre
    this_state[N][N] = 0;
}

function AddBar(this_state) {
    //Clone the current state (for undo)
    old_state = _.cloneDeep(this_state);

    //Add horizontal bar
    this_state[N][N - 1] = 1;
    this_state[N][N] = 1;
    this_state[N][N + 1] = 1;
}

function AddCorner(this_state) {
    //Clone the current state (for undo)
    old_state = _.cloneDeep(this_state);

    //Add horizontal bar
    this_state[N + 1][N] = 1;
    this_state[N][N] = 1;
    this_state[N - 1][N + 1] = 1;
}


function Shift(dir, this_state) {
    //Clone the current state
    old_state = _.cloneDeep(this_state);

    var sv = cdv[dir];//Shift vector

    //Loop over all the axial coordinates in the hexagon
    for (var q = -N; q <= N; q++) {
        for (var r = -N; r <= N; r++) {
            //Calculate s
            var s = -q - r

            //Calculate updated position
            var qprime = q + sv[0]
            var rprime = r + sv[1]
            var sprime = s + sv[2]
            // console.log('qrs', q,r,s, 'qrsprime', qprime, rprime,sprime);
            //If new position is still on the map
            if (Math.abs(qprime) < 5 & Math.abs(rprime) < 5 & Math.abs(sprime) < 5) {
                //Write into the new position
                this_state[qprime + N][rprime + N] = old_state[q + N][r + N];
            }

            //Zero the cells left behind
            if (sv[0] != 0 & q == (-N * sv[0])) {
                // console.log('q zeroing', q, r)
                this_state[q + N][r + N] = 0;
            }
            if (sv[1] != 0 & r == (-N * sv[1])) {
                // console.log('r zeroing', q, r)
                this_state[q + N][r + N] = 0;
            }
            if (sv[2] != 0 & s == (-N * sv[2])) {
                // console.log('s zeroing', q, r)
                this_state[q + N][r + N] = 0;
            }
        }
    }
}



function RotateClockwise(this_state) {
    old_bo = _.cloneDeep(board_orientation);
    board_orientation[0] = -old_bo[1];
    board_orientation[1] = -old_bo[2];
    board_orientation[2] = -old_bo[0];

    old_state = _.cloneDeep(this_state);
    for (var q = -N; q <= N; q++) {
        for (var r = -N; r <= N; r++) {
            s = -q - r
            //120 degree rotations
            // var q_rot= r;
            // var r_rot = s;
            // var s_rot = q;

            //60 degree rotations
            //xx, yy, zz = -yy, -zz, -xx
            var q_rot = -r;
            var r_rot = -s;
            var s_rot = -q;
            if (Math.abs(q) <= N & Math.abs(r) <= N & Math.abs(s) <= N) {
                //Stops things spinning out of the  hex and growing the array dims?
                this_state[q_rot + N][r_rot + N] = old_state[q + N][r + N];
            }

        }
    }
}

function Flip(axis = 1, this_state) {

    old_bo = _.cloneDeep(board_orientation);
    board_orientation[0] = old_bo[0];
    board_orientation[1] = old_bo[2];
    board_orientation[2] = old_bo[1];

    old_state = _.cloneDeep(this_state);
    for (var q = -N; q <= N; q++) {
        for (var r = -N; r <= N; r++) {
            s = -q - r;
            if (axis == 1) {
                q_flip = s;
                r_flip = r;
                s_flip = q;

            } else if (axis == 2) {
                q_flip = r;
                r_flip = q;
                s_flip = s;
            } else if (axis == 3) {
                q_flip = q;
                r_flip = s;
                s_flip = r;


            }

            if (Math.abs(q) <= N & Math.abs(r) <= N & Math.abs(s) <= N) {
                //Stops things spinning out of the  hex and growing the array dims?
                this_state[q_flip + N][r_flip + N] = old_state[q + N][r + N];
            }

        }
    }
}

function HexReflect(axis = 1, this_state) {
    old_state = _.cloneDeep(this_state);
    for (var q = -N; q <= N; q++) {
        for (var r = -N; r <= N; r++) {
            s = -q - r;
            if (axis == 1) {
                q_flip = s;
                r_flip = r;
                s_flip = q;

            } else if (axis == 2) {

                q_flip = r;
                r_flip = q;
                s_flip = s;

            } else if (axis == 3) {
                q_flip = q;
                r_flip = s;
                s_flip = r;

            }

            if (Math.abs(q) <= N & Math.abs(r) <= N & Math.abs(s) <= N) {
                //Stops things spinning out of the  hex and growing the array dims?
                this_state[q_flip + N][r_flip + N] = this_state[q_flip + N][r_flip + N] || old_state[q + N][r + N];
            }

        }
    }
}

function Check(this_state) {
    var match = true;
    for (var q = -N; q <= N; q++) {
        for (var r = -N; r <= N; r++) {
            var s = -q - r;

            if (Math.abs(q) <= N & Math.abs(r) <= N & Math.abs(s) <= N) {
                if (this_state[q + N][r + N] !== target[q + N][r + N]) {
                    match = false;
                }

                if (locked_state[q + N][r + N] == 1 & target[q + N][r + N] == 0) {
                    spillage++
                }
            }
        }
    }

    if (match) {
        locked_state = _.cloneDeep(this_state);
        Update();
        save_data();
        alert("Protein synthesised in " + actions.length + " actions");
    }
}

// function Lock(this_state, real) 
// {
//     old_state = _.cloneDeep(this_state);
//     var match = true;
//     spillage = 0;

//     for (var q=-N; q<=N; q++)
//     {
//         for (var r=-N; r<=N; r++)
//         {
//             var s = -q-r;
//             if (this_state[q+N][r+N]==1)
//             {
//                 if (real)
//                 {
//                     locked_state[q+N][r+N]=1;
//                 } else {
//                     target_lock_tmp[q+N][r+N]=1;
//                 }

//                 this_state[q+N][r+N]=0;
//             }

//             if (real & Math.abs(q)<=N & Math.abs(r)<=N & Math.abs(s)<=N)
//             {
//                 if (locked_state[q+N][r+N] !== target[q+N][r+N])    
//                 {
//                    //console.log('nonmatch', q,r);

//                     match = false; 
//                 }

//                 if (locked_state[q+N][r+N]==1 & target[q+N][r+N]==0)
//                 {
//                     spillage++
//                 }
//             }
//         }
//     }

//     // console.log("Match =", match);
//     // console.log("Locked state:");
//     // console.log(JSON.stringify(locked_state));
//     // console.log("Target:");
//     // console.log(JSON.stringify(target));


//     if (real)
//     {
//         inputLocked = true;
//         attemptCount++;

//         // const existingControls = document.getElementById("control-buttons");
//         // if (existingControls) existingControls.remove();

//         const btnContainer = document.createElement("div");
//         btnContainer.id = "control-buttons";
//         btnContainer.style.marginTop = "1em";
//         btnContainer.style.alignSelf = "center";
//         btnContainer.style.marginBottom = "1em";

//         const feedback = document.createElement("p");
//         feedback.textContent = match ? "Pattern complete" : "Pattern not complete -- try again.";
//         alert(match ? "Pattern complete" : "Pattern not complete -- try again!" + actions.length + " actions, with " + spillage + " mistakes!");
//         btnContainer.appendChild(feedback);

//         const tryAgainBtn = document.createElement("button");
//         tryAgainBtn.textContent = "Try Again";
//         tryAgainBtn.onclick = function () {
//             inputLocked = false;
//             //console.log('attempt count: ' + attemptCount);
//             // if(jsPsych.getCurrentTrial()==hexTutorial){
//             //     //timeline.splice(tutCounter+1, 0, hexTutorial);
//             //     //tutCounter = tutCounter-1;
//             //     R
//             // }
//             // else{
//             //     //timeline.splice(patternCounter+1, 0, hexTrial);
//             //     //patternCounter = patternCounter-1;
//             // }

//             //jsPsych.finishTrial();
//             ResetBoard();

//         };
//         btnContainer.appendChild(tryAgainBtn);

//         // if (match || attemptCount >= 3) {
//         //     const moveOnBtn = document.createElement("button");
//         //     moveOnBtn.textContent = "Move On";
//         //     moveOnBtn.style.marginLeft = "1em";
//         //     moveOnBtn.onclick = function () {
//         //         if (match) {
//         //             save_data(); 
//         //             attemptCount = 0;
//         //         }
//         //         jsPsych.finishTrial();
//         //     };
//         //     btnContainer.appendChild(moveOnBtn);
//         // }

//     document.body.prepend(btnContainer);
//     }
// }

function ResetBoard() {
    save_data(); //Save the attempt

    state = _.cloneDeep(empty_state);
    old_state = _.cloneDeep(empty_state);
    locked_state = _.cloneDeep(empty_state);
    prelocked_state = _.cloneDeep(empty_state);
    actions = [];
    spec_actions = [];
    spillage = 0;
    display_array = [];
    results = {
        target: [], performance: [], action_history: [],
        state_history: [], player_history: [],
        complete_action_history: [], complete_state_history: [],
        library_history: [], library_update_at: [0]
    };



    Update();
}

// function Lock(this_state, real)
// {
//     old_state = _.cloneDeep(this_state);
//     var match = true;
//     spillage = 0;

//     for (var q=-N; q<=N; q++)
//     {
//         for (var r=-N; r<=N; r++)
//         {
//             var s = -q-r;
//             if (this_state[q+N][r+N]==1)
//             {
//                 if (real)
//                 {
//                     locked_state[q+N][r+N]=1;
//                 } else {
//                     target_lock_tmp[q+N][r+N]=1;
//                 }

//                 this_state[q+N][r+N]=0;
//             }

//             if (real & Math.abs(q)<=N & Math.abs(r)<=N & Math.abs(s)<=N)
//             {
//                 if (locked_state[q+N][r+N]==0 & target[q+N][r+N]==1)
//                 {
//                    console.log('nonmatch', q,r);
//                     match = false; 
//                 }

//                 if (locked_state[q+N][r+N]==1 & target[q+N][r+N]==0)
//                 {
//                     spillage++
//                 }
//             }
//         }
//     }

//     if (real)
//     {
//         if (match)
//         {
//             save_data();
//         }
//     }

// }

function RandomPattern() {
    var target_pattern = _.cloneDeep(state);

    for (var x = 0; x < (2 * N + 1); x++) {
        for (var y = 0; y < (2 * N + 1); y++) {
            var these_cube_coords = oddr_to_cube(Math.floor((x - 2 * N) / 2), y - N);
            //oddr_to_cube(Math.floor(prx[x]/2),pry[y]);
            if (Math.abs(these_cube_coords[0]) <= N & Math.abs(these_cube_coords[1]) <= N & Math.abs(these_cube_coords[2]) <= N) {
                if (Math.random() > .5) {
                    target_pattern[x][y] = 1;
                }
            }
        }
    }
    return target_pattern;
}


function ChallengePattern(sequence) {
    var target_pattern = _.cloneDeep(state);

    var seq_arr = Array.from(sequence);
    for (var step = 0; step < sequence.length; step++) {
        var this_key = seq_arr.shift();
        var tmp = action_keys.indexOf(this_key);
        console.log('keycode', this_key, tmp, action_keycodes.concat(cachable_keycodes));

        Action(action_keycodes.concat(cachable_keycodes)[tmp], target_pattern, false);
    }

    for (var q = -N; q <= N; q++) {
        for (var r = -N; r <= N; r++) {
            if (target_lock_tmp[q + N][r + N] == 1) {
                target_pattern[q + N][r + N] = 1;
            }
        }
    }

    return target_pattern;
}


function ProceduralPattern(depth) {
    var target_pattern = _.cloneDeep(state);
    var generation_procedure = [];

    var filter = [];
    for (var i = 0; i < library[player - 1].length; i++) {
        filter[i] = library[player - 1][i].length > 1;
    }
    console.log('cache_exists', cachable_keycodes.filter((r, i) => filter[i]));

    for (var step = 0; step < depth; step++) {
        this_key = ROT.RNG.getItem(action_keycodes.concat(cachable_keycodes.filter((r, i) => filter[i])));//[cache_exists])
        //Remove the cachable keycode concatenation to use prior
        generation_procedure.push(this_key);
        Action(this_key, target_pattern, false);
    }

    console.log('generative procedure:',
        generation_procedure, action_keycodes.concat(cachable_keycodes));
    var tmp_key_array = [];
    for (var i = 0; i < generation_procedure.length; i++) {
        var tmp = action_keycodes.concat(cachable_keycodes).indexOf(generation_procedure[i]);
        tmp_key_array.push(action_keys[tmp]);
        // console.log(tmp_key_array, tmp);
    }
    pattern = tmp_key_array.join('');

    console.log('target_pattern', target_pattern);
    return target_pattern;
}



function UseCache(n, this_state, this_real, player) {
    //Read the cached pattern TODO KEEP THIS NOT IN THE STRING
    console.log('using cache', n, library[player - 1][n]);

    var cache_pattern = _.cloneDeep(library[player - 1][n]);//Array.from($('#cache' + n).text());
    //console.log('before expanding:', cache_pattern);

    //Get rid of any nesting before taking the action sequence
    console.log('cache pattern is', cache_pattern, cachable_keycodes, player, library[player - 1], n);

    var infinite_loop_safeguard = 0;
    var loopy = false;
    while (cache_pattern.includes(cachable_keycodes[0]) |
        cache_pattern.includes(cachable_keycodes[1]) |
        cache_pattern.includes(cachable_keycodes[2]) |
        cache_pattern.includes(cachable_keycodes[3])) {

        infinite_loop_safeguard++;
        if (infinite_loop_safeguard > 20) {
            console.log('infinite loop safeguard triggered, breaking out of loop', cache_pattern);
            loopy = true;
            break;
        }

        //Loop through the current pattern and replace any cache keycodes with the relevant cached pattern
        loop1:
        for (var i = 0; i < cache_pattern.length; i++) {
            //Check the current element to see if it is one of the cachable keycodes TODO REVISIT THIS LOGIC!
            for (var j = 0; j < cachable_keycodes.length; j++) {
                if (cache_pattern[i] == cachable_keycodes[j]) {
                    console.log(i, j, cache_pattern[i], cachable_keycodes[j], 'found cache keycode in pattern, replacing with', library[player - 1][j]);
                    //console.log('cache pattern before', cache_pattern);
                    cache_pattern.splice(i, 1);
                    //console.log('cache pattern during', cache_pattern);
                    insertArrayAt(cache_pattern, i, library[player - 1][j]);
                    console.log('cache pattern after', cache_pattern);
                    break loop1;
                };
            }
        }
    }
    if (!loopy) {
        //We've fully expanded the cache pattern, now we can feed it into the Action function step by step

        //How long is the fully expanded string?
        //var cpl = cache_pattern.length;

        while (cache_pattern.length > 0) {
            cur_keycode = cache_pattern.shift();
            Action(cur_keycode, this_state = this_state, real = this_real, midcache = true);
        }
        // if (this_real)
        // {
        //     Update();
        //     spec_actions.push(cachable_keycodes[n]);
        //     UpdateStringVis();
        // }
    } else {
        alert('Sorry using this cache would create an infinite loop. Try saving a different pattern', actions, spec_actions, library);
        library[player - 1][n] = [];
        UpdateCacheVis(n, player);
        Clear();

    }


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
    //                 //$('#current-actions').text(actions.join(''));//

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

function save_data() {
    var now = new Date();

    results.performance = { steps: actions.length, errors: spillage, cache: cache };
    results_str = JSON.stringify(results);

    jQuery.ajax({
        url: './static/php/save_data.php',
        type: 'POST',
        data: { results: results_str },
        success: function (data) {
            console.log('Sent data to database');

        },
        error: function (xhr, status, error) {
            //Just print out what comes back if it doesn't work
            console.log(xhr, status, error);
        }
    })
}

function Instructions1() {
    $('#instructions').show();
    $('#insbtn1').hide();
}

function Instructions2() {
    $('#instructions').hide();
    $('#insbtn1').show();
}

function Hint1() {
    $('#hint1').show();
    $('#hintbtn1').hide();
}

function Hint2() {
    $('#hint2').show();
    $('#hintbtn2').hide();
}
