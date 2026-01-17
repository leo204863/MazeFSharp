(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Config.fs: static configuration
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Config

open Prelude

let filled_pixel_char = '*' //'\u2588'    // ascii '\219' for character '█'
let wall_pixel_char = '|' //'\u2588'    // ascii '\219' for character '█'
let empty_pixel_char = ' '

let default_flip_queue = 2  // double buffering
let default_fps_cap = 30

let log_pipe_name = "LogPipe"
let log_pipe_translate_eol = '\255'

let game_console_title = "Game Window"
let log_console_title = "Log Window"


let log_msg_color = Color.Gray
let log_warn_color = Color.Yellow
let log_error_color = Color.Red
let log_debug_color = Color.Cyan


//List with all the sounds
type SoundpathName = SPIntro|SPMenu|SPVictory|SPArcade|SPLose|SPBlind|SPTimed
let soundpaths: (SoundpathName * string)list = [(SPIntro,"..\..\Game_sounds\intro.wav");
                                                (SPMenu, "..\..\Game_sounds\misc_menu.wav");
                                                (SPVictory,"..\..\Game_sounds\Victory.wav");
                                                (SPArcade,"..\..\Game_sounds\ingame.wav");
                                                (SPLose,"..\..\Game_sounds\game_over.wav");
                                                (SPBlind,"..\..\Game_sounds\cieca.wav");
                                                (SPTimed,"..\..\Game_sounds\hello.wav")]

let startingScreenLogo = "



                                                            ████  ████████████████████████
                                                            ██                          ██
                                                            ██  ██████████  ██████████  ██
                                                            ██  ██  ██  ██  ██      ██  ██
                                                            ██  ██  ██  ██  ██████████  ██
                                                            ██  ██  ██  ██  ██      ██  ██
                                                            ██  ██  ██  ██  ██      ██  ██
                                                            ██                          ██
                                                            ██  ██████████  ██████████  ██
                                                            ██          ██  ██          ██
                                                            ██  ██████████  ██████████  ██
                                                            ██  ██          ██          ██
                                                            ██  ██████████  ██████████  ██
                                                            ██                          ██
                                                            ██████████████████  ██████████



                                                                Press any key to start...
            "

let mutable menuScreen = "
    \219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219
    \219\219    | `-.  | `.  -_-_ _-_  _-  _- -_ -  .'|   |.'|     \219\219
    \219\219._  |    |`!  |`.  -_ -__ -_ _- _-_-  .'  |.;'   |   _.\219\219
    \219\219| `-!._  |  `;!  ;. _______________ ,'| .-' |   _!.i'  \219\219
    \219\219|     |`-!._ | `.| [@]           [@]|.''|  _!.;'   |   \219\219
    \219\219'..__ |    |`';.| i|_|  -------  |_|'| _!-|   |   _|..-\219\219
    \219\219    |``--..|_ | `;!|i| |       | |i|.'j   |_..!-'|     \219\219
    \219\219    |    |   |`-,!_|_|  -------  |_||.!-;'  |    |     \219\219
    \219\219____|____!.,.!,.!,!|i|           |i|,!,.!.,.!..__|_____\219\219
    \219\219|     |    |  |  | |_|  -------  |_|| |   |   |    |   \219\219
    \219\219|     |    |..!-;'i|i| |       | |i| |`-..|   |    |   \219\219
    \219\219|    _!.-j'  | _!,'|_|  -------  |_||!._|  `i-!.._ |   \219\219
    \219\219!.-'|    | _.'|  !;|i|           |i|`.| `-._|    |``-..\219\219
    \219\219    |  _.''|  !-| !|_|  -------  |_|.|`-. | ``._ |     \219\219
    \219\219    |.|    |.|  !| |i| |       | |i||`. |`!   | `'.    \219\219
    \219\219_.-'  |  .'  |.' |/|_|  -------  |_|! |`!  `,.|    |-._\219\219
    \219\219|     !.'|  .'| .'|[@]___________[@] \|  `. | `._  |   \219\219
    \219\219|   .'   |.|  |/| /                 \|`.  |`!    |.|   \219\219
    \219\219|_.'|   .' | .' |/                   \  \ |  `.  | `._-\219\219
    \219\219'   | .'   |/|  /                     \ |`!   |`.|    `\219\219
    \219\219    !'|   .' | /                       \|  `  |  `.    \219\219
    \219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219
"

let mutable instructionMenu = "
                        \219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219
                        \219\219                                                       \219\219
                        \219\219                      Menu' Tasti                      \219\219
                        \219\219                                                       \219\219
                        \219\219              -------                                  \219\219
                        \219\219             |   W   |        Tasti per                \219\219
                        \219\219             | A S D |        i movimenti              \219\219
                        \219\219              -------                                  \219\219
                        \219\219              -------                                  \219\219
                        \219\219             | Enter |        Invio                    \219\219
                        \219\219              -------                                  \219\219
                        \219\219              -------                                  \219\219
                        \219\219             |   Q   |        Esci                     \219\219
                        \219\219              -------                                  \219\219
                        \219\219              -------                                  \219\219
                        \219\219             |   E   |        Vis. soluzione           \219\219
                        \219\219              -------                                  \219\219
                        \219\219                                                       \219\219
                        \219\219                                                       \219\219
                        \219\219               Premi un tasto per uscire...            \219\219
                        \219\219                                                       \219\219
                        \219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219\219
                        "

let mutable lose = "         
                            ............                   ...               ...                ...     .................                          
                       ....              ....         ....     ....          ........      .........   .....                                       
                      .....             .....         ....     ....          .........    ..........   .....                                       
                      .'''.              ....     ........     ........     ..'''...............'''.   .'''..............                          
                      .;;;.                     .';,,.             .;,;'..  .,;;,.   .',;,.    ';;;.   .;;;;,,;;;;;;;;,;;.                         
                      ';;;'                     .,;;;'            .';;:,..  .;;;,.   .,;;,.    ,:;;.   .;;;;,,,,,,,,,,,,,.                         
                      ;oll;        .;ccccccc:.  .:ollcccc::cccc::ccclll:'.  .lllc.   .,;;,.   .:lll,   ,lll:.                                      
                      ;ooo:        .cdooooool.  .coooooooooooooooooooooc,.  'lool.   .,;;,.   .:ooo;   ;ooo:.                                      
                     .cxxxc.        .'''cxxxd'  .oxxxc'''''.''''.',lxxxl;.  ,dxxd'    ....    .lxxx:   :xxxc.                                      
                     .lOkOl.            ;kkOx,  .dkkk:             cOkko;.  ;xOkd'            .oOkOc  .ckkOl.                                      
                      ,lclc:::::::::::::d000k,  'x000c             l000x:.  ;O00k,            .xK0Kl  .l00Kkc::::::::;;::.                         
                          ;KMMMMMWMMMMMWWWWWK;  'OWWWo             oWWWOl'  :NWWK;            .OWWWd  .dWWMWWMMMMMMMMWMMMd.                        
                          ,kKKKKKKKKKKKKKKKKO,  .xKKKc             lKKKd:.  ;OKKO,            .dKKKl   lKKKKKKKKKKKKKKKKKl                         
                           ..................    ....               ....     ....              ....     .................                          
                                                                                                                                                   
                                                                                                                 
                            ............          ...               ...       ................     .................                               
                                                 ....               ....     ....                  ....                                            
                      .....              ....    .....             ......   .....                  ....              .....                         
                      .'.'.             .....    .'...             ......   ......                .....              .....                         
                      .,,,.             .,,,,.   ',,,.             .,,,'.   .,,,,,''''''''''',.   .,,,,.             ',,,.                         
                      .;;;'             .,;;,.  .';;;.             .;;;'.   .,;;;;,;;;;;;;;;;;'.  .,;;,.             ';;;.                         
                      ,ccc,             .ccc:.  .;ccc'             ,ccc;'.  .cccc'.............   .cccc;,,,,,,,,,,,,,.....                         
                      ;ooo:             'oool.  .cooo,             ,oooc'.  'oool.                'looooooooooooooodc.                             
                      :ddd:.            'dddo'  .,;;;,,,,'    .',,,;;;;'.   ,dddo.                ,odddc;;;;;;;;;;;;;,,''.                         
                     .lkkkl.            ;xkkx,       ;kkOd'   'xkkx;        ;kkkx'                ,xkOx,            .oOkkc.                        
                     .:dddc'............:dddo'       ,dddo,...;dddo'        ;kOOk:.............   ;kOOx,            .dOOOc                         
                       ...:OKKKKKKKKKKKKo....         ...'dKKKd....         :XNNNXKKKKKKKKKKKKx.  :XNNK;            .kNNNo                         
                          ;KWMMMMMMMMWWWd                .kWWWx.            cNWWWWWWMMMMMMMMWWO'  cNWWX;            .OWWWo                         
                          .,;;;;;;;;;;;;.                 ';;;.             .;;;;;;;;;;;;;;;;;'   .;;;,.             ';;;.             
                          
                                                           
                                                           Premi un tasto per continuare
                                                                                                                                                  "




let mutable victory = "              
                                                         +h////////////////////////////////h+                      
                                                          h/............................../h                       
                                              `+ooo++o+   .moooooo+..:oooooooooo:..+oooooom.   +o++ooo+`           
                                              /s......os   m..............................m   so......s/           
                                              /s./y+y:./so+m..//////..............//////..m+os/.:y+y/.s/           
                                              /s.oo `os/-..m..::::::......//......::::::..m..-/so` oo.s/           
                                              /s.oo    -+oom............./hh/.............moo+-    oo.s/           
                                              /s.oo        m............/h``h/............m        oo.s/           
                                              /s.oo        m......:ooooos`  `sooooo:..`...m        oo.s/           
                                              :h.-ss:`     m......-os-          -so-......m     `:ss-.h:           
                                               /y+-./ss:`  m........-os.      .so-........m  `:ss/.-+y/            
                                                 -os+-./so:m-.........h-      -h.........-m:os/.-+so-              
                                                    -os+-./ho........-m .+oo+. m-........sh/.-+so-                 
                                                       -os+-h/.......+ds+-..-+sd+......./h-+so-                    
                                                          -oymo........................+myo-                       
                                                             -oy/.......oooooo......./yo-                          
                                                                /so:..............:os/                             
                                                                  `/oso/-....-/oso/`                               
                      +y:     /y/      .+sys:      .ss.     oy.     ss....----ss       /y/       -yo.   /yyyyyyyyy/   .syy:    ?+:                                      
                      ohs.   -sh+     .sho/yh/     -hh.    .hh-     oy.../ssssdo       +ho       /hy.      /shs/      -yhhh    3s/             
                      .+hy/-/yh+.    .sho   hh:    -hh.    .hh-     ss........ss       +hs       /hy.      .sh+.      -yhshy   1s/                                       
                        :yhyhs-      oho    :hh-   -hh.    .hh-     d+........+d       +hs  /ds  /hy.      .oh+       .yh/ohs  sh/                                        
                         .shs.      .yh/    .yh/   -hh.    .yh-     d+........+d       oho  +ds  /dy.      .oh+       .yh:-sho yh/                                         
                         .oh+        :hh    oho.   -hh.    .hh-     ss........ss       ohs  +ds  /hy.      .oh+       .yh:--yh+yh/                                          
                          oh+         /hh:-ohs.    -hh-    -hh-     oy.../ssssdo       +hs  +dy  +hs`      -sh+.      .yh:..-yhhh/                                              
                          oh+          /hhyhs.     .ohyssssyhs.     d+........+d        -yhshdhshh/     /shhysso:?/   .yh-```:hhh/                                                                      
                                                                 oyyooooooooooooyyo                             
                                                            osoooooooooooo+oooooooooooso                                    
                                                           /y:------------------------:y/                                   
                                                          :d/::::::::::::::::::::::::::/d:                                  
                                                          +o++++++++++++++++++++++++++++o+                                         
 
                                                      premi un tasto qualsiasi per continuare                                               
                                                   
"
