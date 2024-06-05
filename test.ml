#use "DriverBDM.ml";;

let ctest = "b+5*c;";;
let output = postfix ctest;;
let s = new eval_stack_bdm;;
s#assign ("b","2");;
s#assign ("c","4");;

compileCLine ctest s;;
s#peek;;

let k = new eval_stack_bdm;;

let lineone = "b=2;";;
compileCLine lineone k;;
let linetwo = "c=4;";;
compileCLine linetwo k;;
let linethree = "b+5*c;";;
compileCLine linethree k;;
k#peek;;




let t = new eval_stack_bdm;;

let lineone = "a=2;";;
let linetwo = "b=3;";;
let linethree = "c=2";;
let linefour = "y=b+b*b-4*a*c;";;

compileCLine lineone t;;
compileCLine linetwo t;;
compileCLine linethree t;;
compileCLine linefour t;;
t#lookup "y";;




let w = new eval_stack_bdm;;

let lineone = "a=2;";;
let linetwo = "b=3;";;
let linethree = "c=1";;
let linefour = "y=b+b*b-4*a*c;";;

compileCLine lineone w;;
compileCLine linetwo w;;
compileCLine linethree w;;
compileCLine linefour w;;
w#lookup "y";;



let v = new eval_stack_bdm;;
let lo = "y= -3";;
compileCLine lo v;;
v#lookup "y";;




let a = new eval_stack_bdm;;
let b = "x=3";;
let c = "y=2";;
let d = "z=1";;
let e = "q=x*y+z-y*z+x";;
compileCLine b a;;
compileCLine c a;;
compileCLine d a;;
compileCLine e a;;
a#lookup "q";;



let ta = new eval_stack_bdm;;
let tb = "x=3+4";;
let tc = "x=1";;
compileCLine tb ta;;
compileCLine tc ta;;
ta#lookup "x";;


#use "testfile.c";;

let i = new eval_stack_bdm;;
processFile "testfile.c" i;;
i#lookup "x";;



s#peek = Some("22");;
k#peek = Some("22");;
t#lookup "y" = Some("-4");;
w#lookup "y" = Some("4");;
v#lookup "y" = Some("-3");;
a#lookup "q" = Some("8");;
ta#lookup "x" = Some("1");;
i#lookup "x" = Some("1");;



