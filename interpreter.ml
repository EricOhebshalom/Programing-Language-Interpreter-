type stackValue = Int of int | Bool of bool | Error of string | String of string | Name of string | Unit | Closure of ((string, stackValue) Hashtbl.t)*(string list)*(string)*bool
and command = ADD | SUB | MUL | DIV | REM | NEG | SWAP | TOSTRING | PUSH of stackValue | POP | PRINTLN | QUIT | CAT | AND | OR | NOT | EQUAL | LESSTHAN |GREATERTHAN | BIND | IF | LET | END | FUN of string*string | FUNEND | CALL | RETURN | INOUTFUN of string*string | COMMENT | JUMPTO | JUMPLOC of string | JUMPIF
let forAutolab = false 
let interpreter ((input : string), (output : string)) : unit =
  
  let ic = open_in input in
  let oc = open_out output in 
  let rec readInput acc =
      try 
          let l = String.trim(input_line ic) in readInput (l::acc)
      with
      | End_of_file -> List.rev acc in
  let strList = readInput [] in 
   
  let isInt (s : string) : bool =
      try
      ignore (int_of_string s);
      true
      with
      | Failure _ -> false
   in

   let isString (s : string) : bool =
      let len = String.length s in
      len >= 2 && String.get s 0 = '"' && String.get s (len - 1) = '"'
   in

   let isLetter c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || (c = '_') in
   let isDigit c = '0' <= c && c <= '9' in
   let isValid c = (isLetter c) || (isDigit c) in

   let rec restCheck s int = 
      if int >= String.length s then true 
      else ((isValid s.[int]) && (restCheck s (int + 1))) in

   let isName (s : string) : bool = 
      ((isLetter s.[0]) && (restCheck s 1))
   in
   
   let isPush (s : string) : bool =
      String.length s >= 5 && String.sub s 0 5 = "push "
   in

   let pushLogic s = 
      match s with
      |":true:"-> PUSH (Bool true)
      |":false:"-> PUSH (Bool false)
      |":error:"-> PUSH (Error "error was pushed onto stack")
      |":unit:"-> PUSH Unit
      |_ -> 
         match (isInt s) with 
         |true -> PUSH(Int (int_of_string s))
         |false -> 
            match (isString s) with
            |true -> PUSH(String (String.sub s 1 ((String.length s) - 2)))
            |false ->
               match (isName s) with
               |true -> PUSH(Name s)
               |false -> PUSH (Error "Value pushed that's not able to be pushed")
   in

   let isFun (s : string) : bool =
      String.length s >= 4 && String.sub s 0 4 = "fun "
   in

   let funLogic s = 
      let parts = String.split_on_char ' ' s in
      match parts with
      |[name1; name2]->(match((isName name1 && isName name2) && (name1 != name2))with 
         |true -> FUN(name1, name2)
         |false -> PUSH (Error "One of the arguments are not a name or both the arguments are the same")) 
      |_ -> PUSH (Error "2 arguments are needed")
   in

   let isInOutFun (s : string) : bool =
      String.length s >= 9 && String.sub s 0 9 = "inOutFun "
   in
   

   let inOutFunLogic s = 
      let parts = String.split_on_char ' ' s in
      match parts with
      |[name1; name2]->(match((isName name1 && isName name2) && (name1 != name2))with 
         |true -> INOUTFUN(name1, name2)
         |false -> PUSH (Error "One of the arguments are not a name or both the arguments are the same")) 
         |_ -> PUSH (Error "2 arguments are needed")
   in

   let isComment (s : string) : bool =
      String.length s >= 2 && String.sub s 0 2 = "//"
   in

   let isJumpLoc (s : string) : bool =
      String.length s >= 2 && String.sub s 0 2 = "##"
   in

  let stringToCommand s =
   match s with
   |"add" -> ADD
   |"sub" -> SUB
   |"mul" -> MUL
   |"div" -> DIV
   |"rem" -> REM
   |"neg" -> NEG
   |"swap" -> SWAP
   |"toString" -> TOSTRING
   |"pop" -> POP
   |"println" -> PRINTLN
   |"quit" -> QUIT
   |"cat" -> CAT
   |"and" -> AND
   |"or" -> OR
   |"not" -> NOT
   |"equal" -> EQUAL
   |"bind" -> BIND
   |"lessThan" -> LESSTHAN
   |"greaterThan" -> GREATERTHAN
   |"if" -> IF
   |"let" -> LET
   |"end" -> END
   |"funEnd" -> FUNEND
   |"call" -> CALL
   |"return" -> RETURN
   |"jumpTo" -> JUMPTO
   |"jumpIf" -> JUMPIF
   |"" -> COMMENT
   |_ ->
      match (isJumpLoc s) with
      |true -> JUMPLOC (String.sub s 2 (String.length s - 2))
      |false ->
         match (isComment s) with
         |true -> COMMENT
         |false ->
            match (isPush s) with 
            |true -> pushLogic (String.sub s 5 (String.length s - 5))
            |false -> 
               match (isFun s) with
               |true ->funLogic (String.sub s 4 (String.length s - 4))
               |false ->
                  match (isInOutFun s) with
                  |true -> inOutFunLogic (String.sub s 9 (String.length s - 9))
                  |false ->(PUSH (Error "Input is not registered"))   
  in

  let stackList = ref [] in stackList := ref[] :: !stackList;

  let scope = ref[] in scope := Hashtbl.create 10 :: !scope;

  let rec realHelp n cur =
   (match cur with
   |[] -> (Error "scope is empty")
   |hd::tl ->(if Hashtbl.mem hd n then (Hashtbl.find hd n) else realHelp n tl))
  in

  let real v =
   match v with
   | Name n ->(realHelp n !scope)
   | _ -> v
   in

   let rec copy lst =
      match lst with
      | [] -> []
      | hd :: tl -> hd :: copy tl
   in

   let rec getEnvironmentHelp instructions returnMe = 
      match instructions with
      |[] -> returnMe
      |hd::tl ->(

         if(isPush hd) then(Hashtbl.replace returnMe (String.sub hd 5 (String.length hd - 5)) (real (Name (String.sub hd 5 (String.length hd - 5)))));
         if(isFun hd) then(Hashtbl.replace returnMe (String.sub hd 4 (String.length hd - 4)) (real (Name (String.sub hd 4 (String.length hd - 4)))));
         if(isInOutFun hd) then(Hashtbl.replace returnMe (String.sub hd 9 (String.length hd - 9)) (real (Name (String.sub hd 9 (String.length hd - 9)))));

         getEnvironmentHelp tl returnMe)
   in

   let getEnvironment instructions = (
      let returnMe = Hashtbl.create 10 in (getEnvironmentHelp instructions returnMe)) 
   in

   let rec untilFunEnd lst count = 
      match lst with
      | [] -> []
      | "funEnd" :: xs when count = 0 -> []
      | "funEnd" :: xs -> "funEnd" :: untilFunEnd xs (count - 1)
      | x :: xs -> 
         if(isFun x || isInOutFun x) then(x :: untilFunEnd xs (count + 1)) 
         else x :: untilFunEnd xs count
   in

   let rec skip lst count = 
      match lst with
      | [] -> ()
      | "funEnd" :: tl ->(
          if count = 0 then
            loopStrings tl
          else
            skip tl (count - 1))
      | hd :: tl ->
          match (isFun hd || isInOutFun hd) with
          | true -> skip tl (count + 1)
          | false -> skip tl count
       
   and jumpFun lst (word : string) = 
   let concat = "##"^word in
   match lst with
   | [] -> (Printf.fprintf oc ":error: Jump called with invalid jump location")
   | hd :: tl -> 
      if hd = concat then loopStrings tl 
      else jumpFun tl word
   
   and loopStrings lst =
   match !stackList with
   |[] -> ()
   |stack :: stackTail ->
   match lst with
   | [] -> ()
   | hd :: tl -> 
      let com = stringToCommand hd in
      (match com with
   
      |JUMPLOC l ->()
      |COMMENT ->()
      |INOUTFUN (name1, name2) -> (
         match !scope with
         |[]->()
         |cur::rest ->(
            let instructions = untilFunEnd tl 0 in
            let environment = getEnvironment instructions in
            Hashtbl.replace cur name1 (Closure (environment, instructions, name2, true));
            stack := Unit :: !stack))
      |FUN (name1, name2)-> (
         match !scope with
         |[]->()
         |cur::rest ->(
            let instructions = untilFunEnd tl 0 in
            let environment = getEnvironment instructions in
            Hashtbl.replace cur name1 (Closure (environment, instructions, name2, false));
            stack := Unit :: !stack))
      |FUNEND -> (stack := (Error "FunEnd called before function") :: !stack)
      |CALL -> (
         match !stack with
         |arg :: funName :: rest -> (
            match arg with
            |Name _|Int _|String _|Bool _|Closure _| Unit ->(
               match (real funName) with
               |Closure(environment, instructions, argument, isInOut)->(
                  stack := rest;
                  stackList := ref (copy rest) :: !stackList;
                  Hashtbl.replace environment argument (real arg);
                  
                  (*
                  (match funName with
                  |Name funnerName ->(
                     let cl = Closure((getEnvironment instructions), instructions, argument, isInOut) in
                     Hashtbl.replace environment funnerName (cl);)
                  |_ ->());*)
                  
                  let oldScope = !scope in
                  scope := [environment];

                  loopStrings instructions;
                  
                  scope := oldScope;
                  
                  let realArg = Hashtbl.find environment argument in
                
                  match !stackList with
                  |[] -> ()
                  |hd::tl -> stackList := tl;
                  
                  match isInOut with
                  |false -> ()
                  |true -> (
                     match arg with
                     |Name n ->(
                        match !scope with
                        |[] -> ()
                        |top :: rest ->(Hashtbl.replace top n realArg))
                     |_ ->()
                  ))

               |_ -> stack := (Error "Variable that's not a closure called") :: !stack)
            |_ -> stack := (Error "argument with invalid type") :: !stack)
         |_ -> stack := (Error "Function call but stack is too small") :: !stack)
      |RETURN ->(
         match !stackList with
         |[]->()
         |hd::[] ->()
         |hd::tl -> (
            stackList := tl;
            match !hd with
            |[] -> ()
            |top :: rest ->(
               match tl with
               |[] -> ()
               |head :: rest ->head := (real top) :: !head)))
      
      |LET ->(
         scope := Hashtbl.create 10 :: !scope;
         stackList := ref(copy !stack) :: !stackList)
      |END->(
         match !scope with
         |[]->()
         |hd::[] ->()
         |hd::tl -> scope := tl;    
         match !stackList with
         |[]->()
         |hd::[] ->()
         |hd::tl -> (
            stackList := tl;
            match !hd with
            |[] -> ()
            |top:: rest ->(
               match tl with
               |[] -> ()
               |head :: rest ->head := top :: !head)))
      |BIND -> (
         match !scope with
         |[]->()
         |cur::tl ->(
         match !stack with
         |a :: Name b :: c -> (
            match a with
            |Int _|String _|Bool _|Unit |Closure _ ->(stack := Unit :: c; Hashtbl.replace cur b a)
            |Name n ->(
               let isThere = real a in 
               match isThere with
               |Error _ -> (stack := (Error "variable points to an error") :: !stack)
               |_ ->(stack := Unit :: c; Hashtbl.replace cur b isThere)
               )
            |_ -> stack := (Error "Invalid variable type") :: !stack)
         |_ -> stack := (Error "not enough stackValues or neither value is a name") :: !stack))
      |IF ->(
         match !stack with
         |a::b::c::rest -> (
            match real c with
            |Bool true ->(stack := a::rest)
            |Bool false ->(stack := b::rest)
            |_ -> stack := (Error "not a bool") :: !stack)
         |_ -> stack := (Error "not enough values on stack when if called") :: !stack)
      |JUMPIF ->(
         match !stack with
         |a::b::c::rest ->(match real a, real b, real c with | String x, String y, Bool z ->(match z with |true ->(jumpFun strList x) |false ->(jumpFun strList y)) |_ -> stack := (Error "Wrong types on stack when jumpIf called") :: !stack)
         |_ -> stack := (Error "not enough values on stack when jumpIf called") :: !stack)
      |JUMPTO ->(
         match !stack with 
         |a::b ->(match real a with | String x-> (stack := b; jumpFun strList x) |_-> stack := (Error "Wrong type on stack when jumpTo called") :: !stack)
         |_-> stack := (Error "not enough values on stack when jumpTo called") :: !stack)
      |LESSTHAN ->(
         match !stack with 
         |a::b::c ->(match real a, real b with | Int x, Int y -> stack := Bool (y < x) :: c |_-> stack := (Error "Wrong type on stack when LessThan called") :: !stack)
         |_-> stack := (Error "not enough values on stack when LessThan called") :: !stack)
      |GREATERTHAN ->(
         match !stack with 
         |a::b::c ->(match real a, real b with | Int x, Int y -> stack := Bool (y > x) :: c |_-> stack := (Error "Wrong type on stack when greaterThan called") :: !stack)
         |_-> stack := (Error "not enough values on stack when greaterThan called") :: !stack)
      |EQUAL ->(
         match !stack with 
         |a::b::c ->(match real a, real b with | Int x, Int y -> stack := Bool (y = x) :: c |_-> stack := (Error "Wrong type on stack when equal called") :: !stack)
         |_-> stack := (Error "not enough values on stack when equal called") :: !stack)
      |NOT ->(
         match !stack with 
         |a::c ->(match real a with | Bool x -> stack := Bool (not x) :: c |_-> stack := (Error "Wrong type on stack when not called") :: !stack)
         |_-> stack := (Error "not enough values on stack when not called") :: !stack)
      |OR ->(
         match !stack with 
         |a::b::c ->(match real a, real b with | Bool x, Bool y -> stack := Bool (y || x) :: c |_-> stack := (Error "Wrong type on stack when or called") :: !stack)
         |_-> stack := (Error "not enough values on stack when or called") :: !stack)
      |AND ->(
         match !stack with 
         |a::b::c ->(match real a, real b with | Bool x, Bool y -> stack := Bool (y && x) :: c |_-> stack := (Error "Wrong type on stack when and called") :: !stack)
         |_-> stack := (Error "not enough values on stack when and called") :: !stack)
      |CAT ->(
         match !stack with 
         |a::b::c ->(match real a, real b with | String x, String y -> stack := String (y ^ x) :: c |_-> stack := (Error "Wrong type on stack when cat called") :: !stack)
         |_-> stack := (Error "not enough values on stack when cat called") :: !stack)
      |ADD ->(
         match !stack with 
         |a::b::c ->(match real a, real b with | Int x, Int y -> stack := Int (y + x) :: c |_-> stack := (Error "Wrong type on stack when add called") :: !stack)
         |_-> stack := (Error "not enough values on stack when add called") :: !stack)
      |SUB ->(
         match !stack with 
         |a::b::c ->(match real a, real b with | Int x, Int y -> stack := Int (y - x) :: c |_-> stack := (Error "Wrong type on stack when sub called") :: !stack)
         |_-> stack := (Error "not enough values on stack when sub called") :: !stack)
      |MUL ->(
         match !stack with 
         |a::b::c ->(match real a, real b with | Int x, Int y -> stack := Int (y * x) :: c |_-> stack := (Error "Wrong type on stack when mul called") :: !stack)
         |_-> stack := (Error "not enough values on stack when mul called") :: !stack)
      |DIV ->(
         match !stack with 
         |a::b::c ->(match real a, real b with | Int 0, _ -> stack := (Error "Divide by 0 when div called") :: !stack | Int x, Int y -> stack := Int (y / x) :: c |_-> stack := (Error "Wrong type on stack when div called") :: !stack)
         |_-> stack := (Error "not enough values on stack when div called") :: !stack)
      |REM ->(
         match !stack with 
         |a::b::c ->(match real a, real b with | Int 0, _ -> stack := (Error "Divide by 0 when rem called") :: !stack | Int x, Int y -> stack := Int (y mod x) :: c |_-> stack := (Error "Wrong type on stack when rem called") :: !stack)
         |_-> stack := (Error "not enough values on stack when rem called") :: !stack)
      |NEG -> (
         match !stack with 
         |a::b ->(match real a with | Int x-> stack := Int (x * -1) :: b |_-> stack := (Error "Wrong type on stack when neg called") :: !stack)
         |_-> stack := (Error "not enough values on stack when neg called") :: !stack)
      |SWAP -> (
         match !stack with 
         |a::b::c -> stack := b::a::c
         |_ -> stack := (Error "not enough values on stack when swap called") :: !stack)
      |POP ->(match !stack with |[] -> stack := (Error "not enough values on stack when swap called") :: !stack |hd :: tl -> stack := tl)
      |PUSH stackValue -> stack := stackValue :: !stack 
      |TOSTRING -> (
         match !stack with
         |[] -> stack := (Error "not enough values on stack when toString called") :: !stack
         |hd :: tl -> 
            stack := tl;
            match hd with
            |String s -> (stack := String s::!stack)
            |Int i ->(stack := String (string_of_int i) :: !stack)
            |Error e ->(
               match forAutolab with
               |false -> stack := String (":error: "^e) :: !stack
               |true -> stack := String (":error:") :: !stack)
            |Unit ->(stack := String ":unit:" :: !stack)
            |Name n ->(stack := String n :: !stack)
            |Bool b ->(stack := String (":"^(string_of_bool b)^":") :: !stack)
            |Closure _ ->(stack := String ":fun:" :: !stack))
      |PRINTLN ->(
         match !stack with 
         |[] -> stack := (Error "not enough values on stack when printLN called") :: !stack
         |hd :: tl ->
            match real hd with
            |String str -> (Printf.fprintf oc "%s\n" str; flush oc; stack := tl)

            |_ -> stack := (Error "not enough values on stack") :: !stack) 
      |QUIT -> ());
      match com with
      |QUIT | JUMPTO | JUMPIF -> ()
      |FUN _ |INOUTFUN _-> (skip tl (0))
      |RETURN ->(stackList := ref [] :: !stackList)
      |_ -> loopStrings tl
   in 
   loopStrings strList;
   close_out oc
   ;;

  (* ***** Code From Here, Replace () above and write your code ***** *)
(* Do not include this in your submission but use this line to test your code *)

match forAutolab with
|false -> interpreter ("input.txt", "output.txt")
|true ->()