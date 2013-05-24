
let pretty_print = ref false
let parse_only = ref false
let benchmark = ref false
let interactive = ref false (* ajout *)

let options = Arg.align [
  "-p", Arg.Set pretty_print, 
  " Display the parsed input on stdout.";

  "--parse-only", Arg.Set parse_only, 
  " Do syntax analysis only.";

  "--bench", Arg.Set benchmark, 
  " Benchmark.";

  "--memo", Arg.Set Memo.flag, 
  " Memoization";

  "--type", Arg.Set Typecheck.flag, 
  " Typecheck before evaluation.";
  
  "--interact", Arg.Set interactive, (* ajout *) 
  " Interactive mode."
]

let parse_file filename =
  let parser lexer lexbuf = try
    Parser.program lexer lexbuf
  with
  | Parser.Error -> Error.error "Parsing" (Position.cpos lexbuf) "Unknown error.\n"
  in
  SyntacticAnalysis.process
    ~lexer_init: (fun filename -> Lexing.from_channel (open_in filename))
    ~lexer_fun: Lexer.main
    ~parser_fun: parser
    ~input: filename

let parse_string entry =
   let parser lexer lexbuf = try
    Parser.program lexer lexbuf
  with
  | Parser.Error -> Error.error "Parsing" (Position.cpos lexbuf) "Unknown error.\n"
  in
  SyntacticAnalysis.process
    ~lexer_init: (fun entry -> Lexing.from_string (entry))
    ~lexer_fun: Lexer.main
    ~parser_fun: parser
    ~input: entry


let filenames = ref []

let usage = "usage: clap [options] input1 ... inputN"

let _ = Arg.parse options (fun s -> filenames := s :: !filenames) usage

let output = Pprint.Channel.pretty 0.8 80 stdout 

let bench f = 
  let start = Unix.gettimeofday () in
  let y = f () in
  let stop = Unix.gettimeofday () in 
  (stop -. start, y)

let process_file filename = 
  let ast = parse_file filename in 
  if !pretty_print then 
    output (Printer.program ast);
  if not !parse_only then (
    Typecheck.program ast;
    let time, result = bench (fun () -> Interpreter.program ast) in
    if !benchmark then 
      output (Pprint.text (Printf.sprintf "TIME[%07.3fs %s]\n" time filename));
    output (Runtime.print_environment result);
    if !interactive then (* ajout *)
      let rec loop chaine = 
        let entry = read_line ()
        in if (String.get entry ((String.length entry)-1))=';' && (String.get entry ((String.length entry)-2))=';'
           then (let ast = parse_string (chaine^entry) in 
                if !pretty_print then 
                   output (Printer.program ast);
                if not !parse_only then (
                   Typecheck.program ast;
                   let time, result = bench (fun () -> Interpreter.program ast) in
                   if !benchmark then 
                      output (Pprint.text (Printf.sprintf "TIME[%07.3fs %s]\n" time filename));
                   output (Runtime.print_environment result);
                   (loop ""))) 
           else (loop (chaine^entry))
       in loop "";
  )
  
let asts = List.map process_file !filenames