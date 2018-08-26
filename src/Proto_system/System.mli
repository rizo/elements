open Kernel

val time : (unit -> 'a) -> 'a


module Path : sig
  type t

  val make : string -> t
  val to_string : t -> string
end

type path = Path.t

val path : string -> path


(* (** {6 Input/Output} *) *)

module IO : sig

  module Channel : sig
    type input
    (** The type of channels that provide read-only operations. *)

    type output
    (** The type of channels that provide write-only operations. *)

    val make_input : ?binary:bool -> path -> input
    (** [make_input ?binary path] is an {!input} channel open for reading from
        a file located at [path]. *)

    val make_output : ?binary:bool -> ?append:bool ->
      ?exclusive:bool -> ?perm:int -> string -> output
    (** [make_output ?binary ?append ?exclusive ?perm path] is an {!output}
        channel open for writing to a file located at [path]. A new file will
        be created at [path], with permissions defined by [perm], if it does
        not exist. If the file at [path] already exists and [exclusive] is set
        to [true] an exception will be risen. The content of the file will be
        truncated unless [append] is set to [true].

        {e Defaults:}

        @param binary = [true] Read the content of the file in binary mode
          if [true] and in textual mode otherwise.
        @param append = [false] Append the content to an existing file (if
          [true] instead of truncating it (if [false]).
        @param exclusive = [false] Raise {!Sys_error} if the file already
          exists.
        @param perm = [0o666] The permissions for the file if it does not
          exist.

        {e Errors:}

        @raise Sys_error if [exclusive] is [true] and the file already exists.
        @raise Sys_error if [path] does not point to a valid file.
        @raise Sys_error if the user does not have write access to the file. *)


    (* {6 Input operations} *)

    val read : input -> string option
    (** [read input] is the content of the channel [input] as a string.

        {[
          let chan = make_input "/tmp/data.txt" in
          let data = read chan in
          print (format "Char count: %d" (String.length data));
          close_input chan
        ]} *)

    val read_line : input -> string option
    val read_char : input -> char option
    val read_byte : input -> int option


    (* {6 Output operations} *)

    val write : string -> output -> unit

    val write_line : string -> output -> unit
    (** [write_line string input] is [write (string ++ "\n") input]. *)

    val write_char : char -> output -> unit
    val write_byte : int -> output -> unit


    (* {6 Iterators} *)

    val iter : input -> string Iter.t
    val read_lines : input -> string Iter.t
    (** [read_lines input] is a line iterator for the [input] channel.

        {!Channel.iter} is an alias for [read_lines].

        {[
          let words = File.read_lines (path "/usr/share/dict/words") in
          print (format "There are %d words in the dict!" (Iter.length words))
        ]} *)

    val read_chars : input -> char Iter.t
    (** [read_chars input] is a character iterator for the [input] channel. *)


    (** {6 Sinks} *)

    val into : output -> string Iter.t -> unit
    val write_lines : output -> string Iter.t -> unit
    (** [write_lines output iter] writes strings produced by iter as lines into
        the [output] channel.

        {!Channel.into} is an alias for [write_lines]. *)

    val write_chars : output -> char Iter.t -> unit
    (** [write_chars output iter] writes characters produced by [iter] into the
        [output] channel. *)

    val write_strings : output -> string Iter.t -> unit
    (** [write_strings output iter] writes strings produced by [iter] into the
        [output] channel. *)

    val close_input : input -> unit
    val close_output : output -> unit
  end

  module File : sig
    val read_lines : path -> string Iter.t
    (** [read_lines path] is a line iterator for a file located at
        a given [path].

        For more operations on lines see {!Channel.Lines} module.

        {[
          let words = File.read_lines (path "/usr/share/dict/words") in
          print (format "There are %d words in the dict!" (Iter.length words))
        ]} *)

    val read_chars : path -> char Iter.t
    (** [read_chars output] is a safe character iterator for a file located at
        a given [path]. *)

    val write_lines : path -> string Iter.t -> unit
    (** [write_lines path iter] writes strings produced by iter as lines into
        the file located at [path].

        {[
          Iter.repeat 10 "Hello, world!"
          |> File.write_lines (path "/tmp/data.txt")
        ]} *)

    val write_chars : path -> char Iter.t -> unit
    (** [write_chars path iter] writes characters produced by [iter] into the
        file located at a given [path].

        {[
          Iter.range 97 123
          |> Iter.map char
          |> File.chars_into (path "/tmp/ascii.txt")
        ]} *)

    val write_strings : path -> string Iter.t -> unit
    (** [write_strings path iter] writes strings produced by [iter] into a file
        located at a given [path]. *)
  end

  val stdin : Channel.input
  (** [stdin] is the standard input channel of the process. *)

  val stdout : Channel.output
  (** [stdout] is the standard output channel of the process. *)

  val stderr : Channel.output
  (** [stderr] is the standard error channel of the process. *)

  val open_file : ?binary:bool -> path -> Channel.input
  (** [open_file ?binary path] is an input channel for a file at [path].

      {[
        (* Open a file for reading in textual mode. *)
        open_file ~binary:false (path "/var/log/system.log")
      ]} *)

  val create_file : ?binary:bool -> ?append:bool -> ?exclusive:bool ->
    ?perm:int -> string -> Channel.output
  (** [create_file ?binary ?append ?exclusive ?perm path] is an output channel
      for a file at [path].

      {[
        (* Open a file for appending, creating it if it doesn't exist. *)
        create_file ~append:true (path "/tmp/data.txt")

          (* Open a file for writing and truncate its content. *)
          create_file (path "/tmp/data.txt")
      ]} *)

  val print : ?output: Channel.output -> ?break:string -> string -> unit
  (** [print ?output ?break string] outputs [string] on the [output]
      channel followed by [break].

      {e Defaults:}

      @param output = {!stdout}
      @param terminator = ["\n"]

      {[
        print "hello";
        print "hello, world" ~break:"!!!\n";
        print (format "hello, %s!" "world");
        print ~output:stderr "hello";
      ]} *)
end
