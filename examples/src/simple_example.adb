-- simple_example
-- A simple example of the use of parse_args

-- Copyright (c) 2014, James Humphry
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
-- REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
-- AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
-- INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
-- LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
-- OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
-- PERFORMANCE OF THIS SOFTWARE.

with Parse_Args;
use Parse_Args;

with Parse_Args.Integer_Array_Options;

with Ada.Text_IO;
use Ada.Text_IO;

procedure Simple_Example is
   AP : Argument_Parser;
begin
   AP.Add_Option(Make_Boolean_Option(False), "help", 'h', Usage => "Display this help text");
   AP.Add_Option(Make_Boolean_Option(False), "foo", 'f', Usage => "The foo option");
   AP.Add_Option(Make_Boolean_Option(True), "bar", 'b', Usage => "The bar option");
   AP.Add_Option(Make_Repeated_Option(0), "baz", 'z',
                 Usage => "The baz option (can be repeated for more baz)");
   AP.Add_Option(Make_Boolean_Option(False), "long-only",
                 Long_Option => "long-only",
                 Usage => "The --long-only option has no short version");
   AP.Add_Option(Make_Boolean_Option(False), "short-only",
                 Short_Option => 'x', Long_Option => "-",
                 Usage => "The -x option has no long version");
   AP.Add_Option(Make_Natural_Option(0), "natural", 'n', Usage => "Specify a natural number argument");
   AP.Add_Option(Make_Integer_Option(-1), "integer", 'i', Usage => "Specify an integer argument");
   AP.Add_Option(Make_String_Option(""), "string", 's', Usage => "Specify a string argument");
   AP.Add_Option(Integer_Array_Options.Make_Option, "array", 'a',
                 Usage => "Specify a comma-separated integer array argument");
   AP.Append_Positional(Make_String_Option("INFILE"), "INFILE");
   AP.Allow_Tail_Arguments("TAIL-ARGUMENTS");
   AP.Set_Prologue("A demonstration of the basic features of the Parse_Args library.");

   AP.Parse_Command_Line;

   if AP.Parse_Success and then AP.Boolean_Value("help") then
      AP.Usage;
   elsif AP.Parse_Success then
      Put_Line("Command name is: " & AP.Command_Name);
      New_Line;

      for I in AP.Iterate loop
         Put_Line("Option "& Option_Name(I) & " was " &
                  (if AP(I).Set then "" else "not ") &
                    "set on the command line. Value: " &
                    AP(I).Image);
      end loop;
      New_Line;

      Put_Line("There were: " & Integer'Image(Integer(AP.Tail.Length)) & " tail arguments.");
      declare
         I : Integer := 1;
      begin
         for J of AP.Tail loop
            Put_Line("Argument" & Integer'Image(I) & " is: " & J);
            I := I + 1;
         end loop;
      end;

   else
      Put_Line("Error while parsing command-line arguments: " & AP.Parse_Message);
   end if;

end Simple_Example;
