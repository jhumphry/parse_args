-- generic_example
-- Anexample of the use of parse_args with

-- Copyright (c) 2015, James Humphry
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

with Parse_Args, Parse_Args.Generic_Discrete_Option;
use Parse_Args;

with Ada.Text_IO;
use Ada.Text_IO;

procedure Generic_Example is
   AP : Argument_Parser;

   type Compass is (North, South, East, West);

   package Compass_Option is new Generic_Discrete_Option(Element => Compass,
                                                         Fallback_Default => North);

   C : aliased Compass_Option.Element_Option := Compass_Option.Make_Option;

begin
   AP.Add_Option(Make_Boolean_Option(False), "help", 'h', Usage => "Display this help text");
   AP.Add_Option(C'Unchecked_Access, "compass", 'c', Usage => "A compass point (North, South, East or West)");
   AP.Set_Prologue("A demonstration of the Parse_Args library with generic types.");

   AP.Parse_Command_Line;

   if AP.Parse_Success and then AP.Boolean_Value("help") then
      AP.Usage;
   elsif AP.Parse_Success then
      Put_Line("Compass point specified: " & Compass'Image(Compass_Option.Value(AP, "compass")));
   else
      Put_Line("Error while parsing command-line arguments: " & AP.Parse_Message);
   end if;

end Generic_Example;
