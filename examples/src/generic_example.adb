-- generic_example
-- An example of the use of parse_args with generic option types

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

with Parse_Args;
use Parse_Args;

with Ada.Text_IO;
use Ada.Text_IO;

with Generic_Example_Options;
use Generic_Example_Options;

procedure Generic_Example is
   AP : Argument_Parser;

begin
   AP.Add_Option(Make_Boolean_Option(False), "help", 'h', Usage => "Display this help text");
   AP.Add_Option(Compass_Option.Make_Option, "compass", 'c',
                 Usage => "A compass point (North (default), South, East or West)");
   AP.Add_Option(Even_Option.Make_Option, "even", 'e',
                 Usage => "An even natural number (default 0)");
   AP.Add_Option(Float_Option.Make_Option, "float", 'f',
                 Usage => "A floating-point number (default 0.0)");
   AP.Add_Option(Float_Array_Option.Make_Option, "float-array", 'g',
                 Usage => "An array of floating-point numbers");
   AP.Set_Prologue("A demonstration of the Parse_Args library with generic types.");

   AP.Parse_Command_Line;

   if AP.Parse_Success and then AP.Boolean_Value("help") then
      AP.Usage;

   elsif AP.Parse_Success then
      Put_Line("Compass point specified: " & Compass'Image(Compass_Option.Value(AP, "compass")));
      Put_Line("Even number specified: " & Natural'Image(Even_Option.Value(AP, "even")));
      Put_Line("Floating-point number specified: " & Float'Image(Float_Option.Value(AP, "float")));
      if Float_Array_Option.Value(AP, "float-array") /= null then
         Put_Line("Floating-point number array: ");
         for I of Float_Array_Option.Value(AP, "float-array").all loop
            Put(Float'Image(I) & ", ");
         end loop;
      else
         Put_Line("No floating-point array specified.");
      end if;

   else
      Put_Line("Error while parsing command-line arguments: " & AP.Parse_Message);

   end if;

end Generic_Example;
