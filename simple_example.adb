-- simple_example

-- A simple example of the use of parse_args
-- Copyright James Humphry 2014

with Parse_Args;
use Parse_Args;

with Ada.Text_IO;
use Ada.Text_IO;

procedure Simple_Example is
   AP : Argument_Parser;
begin
   AP.Add_Option(Make_Boolean_Option(False), "foo", 'f');
   AP.Add_Option(Make_Boolean_Option(True), "bar", 'b');
   AP.Add_Option(Make_Repeated_Option(0), "baz", 'z');
   AP.Add_Option(Make_Natural_Option(0), "natural", 'n');
   AP.Add_Option(Make_Integer_Option(-1), "integer", 'i');
   AP.Add_Option(Make_String_Option(""), "string", 's');
   AP.Append_Positional(Make_String_Option("INFILE"), "infile");
   AP.Allow_Tail_Arguments;

   AP.Parse_Command_Line;

   if AP.Parse_Success then
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
