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
   AP.Add_Boolean_Option("test");
   AP.Parse_Command_Line;
   Put_Line("Command name is: " & AP.Command_Name);
   declare
      test : Boolean_Argument'Class := Boolean_Argument'Class(AP("test").all);
   begin
      Put_Line("Value of option test is: " & Boolean'Image(test.Value));
   end;

end Simple_Example;
