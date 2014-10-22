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
   AP.Add_Boolean_Option("foo",'f', False);
   AP.Add_Boolean_Option("bar",'b', True);
   AP.Add_Repeated_Option("baz",'z', 0);
   AP.Add_Natural_Option("natural", 'n', 0);

   AP.Parse_Command_Line;

   if AP.Parse_Success then
      Put_Line("Command name is: " & AP.Command_Name);
      Put_Line("Value of option foo is: " & Boolean'Image(AP.Boolean_Value("foo")));
      Put_Line("Option foo was " & (if AP("foo").Set then "" else "not ") & "set on the command line.");
      Put_Line("Value of option bar is: " & Boolean'Image(AP.Boolean_Value("bar")));
      Put_Line("Option bar was " & (if AP("bar").Set then "" else "not ") & "set on the command line.");
      Put_Line("Value of option baz is: " & Natural'Image(AP.Natural_Value("baz")));
      Put_Line("Option baz was " & (if AP("baz").Set then "" else "not ") & "set on the command line.");
      Put_Line("Value of option natural is: " & Natural'Image(AP.Natural_Value("natural")));
      Put_Line("Option natural was " & (if AP("natural").Set then "" else "not ") & "set on the command line.");
   else
      Put_Line("Error while parsing command-line arguments: " & AP.Parse_Message);
   end if;

end Simple_Example;
