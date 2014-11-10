-- parse_args-usage.adb

-- A simple command line option parser
-- Copyright James Humphry 2014

with Ada.Text_IO, Ada.Strings;
use Ada.Text_IO, Ada.Strings;

with Ada.Containers;
use type Ada.Containers.Count_Type;

separate (Parse_Args)
procedure Usage(A : in Argument_Parser) is
begin
   Put("Usage: ");
   Put(A.Command_Name);
   if A.Option_Help_Details.Length > 0 then
      Put(" [OPTIONS]...");
   end if;

   for I of A.Option_Help_Details loop
      if I.Positional then
         Put(" [" & To_String(I.Name) & "]");
      end if;
   end loop;

   if A.Allow_Tail then
      Put("...");
   end if;

   New_Line;

   if Length(A.Prologue) > 0 then
      Put_Line(To_String(A.Prologue));
   end if;
   New_Line;

   for I of A.Option_Help_Details loop
      if not I.Positional then

         if I.Short_Option /= '-' then
            Set_Col(3);
            Put("-" & I.Short_Option & ", ");
         end if;

         if I.Long_Option /= Null_Unbounded_String then
            Set_Col(7);
            Put("--" & To_String(I.Long_Option));
         end if;

         Set_Col(20);
         Put(To_String(I.Usage));
         New_Line;
      end if;
   end loop;

end Usage;
