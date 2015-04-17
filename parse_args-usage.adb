-- parse_args-usage.adb
-- A simple command line option parser

-- Copyright (c) 2014 - 2015, James Humphry
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

pragma Profile(No_Implementation_Extensions);

with Ada.Text_IO, Ada.Strings;
use Ada.Text_IO, Ada.Strings;

with Ada.Containers;
use type Ada.Containers.Count_Type;

separate (Parse_Args)
procedure Usage(A : in Argument_Parser) is

   First_Positional : Boolean := True;

begin
   Put("Usage: ");
   Put(A.Command_Name);
   if A.Option_Help_Details.Length > 0 then
      Put(" [OPTIONS]...");
   end if;

   for I of A.Option_Help_Details loop
      if I.Positional then
         if First_Positional then
            Put(" [--] ");
            First_Positional := False;
         end if;
         Put(" [" & To_String(I.Name) & "]");
      end if;
   end loop;

   if A.Allow_Tail then
      Put(" [" & To_String(A.Tail_Usage) & "]...");
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
            Put("--" & To_String(I.Long_Option) & " ");
         end if;

         Set_Col(20);
         Put(To_String(I.Usage));
         New_Line;
      end if;
   end loop;

end Usage;
