-- parse_args-testable.adb
-- A testable version of the Argument_Parser type

-- Copyright (c) 2016, James Humphry
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

package body Parse_Args.Testable is

   ------------------
   -- Command_Name --
   ------------------

   overriding function Command_Name (A : in Testable_Argument_Parser)
      return String is
      (To_String(A.Command_Name_Override));

   --------------------
   -- Argument_Count --
   --------------------

   overriding function Argument_Count (A : in Testable_Argument_Parser)
      return Natural is
      (Natural(A.Input_Arguments.Length));

   --------------
   -- Argument --
   --------------

   overriding function Argument (A : in Testable_Argument_Parser;
                                 Number : in Positive)
      return String is
     (To_String(A.Input_Arguments(Number)));

   ---------------------
   -- Clear_Arguments --
   ---------------------

   not overriding procedure Clear_Arguments (A : in out Testable_Argument_Parser)
   is
   begin
      A.Input_Arguments.Clear;
   end Clear_Arguments;

   ---------------------
   -- Append_Argument --
   ---------------------

   not overriding procedure Append_Argument (A : in out Testable_Argument_Parser;
                                             S : in String) is
   begin
      A.Input_Arguments.Append(To_Unbounded_String(S));
   end Append_Argument;

   ----------------------
   -- Append_Arguments --
   ----------------------

   not overriding procedure Append_Arguments (A : in out Testable_Argument_Parser;
                                              S : in Unbounded_String_Array) is
   begin
      for I of S loop
         A.Input_Arguments.Append(I);
      end loop;
   end Append_Arguments;

   ----------------------
   -- Set_Command_Name --
   ----------------------

   not overriding procedure Set_Command_Name(A : in out Testable_Argument_Parser;
                                             S : in String) is
   begin
      A.Command_Name_Override := To_Unbounded_String(S);
   end Set_Command_Name;

end Parse_Args.Testable;
