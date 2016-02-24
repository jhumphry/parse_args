-- parse_args_suite-parse_args_tests.adb
-- Unit tests for the Parse_Args project

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

with AUnit.Assertions;

with Parse_Args;
with Parse_Args.Testable;

package body Parse_Args_Suite.Parse_Args_Tests is

   use AUnit.Assertions;

   use Parse_Args;
   use Parse_Args.Testable;

   AP : Testable_Argument_Parser;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T: in out Parse_Args_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Check_Basic_Usage'Access,
                        "Check basic functionality");
   end Register_Tests;

   ----------
   -- Name --
   ----------

   function Name (T : Parse_Args_Test) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("Tests of Parse_Args package functionality");
   end Name;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out Parse_Args_Test) is
      pragma Unreferenced (T);
   begin

      -- Define options
      AP.Add_Option(O => Make_Boolean_Option(False),
                    Name => "foo",
                    Short_Option => 'f',
                    Usage => "Foo option");
      AP.Add_Option(O => Make_Boolean_Option(True),
                    Name => "bar",
                    Short_Option => 'b',
                    Usage => "Bar option");

      -- Add sample input
      AP.Set_Command_Name("parse_args_tests");
      AP.Clear_Arguments;
      AP.Append_Arguments((+"--foo",
                          +"-b"));
   end Set_Up;

   -----------------------
   -- Check_Basic_Usage --
   -----------------------

   procedure Check_Basic_Usage (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced(T);

   begin
      Assert(AP.Ready, "New Argument_Parser not ready for use");
      AP.Parse_Command_Line;
      Assert(AP.Parse_Success, "Argument_Parser did not parse successfully");

      Assert(AP.Command_Name = "parse_args_tests",
             "Cannot retrieve command name");

      Assert(AP.Boolean_Value("foo"),
             "Boolean option foo (default false) not toggled");

      Assert(not AP.Boolean_Value("bar"),
             "Boolean option bar (default true) not toggled");

   end Check_Basic_Usage;

end Parse_Args_Suite.Parse_Args_Tests;
