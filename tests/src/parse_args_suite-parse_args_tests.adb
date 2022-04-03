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

with System.Assertions;
with AUnit.Assertions;

with Parse_Args;
with Parse_Args.Testable;

package body Parse_Args_Suite.Parse_Args_Tests is

   use AUnit.Assertions;

   use Parse_Args;
   use Parse_Args.Testable;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T: in out Parse_Args_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Check_Basics'Access,
                        "Check basic functionality");
      Register_Routine (T, Check_Boolean_Usage'Access,
                        "Check Boolean option functionality");
      Register_Routine (T, Check_Repeated_Usage'Access,
                        "Check Repeated option functionality");
      Register_Routine (T, Check_Integer_Usage'Access,
                        "Check Integer option functionality");
      Register_Routine (T, Check_String_Usage'Access,
                        "Check String option functionality");
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
      null;
   end Set_Up;

   ------------------
   -- Check_Basics --
   ------------------

   procedure Check_Basics (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced(T);

      function Setup_AP return Testable_Argument_Parser is
      begin
         return Result : Testable_Argument_Parser do
            Result.Add_Option(O => Make_Boolean_Option(False),
                              Name => "foo",
                              Short_Option => 'f');
            Result.Add_Option(O => Make_Boolean_Option(False),
                              Name => "bar",
                              Short_Option => 'b');
            Result.Add_Option(O => Make_Boolean_Option(False),
                              Name => "baz",
                              Short_Option => 'z');
            Result.Set_Command_Name("parse_args_tests");
         end return;
      end Setup_AP;
   begin

      declare
         AP : Testable_Argument_Parser := Setup_AP;
         Catch_Message_Too_Soon : Boolean := False;
         Catch_Repeated_Parsing : Boolean := False;
         Catch_No_Such_Argument : Boolean := False;
      begin
         AP.Clear_Arguments;
         AP.Append_Arguments((+"--foo",
                             +"-b"));

         Assert(AP.Ready, "New Argument_Parser not ready for use");

         begin
            declare
               Dummy : String := AP.Parse_Message;
            begin
               null;
            end;
         exception
            when Program_Error | System.Assertions.Assert_Failure =>
               Catch_Message_Too_Soon := True;
         end;

         Assert(Catch_Message_Too_Soon,
                "Returned a parse message before the parse has taken place");

         AP.Parse_Command_Line;

         Assert(AP.Parse_Success, "Argument_Parser did not parse successfully: " &
                  AP.Parse_Message);

         begin
            AP.Parse_Command_Line;
         exception
            when Program_Error | System.Assertions.Assert_Failure =>
               Catch_Repeated_Parsing := True;
         end;

         Assert(Catch_Repeated_Parsing,
                "Did not object to Parse_Command_Line being called twice");

         Assert(AP.Command_Name = "parse_args_tests",
                "Cannot retrieve command name");

         Assert(AP.Get("foo").Set,
                "Boolean option bar incorrectly not marked as set");
         Assert(AP.Get("bar").Set,
                "Boolean option bar incorrectly not marked as set");
         Assert(not AP.Get("baz").Set,
                "Boolean option baz incorrectly marked as set");

         begin
            declare
               Dummy : Boolean := AP.Boolean_Value("nosuch");
            begin
               null;
            end;
         exception
            when Constraint_Error =>
               Catch_No_Such_Argument := True;
         end;
         Assert(Catch_No_Such_Argument,
                "Returned a value for a non-existent option");
      end;

   end Check_Basics;

   -------------------------
   -- Check_Boolean_Usage --
   -------------------------

   procedure Check_Boolean_Usage (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced(T);

      function Setup_AP return Testable_Argument_Parser is
      begin
         return Result : Testable_Argument_Parser do
            Result.Add_Option(O => Make_Boolean_Option(False),
                              Name => "foo",
                              Short_Option => 'f');
            Result.Add_Option(O => Make_Boolean_Option(True),
                              Name => "bar",
                              Short_Option => 'b');
            Result.Add_Option(O => Make_Boolean_Option(False),
                              Name => "baz",
                              Short_Option => 'z',
                              Long_Option => "-");
            Result.Add_Option(O => Make_Boolean_Option(False),
                              Name => "bork",
                              Short_Option => '-',
                              Long_Option => "borkable");
            Result.Set_Command_Name("parse_args_tests");
         end return;
      end Setup_AP;
   begin

      declare
         AP : Testable_Argument_Parser := Setup_AP;
      begin
         AP.Clear_Arguments;
         AP.Append_Arguments((+"--foo",
                             +"-b",
                             +"--borkable"));

         AP.Parse_Command_Line;
         Assert(AP.Parse_Success, "Argument_Parser did not parse successfully: " &
                  AP.Parse_Message);

         Assert(AP.Boolean_Value("foo"),
                "Boolean option foo (default false) not toggled");
         Assert(not AP.Boolean_Value("bar"),
                "Boolean option bar (default true) not toggled via short option");
         Assert(AP.Boolean_Value("bork"),
                "Boolean option bork (default false) not toggled via renamed long option");
      end;

      declare
         AP : Testable_Argument_Parser := Setup_AP;
      begin
         AP.Clear_Arguments;
         AP.Append_Argument("-bz");

         AP.Parse_Command_Line;
         Assert(AP.Parse_Success, "Argument_Parser did not parse successfully");

         Assert(not AP.Boolean_Value("foo"),
                "Boolean option foo (default false) set despite not being present in option group");

         Assert(not AP.Boolean_Value("bar"),
                "Boolean option bar (default true) not toggled via short option group");
         Assert(AP.Get("baz").Set,
                "Boolean option baz (default false) not toggled via short option group");
      end;

      declare
         AP : Testable_Argument_Parser := Setup_AP;
      begin
         AP.Clear_Arguments;
         AP.Append_Arguments((+"--nonesuch", +"--foo"));
         AP.Parse_Command_Line;
         Assert(not AP.Parse_Success,
                "Parse successful despite passing non-existent long option");
      end;

      declare
         AP : Testable_Argument_Parser := Setup_AP;
      begin
         AP.Clear_Arguments;
         AP.Append_Arguments((+"-n", +"--foo"));
         AP.Parse_Command_Line;
         Assert(not AP.Parse_Success,
                "Parse successful despite passing non-existent short option");
      end;

      declare
         AP : Testable_Argument_Parser := Setup_AP;
      begin
         AP.Clear_Arguments;
         AP.Append_Argument("-fnz");
         AP.Parse_Command_Line;
         Assert(not AP.Parse_Success,
                "Parse successful despite passing non-existent grouped short option");
      end;

      declare
         AP : Testable_Argument_Parser := Setup_AP;
      begin
         AP.Clear_Arguments;
         AP.Append_Arguments((+"--foo", +"invalidarg"));
         AP.Parse_Command_Line;
         Assert(not AP.Parse_Success,
                "Parse successful despite passing an argument to a Boolean option");
      end;

      declare
         AP : Testable_Argument_Parser := Setup_AP;
      begin
         AP.Clear_Arguments;
         AP.Append_Argument("--baz");
         AP.Parse_Command_Line;
         Assert(not AP.Parse_Success,
                "Parse successful despite using a long option name on a short-name only option");
      end;

      declare
         AP : Testable_Argument_Parser := Setup_AP;
      begin
         AP.Clear_Arguments;
         AP.Append_Argument("--bork");
         AP.Parse_Command_Line;
         Assert(not AP.Parse_Success,
                "Parse successful despite using the underlying option name for a renamed long option");
      end;

   end Check_Boolean_Usage;

   --------------------------
   -- Check_Repeated_Usage --
   --------------------------

   procedure Check_Repeated_Usage (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced(T);

      function Setup_AP return Testable_Argument_Parser is
      begin
         return Result : Testable_Argument_Parser do
            Result.Add_Option(O => Make_Repeated_Option,
                              Name => "foo",
                              Short_Option => 'f');
            Result.Add_Option(O => Make_Repeated_Option,
                              Name => "bar",
                              Short_Option => 'b');
            Result.Add_Option(O => Make_Repeated_Option(5),
                              Name => "baz",
                              Short_Option => 'z');
            Result.Add_Option(O => Make_Boolean_Option,
                              Name => "snafu",
                              Short_Option => 's');
            Result.Set_Command_Name("parse_args_tests");
         end return;
      end Setup_AP;
   begin

      declare
         AP : Testable_Argument_Parser := Setup_AP;
      begin
         AP.Clear_Arguments;
         AP.Append_Arguments((+"--foo",
                             +"--foo",
                             +"-b", +"-b"));

         AP.Parse_Command_Line;
         Assert(AP.Parse_Success, "Argument_Parser did not parse successfully: " &
                  AP.Parse_Message);

         Assert(AP.Integer_Value("foo") = 2,
                "Repeated options (using long option type) not working");
         Assert(AP.Integer_Value("bar") = 2,
                "Repeated options (using short option type) not working");
         Assert(AP.Integer_Value("baz") = 5,
                "Repeated options defaults not working");
      end;

      declare
         AP : Testable_Argument_Parser := Setup_AP;
      begin
         AP.Clear_Arguments;
         AP.Append_Arguments((+"-fbfb",
                             +"--baz",
                             +"-z", +"-z"));

         AP.Parse_Command_Line;
         Assert(AP.Parse_Success, "Argument_Parser did not parse successfully: " &
                  AP.Parse_Message);

         Assert(AP.Integer_Value("foo") = 2,
                "Repeated options (using short option group) not working");
         Assert(AP.Integer_Value("bar") = 2,
                "Repeated options (using short option group) not working");
         Assert(AP.Integer_Value("baz") = 8,
                "Repeated optionsw with a default and mixed long and short " &
                  "options are not working");
      end;

      declare
         AP : Testable_Argument_Parser := Setup_AP;
      begin
         AP.Clear_Arguments;
         AP.Append_Arguments((+"-fz",
                             +"--snafu",
                             +"--baz",
                             +"-z",
                             +"-z",
                             +"--foo"));

         AP.Parse_Command_Line;
         Assert(AP.Parse_Success, "Argument_Parser did not parse successfully: " &
                  AP.Parse_Message);

         Assert(AP.Integer_Value("foo") = 2,
                "Repeated options (using short option group) not working");
         Assert(AP.Integer_Value("bar") = 0,
                  "Repeated options (with no default set and not invoked) not " &
                  "returning 0 when retrieved");
         Assert(AP.Integer_Value("baz") = 9,
                "Repeated optionsw with a default and mixed long and short " &
                  "options are not working");
      end;

   end Check_Repeated_Usage;

   -------------------------
   -- Check_Integer_Usage --
   -------------------------

   procedure Check_Integer_Usage (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced(T);

      function Setup_AP return Testable_Argument_Parser is
      begin
         return Result : Testable_Argument_Parser do
            Result.Add_Option(O => Make_Integer_Option,
                              Name => "foo",
                              Short_Option => 'f');
            Result.Add_Option(O => Make_Natural_Option,
                              Name => "bar",
                              Short_Option => 'b');
            Result.Add_Option(O => Make_Positive_Option,
                              Name => "baz",
                              Short_Option => 'z');
            Result.Add_Option(O => Make_Integer_Option(Default => 15,
                                                       Min     => 10,
                                                       Max     => 20),
                              Name => "coz",
                              Short_Option => 'c');
            Result.Add_Option(O => Make_Boolean_Option,
                              Name => "door",
                              Short_Option => 'd');
            Result.Set_Command_Name("parse_args_tests");
         end return;
      end Setup_AP;

      Catch_No_Such_Argument : Boolean := False;

   begin

      declare
         AP : Testable_Argument_Parser := Setup_AP;
      begin
         AP.Clear_Arguments;
         AP.Append_Arguments((+"--foo", +"5",
                             +"-c", +"12"));

         AP.Parse_Command_Line;
         Assert(AP.Parse_Success, "Argument_Parser did not parse successfully: " &
                  AP.Parse_Message);

         Assert(AP.Integer_Value("foo") = 5,
                "Integer option (long form) not working");
         Assert(AP.Integer_Value("bar") = 0,
                "Natural Option default not correct");
         Assert(AP.Integer_Value("baz") = 1,
                "Positive option default not correct");
         Assert(AP.Integer_Value("coz") = 12,
                "Integer option (short form) with custom range not working");
      end;

      declare
         AP : Testable_Argument_Parser := Setup_AP;
      begin
         AP.Clear_Arguments;
         AP.Append_Arguments((+"-f", +"-7"));
         AP.Append_Arguments((+"-z", +"16#FF#"));

         AP.Parse_Command_Line;
         Assert(AP.Parse_Success, "Argument_Parser did not parse successfully: " &
                  AP.Parse_Message);

         Assert(AP.Integer_Value("foo") = -7,
                "Positive option did not accept negative input");
         Assert(AP.Integer_Value("baz") = 255,
                "Positive option did not accept hex input");
      end;

      declare
         AP : Testable_Argument_Parser := Setup_AP;
      begin
         AP.Clear_Arguments;
         AP.Append_Arguments((+"-df", +"8"));

         AP.Parse_Command_Line;
         Assert(AP.Parse_Success, "Argument_Parser did not parse successfully: " &
                  AP.Parse_Message);

         Assert(AP.Integer_Value("foo") = 8,
                "Integer option (short group) not working");
      end;

      declare
         AP : Testable_Argument_Parser := Setup_AP;
      begin
         AP.Clear_Arguments;
         AP.Append_Arguments((+"-z", +"0"));

         AP.Parse_Command_Line;
         Assert(not AP.Parse_Success,
               "Argument parser did not reject 0 as input for Positive option");
      end;

      declare
         AP : Testable_Argument_Parser := Setup_AP;
      begin
         AP.Clear_Arguments;
         AP.Append_Arguments((+"-c", +"8"));

         AP.Parse_Command_Line;
         Assert(not AP.Parse_Success,
                "Argument parser did not reject out-of range value for " &
                  "customised Integer option");
      end;

      declare
         AP : Testable_Argument_Parser := Setup_AP;
      begin
         AP.Clear_Arguments;
         AP.Append_Arguments((+"-f", +"--bar", +"8"));

         AP.Parse_Command_Line;
         Assert(not AP.Parse_Success,
                "Argument parser did not reject missing option value for an" &
               "Integer option");
      end;

      declare
         AP : Testable_Argument_Parser := Setup_AP;
      begin
         AP.Clear_Arguments;
         AP.Append_Arguments((+"-fb", +"8"));

         AP.Parse_Command_Line;
         Assert(not AP.Parse_Success,
                "Argument parser did not reject missing option value for an" &
               "Integer option as part of a short option group");
      end;

      declare
         AP : Testable_Argument_Parser := Setup_AP;
      begin
         AP.Clear_Arguments;
         AP.Append_Arguments((+"-f", +"8",
                            +"--foo", +"9"));

         AP.Parse_Command_Line;
         Assert(not AP.Parse_Success,
                "Argument parser did not reject specifying an Integer option" &
               "twice.");
      end;

      declare
         AP : Testable_Argument_Parser := Setup_AP;
      begin
         AP.Parse_Command_Line;
         declare
            Dummy : Integer := AP.Integer_Value("nosuch");
         begin
            null;
         end;
      exception
         when Constraint_Error =>
            Catch_No_Such_Argument := True;
      end;

      Assert(Catch_No_Such_Argument,
             "Returned a value for a non-existent integer option");

   end Check_Integer_Usage;

   ------------------------
   -- Check_String_Usage --
   ------------------------

   procedure Check_String_Usage (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced(T);

      function Setup_AP return Testable_Argument_Parser is
      begin
         return Result : Testable_Argument_Parser do
            Result.Add_Option(O => Make_String_Option,
                              Name => "foo",
                              Short_Option => 'f');
            Result.Add_Option(O => Make_String_Option(Default => "Hello"),
                              Name => "bar",
                              Short_Option => 'b');
            Result.Add_Option(O => Make_String_Option,
                              Name => "baz",
                              Short_Option => 'z');
            Result.Set_Command_Name("parse_args_tests");
         end return;
      end Setup_AP;

      Catch_No_Such_Argument : Boolean := False;

   begin

      declare
         AP : Testable_Argument_Parser := Setup_AP;
      begin
         AP.Clear_Arguments;
         AP.Append_Arguments((+"--foo", +"5",
                             +"-z", +"Goodbye"));

         AP.Parse_Command_Line;
         Assert(AP.Parse_Success, "Argument_Parser did not parse successfully: " &
                  AP.Parse_Message);

         Assert(AP.String_Value("foo") = "5",
                "String option (long form) not working");
         Assert(AP.String_Value("bar") = "Hello",
                "String Option default not correct");
         Assert(AP.String_Value("baz") = "Goodbye",
                "String option (short form) not working");
      end;

      declare
         AP : Testable_Argument_Parser := Setup_AP;
      begin
         AP.Parse_Command_Line;
         declare
            Dummy :String := AP.String_Value("nosuch");
         begin
            null;
         end;
      exception
         when Constraint_Error =>
            Catch_No_Such_Argument := True;
      end;

      Assert(Catch_No_Such_Argument,
             "Returned a value for a non-existent string option");

   end Check_String_Usage;

end Parse_Args_Suite.Parse_Args_Tests;
