-- parse_args_suite-parse_args_tests.ads
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

with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Parse_Args_Suite.Parse_Args_Tests is

   type Parse_Args_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T: in out Parse_Args_Test);

   function Name (T : Parse_Args_Test) return Test_String;

   procedure Set_Up (T : in out Parse_Args_Test);

   procedure Check_Basics (T : in out Test_Cases.Test_Case'Class);

   procedure Check_Boolean_Usage (T : in out Test_Cases.Test_Case'Class);

   procedure Check_Repeated_Usage (T : in out Test_Cases.Test_Case'Class);

   procedure Check_Integer_Usage (T : in out Test_Cases.Test_Case'Class);

   procedure Check_String_Usage (T : in out Test_Cases.Test_Case'Class);

end Parse_Args_Suite.Parse_Args_Tests;
