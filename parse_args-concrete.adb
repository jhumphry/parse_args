-- parse_args-concrete.adb
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

pragma Restrictions(No_Implementation_Aspect_Specifications,
                    No_Implementation_Attributes,
                    No_Implementation_Identifiers,
                    No_Implementation_Units);

-- pragma Profile(No_Implementation_Extensions) is not used in this compilation
-- unit because it sets the restriction No_Implementation_Pragmas. A GNAT
-- specific pragma Unreferenced is being used to suppress unneccesary warnings
-- where parameters are intentionally not being used.

package body Parse_Args.Concrete is

   ----------------
   -- Set_Option --
   ----------------

   procedure Set_Option(O : in out Option_With_Argument;
                        A : in out Argument_Parser'Class) is

   begin
      if O.Set then
         A.State := Finish_Erroneous;
         A.Message := To_Unbounded_String("Argument cannot be specified twice.");
      else
         A.State := Required_Argument;
      end if;
   end Set_Option;

   ----------------
   -- Set_Option --
   ----------------

   procedure Set_Option(O : in out Concrete_Boolean_Option;
                        A : in out Argument_Parser'Class) is
      pragma Unreferenced(A);
   begin
      O.Set := True;
      O.Value := not O.Default;
   end Set_Option;

   ----------------
   -- Set_Option --
   ----------------

   procedure Set_Option(O : in out Repeated_Option;
                        A : in out Argument_Parser'Class) is
      pragma Unreferenced(A);
   begin
      O.Set := True;
      O.Value := O.Value + 1;
   end Set_Option;

   -------------------------
   -- Set_Option_Argument --
   -------------------------

   procedure Set_Option_Argument(O : in out Concrete_Integer_Option;
                                 Arg : in String;
                                 A : in out Argument_Parser'Class) is
   begin
      O.Set := True;
      O.Value := Integer'Value(Arg);
      if O.Value < O.Min or O.Value > O.Max then
         A.State := Finish_Erroneous;
         A.Message := To_Unbounded_String(Arg & " should be between " &
                                            Integer'Image(O.Min) & " and " &
                                            Integer'Image(O.Max));
      end if;
   exception
      when Constraint_Error =>
         A.State := Finish_Erroneous;
         A.Message := To_Unbounded_String("Not a valid integer: " & Arg);
   end Set_Option_Argument;

   -------------------------
   -- Set_Option_Argument --
   -------------------------

   procedure Set_Option_Argument(O : in out Concrete_String_Option;
                                 Arg : in String;
                                 A : in out Argument_Parser'Class) is
      pragma Unreferenced(A);
   begin
      O.Set := True;
      O.Value := To_Unbounded_String(Arg);
   end Set_Option_Argument;

   --------------
   -- Finalize --
   --------------

   procedure Finalize(Object : in out Concrete_String_Option) is
   begin
      Object.Value := Ada.Strings.Unbounded.Null_Unbounded_String;
      Object.Default := Ada.Strings.Unbounded.Null_Unbounded_String;
   end Finalize;

end Parse_Args.Concrete;
