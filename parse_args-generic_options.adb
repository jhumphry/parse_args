-- parse_args-generic_discrete_option.ads
-- A simple command line option parser

-- Copyright (c) 2015, James Humphry
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

package body Parse_Args.Generic_Options is

   use Ada.Finalization;

   ----------------
   -- Set_Option --
   ----------------

   procedure Set_Option
     (O : in out Element_Option;
      A : in out Argument_Parser'Class)
   is
   begin
      if O.Set then
         A.State := Finish_Erroneous;
         A.Message := To_Unbounded_String("Argument cannot be specified twice.");
      else
         A.State := Required_Argument;
      end if;
   end Set_Option;

   -------------------------
   -- Set_Option_Argument --
   -------------------------

   procedure Set_Option_Argument
     (O   : in out Element_Option;
      Arg : in     String;
      A   : in out Argument_Parser'Class)
   is
      Constraint_Met : Boolean := True;
   begin
      O.Set := True;
      O.Value := Value(Arg);
      Valid(O.Value, Constraint_Met);
      if not Constraint_Met then
         A.State := Finish_Erroneous;
         A.Message := To_Unbounded_String(Arg & " does not meet constraints");
      end if;
   exception
      when Constraint_Error =>
         A.State := Finish_Erroneous;
         A.Message := To_Unbounded_String("Not a valid value: " & Arg);
   end Set_Option_Argument;

   -----------
   -- Value --
   -----------

   function Value (A : in Argument_Parser; Name : in String) return Element is
   begin
      if A.Arguments.Contains(Name) then
         if A.Arguments(Name).all in Element_Option'Class then
            return Element_Option'Class(A.Arguments(Name).all).Value;
         else
            raise Constraint_Error with "Argument " & Name
              & " is not of the right type.";
         end if;
      else
         raise Constraint_Error with "No argument: " & Name & ".";
      end if;
   end Value;

   -----------------
   -- Make_Option --
   -----------------

   function Make_Option
     (Default : in Element := Fallback_Default)
      return Option_Ptr
   is
     (new Element_Option'(Limited_Controlled with
                          Set => False,
                          Value => Default,
                          Default => Default
                         ));

end Parse_Args.Generic_Options;
