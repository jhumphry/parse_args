-- parse_args-testable.ads
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

with Ada.Containers.Vectors;
with Ada.Strings;
with Ada.Strings.Unbounded;

package Parse_Args.Testable is

   subtype Unbounded_String is Ada.Strings.Unbounded.Unbounded_String;

   function "+"(Source : in String) return Ada.Strings.Unbounded.Unbounded_String
                renames Ada.Strings.Unbounded.To_Unbounded_String;

   type Unbounded_String_Array is array (Integer range <>)
     of Ada.Strings.Unbounded.Unbounded_String;

   -- The package Unbounded_String_Vector and the actual record extension for
   -- Testable_Argument_Parser should be in the private part of this package.
   -- However GNAT GPL 2015 chokes if this is the case, so they have to be left
   -- public. As this is package is only used for testing, we can live with
   -- this.

   package Unbounded_String_Vector is
     new Ada.Containers.Vectors(Index_Type   => Positive,
                                Element_Type => Unbounded_String,
                               "="           => Ada.Strings.Unbounded."=");

   type Testable_Argument_Parser is new Argument_Parser with
      record
         Command_Name_Override : Unbounded_String
           :=  Ada.Strings.Unbounded.Null_Unbounded_String;
         Input_Arguments : Unbounded_String_Vector.Vector
           := Unbounded_String_Vector.Empty_Vector;
      end record;

   overriding function Command_Name(A : in Testable_Argument_Parser) return String;
   overriding function Argument_Count(A : in Testable_Argument_Parser)
                                      return Natural;
   overriding function Argument(A : in Testable_Argument_Parser;
                                Number : in Positive)
                     return String;
   not overriding procedure Clear_Arguments(A : in out Testable_Argument_Parser);
   not overriding procedure Append_Argument(A : in out Testable_Argument_Parser;
                                            S : in String);
   not overriding procedure Append_Arguments(A : in out Testable_Argument_Parser;
                                             S : in Unbounded_String_Array);
   not overriding procedure Set_Command_Name(A : in out Testable_Argument_Parser;
                                             S : in String);

private

end Parse_Args.Testable;
