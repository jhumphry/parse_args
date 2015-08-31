-- parse_args-concrete.ads
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

-- This private child package contains some concrete implementations of
-- different options. It is not expected that package users will create these
-- types directly. Instead the factory functions in the parent package are used,
-- as these ensure that the options will be indexed and referenced correctly.

pragma Profile(No_Implementation_Extensions);

private package Parse_Args.Concrete is

   -- Concrete options are not exposed to ensure that any options are not added
   -- to Argument_Parser inconsistently.

   type Option_With_Argument is abstract new Option with null record;
   procedure Set_Option(O : in out Option_With_Argument; A : in out Argument_Parser'Class);

   type Concrete_Boolean_Option is new Option and Boolean_Option with record
      Value : Boolean := False;
      Default : Boolean := False;
   end record;
   procedure Set_Option(O : in out Concrete_Boolean_Option; A : in out Argument_Parser'Class);
   function Image(O : in  Concrete_Boolean_Option) return String is (Boolean'Image(O.Value));
   function Value(O : in Concrete_Boolean_Option) return Boolean is (O.Value);

   type Concrete_Integer_Option is new Option_With_Argument and Integer_Option with record
      Value : Integer := 0;
      Default : Integer := 0;
      Min : Integer := Integer'First;
      Max : Integer := Integer'Last;
   end record;
   procedure Set_Option_Argument(O : in out Concrete_Integer_Option;
                                 Arg : in String;
                                 A : in out Argument_Parser'Class);
   function Image(O : in  Concrete_Integer_Option) return String is (Integer'Image(O.Value));
   function Value(O : in Concrete_Integer_Option) return Integer is (O.Value);

   type Repeated_Option is new Concrete_Integer_Option with null record;
   procedure Set_Option(O : in out Repeated_Option; A : in out Argument_Parser'Class);

   type Concrete_String_Option is new Option_With_Argument and String_Option with record
      Value : Unbounded_String := Null_Unbounded_String;
      Default : Unbounded_String := Null_Unbounded_String;
   end record;
   procedure Set_Option_Argument(O : in out Concrete_String_Option;
                                 Arg : in String;
                                 A : in out Argument_Parser'Class);
   function Value(O : in Concrete_String_Option) return String is (To_String(O.Value));
   function Image(O : in Concrete_String_Option) return String renames Value;
   overriding procedure Finalize(Object : in out Concrete_String_Option);

end Parse_Args.Concrete;
