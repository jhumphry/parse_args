-- parse_args-generic_indefinite_options.ads
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

generic
   type Element(<>);
   type Element_Access is access Element;
   with function Value (S : String) return Element_Access;
   with function Image (Arg : Element_Access) return String;
   with procedure Free_Element(X : in out Element_Access) is null;
   with procedure Valid (Arg : in Element_Access; Result : in out Boolean) is null;
package Parse_Args.Generic_Indefinite_Options is

   type Element_Option is new Option with private;
   overriding function Image (O : in Element_Option) return String;
   not overriding function Value (O : in Element_Option) return Element_Access;

   function Value(A : in Argument_Parser; Name : in String) return Element_Access;

   function Make_Option return Option_Ptr;

private

   type Element_Option is new Option with record
      Value   : Element_Access;
   end record;

   overriding procedure Finalize(Object : in out Element_Option);
   procedure Set_Option
     (O : in out Element_Option;
      A : in out Argument_Parser'Class);
   procedure Set_Option_Argument
     (O   : in out Element_Option;
      Arg : in     String;
      A   : in out Argument_Parser'Class);
   overriding function Image (O : in Element_Option) return String is
     (Image(O.Value));
   not overriding function Value (O : in Element_Option) return Element_Access is
     (O.Value);

end Parse_Args.Generic_Indefinite_Options;
