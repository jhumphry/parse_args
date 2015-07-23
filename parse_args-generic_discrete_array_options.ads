-- parse_args-generic_discrete_array_options.ads
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

with Ada.Unchecked_Deallocation;

with Parse_Args.Generic_Indefinite_Options;
with Parse_Args.Split_CSV;

generic
   type Element is (<>);
   type Element_Array is array (Integer range <>) of Element;
   type Element_Array_Access is access Element_Array;
   with procedure Valid (Arg : in Element_Array_Access; Result : in out Boolean) is null;
package Parse_Args.Generic_Discrete_Array_Options is

   procedure Free_Element_Array is new Ada.Unchecked_Deallocation(Object => Element_Array,
                                                                  Name => Element_Array_Access);

   function Split_CSV_Element_Array is new Split_CSV(Element => Element,
                                                     Element_Array => Element_Array,
                                                     Element_Array_Access => Element_Array_Access,
                                                     Value => Element'Value);

   function Element_Array_Image(Arg : Element_Array_Access) return String;

   package Inner is new Parse_Args.Generic_Indefinite_Options(Element => Element_Array,
                                                              Element_Access => Element_Array_Access,
                                                              Value => Split_CSV_Element_Array,
                                                              Image => Element_Array_Image,
                                                              Free_Element => Free_Element_Array,
                                                              Valid => Valid
                                                             );

   subtype Element_Array_Option is Inner.Element_Option;
   function Image (O : in Element_Array_Option) return String renames Inner.Image;
   function Value (O : in Element_Array_Option) return Element_Array_Access renames Inner.Value;
   function Value(A : in Argument_Parser; Name : in String) return Element_Array_Access renames Inner.Value;
   function Make_Option return Option_Ptr renames Inner.Make_Option;

end Parse_Args.Generic_Discrete_Array_Options;
