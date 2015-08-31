-- parse_args-integer_array_options.ads
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

with Parse_Args.Generic_Discrete_Array_Options;

package Parse_Args.Integer_Array_Options is

   type Integer_Array is array (Integer range <>) of Integer;
   type Integer_Array_Access is access Integer_Array;

   package Inner is new Generic_Discrete_Array_Options(Element => Integer,
                                                       Element_Array => Integer_Array,
                                                       Element_Array_Access => Integer_Array_Access
                                                      );

   subtype Element_Array_Option is Inner.Element_Array_Option;
   function Image (O : in Element_Array_Option) return String renames Inner.Image;
   function Value (O : in Element_Array_Option) return Integer_Array_Access renames Inner.Value;
   function Value(A : in Argument_Parser; Name : in String) return Integer_Array_Access renames Inner.Value;
   function Make_Option return Option_Ptr renames Inner.Make_Option;

end Parse_Args.Integer_Array_Options;
