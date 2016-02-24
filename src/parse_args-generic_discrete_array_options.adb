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

package body Parse_Args.Generic_Discrete_Array_Options is

   -------------------------
   -- Element_Array_Image --
   -------------------------

   function Element_Array_Image (Arg : Element_Array_Access) return String is
   begin
      if Arg /= null then
         return "<Array of length: " & Integer'Image(Arg'Length) & ">";
      else
         return "<Empty array>";
      end if;
   end Element_Array_Image;

end Parse_Args.Generic_Discrete_Array_Options;
