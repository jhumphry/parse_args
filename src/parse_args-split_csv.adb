-- parse_args-split_csv.adb
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

with Ada.Strings.Fixed;

function Parse_Args.Split_CSV (S : String) return Element_Array_Access is
   use Ada.Strings.Fixed;
   Result : Element_Array_Access;
   U,V : Natural;
begin

   Result := new Element_Array(1..(Count(S, ",") + 1));

   U := S'First;
   for I in Result.all'Range loop
      V := Index (Source => S,
                  Pattern => ",",
                  From => U);
      V := (if V = 0 then S'Last+1 else V);
      Result.all(I) := Value(S(U..(V-1)));
      U := V + 1;
   end loop;

   return Result;

end Parse_Args.Split_CSV;
