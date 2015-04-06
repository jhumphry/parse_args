-- parse_args.ads
-- A simple command line option parser

-- Copyright (c) 2014, James Humphry
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

with Ada.Command_Line;
with Ada.Finalization;
with Ada.Iterator_Interfaces;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;

private with Ada.Strings.Unbounded;
private with Ada.Containers;
private with Ada.Containers.Indefinite_Hashed_Maps, Ada.Strings.Hash;
private with Ada.Containers.Doubly_Linked_Lists;

package Parse_Args is

   package String_Doubly_Linked_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists(Element_Type => String);
   use type String_Doubly_Linked_Lists.List;

   type Integer_Array is array (Integer range <>) of Integer;
   Empty_Integer_Array : constant Integer_Array(1..0) := (1..0 => 0);

   type Argument_Parser is tagged limited private
     with Constant_Indexing => Constant_Reference,
     Default_Iterator => Iterate,
     Iterator_Element => Option;

   procedure Parse_Command_Line(A : in out Argument_Parser);
   function Parse_Success(A : in Argument_Parser) return Boolean;
   function Parse_Message(A : in Argument_Parser) return String;
   function Command_Name(A : in Argument_Parser) return String is
     (Ada.Command_Line.Command_Name);
   procedure Usage(A : in Argument_Parser);

   function Boolean_Value(A : in Argument_Parser; Name : in String) return Boolean;
   function Natural_Value(A : in Argument_Parser; Name : in String) return Natural;
   function Integer_Value(A : in Argument_Parser; Name : in String) return Integer;
   function String_Value(A : in Argument_Parser; Name : in String) return String;
   function Integer_Array_Value(A : in Argument_Parser; Name : in String)
                                return Integer_Array;

   function Tail(A: in Argument_Parser) return String_Doubly_Linked_Lists.List;

   type Option is abstract new Ada.Finalization.Limited_Controlled with
      record
         Set : Boolean := False;
      end record;

   procedure Set_Option(O : in out Option; A : in out Argument_Parser'Class) is abstract;
   procedure Set_Option_Argument(O : in out Option;
                                 Arg : in String;
                                 A : in out Argument_Parser'Class) is null;
   function Image(O : in Option) return String is abstract;

   type Option_Ptr is not null access Option'Class;

   -- The following definitions are to support the indexing, dereferencing and
   -- iteration over the Argument_Parser type

   type Cursor is private;
   function Has_Element(Position : Cursor) return Boolean;
   function Option_Name(Position : Cursor) return String;

   package Argument_Parser_Iterators is new Ada.Iterator_Interfaces(Cursor => Cursor,
                                                                    Has_Element => Has_Element);
   function Iterate (Container : in Argument_Parser)
      return Argument_Parser_Iterators.Forward_Iterator'Class;

   type Option_Constant_Ref(Element : not null access Option'Class) is private
     with Implicit_Dereference => Element;

   function Constant_Reference(C : aliased in Argument_Parser;
                               Name : in String) return Option_Constant_Ref;
   function Constant_Reference(C : aliased in Argument_Parser;
                               Position : in Cursor) return Option_Constant_Ref;

   -- The following definitions actually add options to the parser.

   procedure Add_Option(A : in out Argument_Parser;
                        O : in Option_Ptr;
                        Name : in String;
                        Short_Option : in Character := '-';
                        Long_Option : in String := "";
                        Usage : in String := "";
                        Prepend_Usage : in Boolean := False
                       );

   procedure Append_Positional(A : in out Argument_Parser;
                               O : in Option_Ptr;
                               Name : in String
                              );

   function Make_Boolean_Option(Default : in Boolean := False) return Option_Ptr;
   function Make_Natural_Option(Default : in Natural := 0) return Option_Ptr;
   function Make_Repeated_Option(Default : in Natural := 0) return Option_Ptr;
   function Make_Integer_Option(Default : in Integer := 0) return Option_Ptr;
   function Make_String_Option(Default : in String := "") return Option_Ptr;
   function Make_Integer_Array_Option return Option_Ptr;

   procedure Allow_Tail_Arguments(A : in out Argument_Parser;
                                  Usage : in String := "ARGUMENTS";
                                  Allow : in Boolean := True);

   procedure Set_Prologue(A: in out Argument_Parser;
                          Prologue : in String);

   -- Define interfaces to specify different possible return values

   type Boolean_Option is limited interface;
   function Value(A : in Boolean_Option) return Boolean is abstract;

   type Natural_Option is limited interface;
   function Value(A : in Natural_Option) return Natural is abstract;

   type Integer_Option is limited interface;
   function Value(A : in Integer_Option) return Integer is abstract;

   type String_Option is limited interface;
   function Value(A : in String_Option) return String is abstract;

   type Integer_Array_Option is limited interface;
   function Value(A : in Integer_Array_Option) return Integer_Array is abstract;

private

   use Ada.Strings.Unbounded;

   -- These functions shadow the standard library, but if they are used in a
   -- dispatching way they can be over-ridden in derived types, which is useful
   -- if you want to test the Argument_Parser code but with data fed in from
   -- a test framework rather than the operating system.
   -- Note that Command_Name is exposed directly above.

   function Argument_Count(A : in Argument_Parser) return Natural is
     (Ada.Command_Line.Argument_Count);
   function Argument(A : in Argument_Parser; Number : in Positive) return String is
     (Ada.Command_Line.Argument(Number));

   -- The following instantiations of the standard containers are used as the
   -- basic data structures for storing information on options

   package Option_Maps is new Ada.Containers.Indefinite_Hashed_Maps(Key_Type => String,
                                                                    Element_Type => Option_Ptr,
                                                                    Hash => Ada.Strings.Hash,
                                                                    Equivalent_Keys => "=");

   function Char_Hash(C : in Character) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type(Character'Pos(C)));

   package Option_Char_Maps is new Ada.Containers.Indefinite_Hashed_Maps(Key_Type => Character,
                                                                    Element_Type => Option_Ptr,
                                                                    Hash => Char_Hash,
                                                                    Equivalent_Keys => "=");

   package Positional_Lists is new Ada.Containers.Doubly_Linked_Lists(Element_Type => Option_Ptr);
   use type Positional_Lists.List;
   use type Positional_Lists.Cursor;

   type Option_Help is
      record
         Name : Unbounded_String := Null_Unbounded_String;
         Positional : Boolean := False;
         Long_Option : Unbounded_String := Null_Unbounded_String;
         Short_Option : Character := '-';
         Usage : Unbounded_String := Null_Unbounded_String;
      end record;

   package Option_Help_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists(Element_Type => Option_Help);
   use type Option_Help_Lists.List;
   use type Option_Help_Lists.Cursor;

   -- The core of the Argument_Parser is a finite state machine that starts in
   -- the Init state and should end either in the Finish_Success or Finish_Erroneous
   -- states

   type Argument_Parser_State is (Init,
                                  Ready,
                                  Required_Argument,
                                  Positional_Only,
                                  Finish_Success,
                                  Finish_Erroneous);

   type Argument_Parser is tagged limited record
      State : Argument_Parser_State := Init;
      Last_Option : access Option'Class := null;
      Arguments : Option_Maps.Map := Option_Maps.Empty_Map;
      Long_Options : Option_Maps.Map := Option_Maps.Empty_Map;
      Short_Options : Option_Char_Maps.Map := Option_Char_Maps.Empty_Map;
      Current_Positional : Positional_Lists.Cursor := Positional_Lists.No_Element;
      Positional : Positional_Lists.List := Positional_Lists.Empty_List;
      Allow_Tail : Boolean := False;
      Tail_Usage : Unbounded_String := Null_Unbounded_String;
      Tail : String_Doubly_Linked_Lists.List := String_Doubly_Linked_Lists.Empty_List;
      Message : Unbounded_String := Null_Unbounded_String;
      Prologue : Unbounded_String := Null_Unbounded_String;
      Option_Help_Details : Option_Help_Lists.List := Option_Help_Lists.Empty_List;
   end record;

   type Option_Constant_Ref(Element : not null access Option'Class) is null record;

   type Cursor is new Option_Maps.Cursor;

   type Argument_Parser_Iterator is new Argument_Parser_Iterators.Forward_Iterator with
      record
         Start : Option_Maps.Cursor;
      end record;
   function First (Object : Argument_Parser_Iterator) return Cursor;
   function Next (Object : Argument_Parser_Iterator; Position : Cursor)
                  return Cursor;

end Parse_Args;
