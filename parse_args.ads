-- parse_args.ads

-- A simple command line option parser
-- Copyright James Humphry 2014

with Ada.Command_Line;
with Ada.Iterator_Interfaces;

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps, Ada.Strings.Hash;
with Ada.Containers.Doubly_Linked_Lists;

package Parse_Args is

   type Argument_Parser is tagged limited private
     with Constant_Indexing => Constant_Reference,
     Default_Iterator => Iterate,
     Iterator_Element => Option;

   procedure Parse_Command_Line(A : in out Argument_Parser);
   function Parse_Success(A : in Argument_Parser) return Boolean;
   function Parse_Message(A : in Argument_Parser) return String;
   function Command_Name(A : in Argument_Parser) return String is
     (Ada.Command_Line.Command_Name);

   function Boolean_Value(A : in Argument_Parser; Name : in String) return Boolean;
   function Natural_Value(A : in Argument_Parser; Name : in String) return Natural;
   function Integer_Value(A : in Argument_Parser; Name : in String) return Integer;
   function String_Value(A : in Argument_Parser; Name : in String) return String;

   type Option is abstract tagged limited record
      Set : Boolean := False;
   end record;

   procedure Set_Option(O : in out Option; A : in out Argument_Parser'Class) is abstract;
   procedure Set_Option_Argument(O : in out Option;
                                 Arg : in String;
                                 A : in out Argument_Parser'Class) is null;

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
                        Long_Option : in String := ""
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

   -- Define interfaces to specify different possible return values

   type Boolean_Option is limited interface;
   function Value(A : in Boolean_Option) return Boolean is abstract;

   type Natural_Option is limited interface;
   function Value(A : in Natural_Option) return Natural is abstract;

   type Integer_Option is limited interface;
   function Value(A : in Integer_Option) return Integer is abstract;

   type String_Option is limited interface;
   function Value(A : in String_Option) return String is abstract;

private

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
      Last_Option : access Option'Class;
      Arguments : Option_Maps.Map;
      Long_Options : Option_Maps.Map;
      Short_Options : Option_Char_Maps.Map;
      Current_Positional : Positional_Lists.Cursor := Positional_Lists.No_Element;
      Positional : Positional_Lists.List;
      Message : Unbounded_String;
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

   -- Concrete options are not exposed to ensure that any options are not added
   -- to Argument_Parser inconsistently.

   type Option_With_Argument is abstract new Option with null record;
   procedure Set_Option(O : in out Option_With_Argument; A : in out Argument_Parser'Class);

   type Concrete_Boolean_Option is new Option and Boolean_Option with record
      Value : Boolean := False;
      Default : Boolean := False;
   end record;
   procedure Set_Option(O : in out Concrete_Boolean_Option; A : in out Argument_Parser'Class);
   function Value(A : in Concrete_Boolean_Option) return Boolean is (A.Value);


   type Concrete_Natural_Option is new Option_With_Argument and Natural_Option with record
      Value : Natural := 0;
      Default : Natural := 0;
   end record;
   procedure Set_Option_Argument(O : in out Concrete_Natural_Option;
                                 Arg : in String;
                                 A : in out Argument_Parser'Class);
   function Value(A : in Concrete_Natural_Option) return Natural is (A.Value);

   type Repeated_Option is new Concrete_Natural_Option with null record;
   procedure Set_Option(O : in out Repeated_Option; A : in out Argument_Parser'Class);

   type Concrete_Integer_Option is new Option_With_Argument and Integer_Option with record
      Value : Integer := 0;
      Default : Integer := 0;
   end record;
   procedure Set_Option_Argument(O : in out Concrete_Integer_Option;
                                 Arg : in String;
                                 A : in out Argument_Parser'Class);
   function Value(A : in Concrete_Integer_Option) return Integer is (A.Value);


   type Concrete_String_Option is new Option_With_Argument and String_Option with record
      Value : Unbounded_String := Null_Unbounded_String;
      Default : Unbounded_String := Null_Unbounded_String;
   end record;
   procedure Set_Option_Argument(O : in out Concrete_String_Option;
                                 Arg : in String;
                                 A : in out Argument_Parser'Class);
   function Value(A : in Concrete_String_Option) return String is (To_String(A.Value));

end Parse_Args;
