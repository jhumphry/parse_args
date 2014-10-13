-- parse_args.ads

-- A simple command line option parser
-- Copyright James Humphry 2014

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Ada.Containers.Indefinite_Hashed_Maps, Ada.Strings.Hash;

package Parse_Args is

   type Argument_Parser is tagged limited private
     with Constant_Indexing => Constant_Reference;

   procedure Add_Boolean_Option(A : in out Argument_Parser;
                               Name : in String;
                               Short_Option : in Character := '-';
                               Default : in Boolean := False;
                               Long_Option : in String := "");

   procedure Parse_Command_Line(A : in out Argument_Parser);
   function Command_Name(A : Argument_Parser) return String;

   type Argument is limited interface;
   function Is_Default(A : Argument) return Boolean is abstract;

   type Argument_Ptr is not null access Argument'Class;

   --type Constant_Argument_Ref(E: not null access constant Argument'Class) is private
   --  with Implicit_Dereference => E;
   function Constant_Reference(C : aliased in Argument_Parser;
                               Name : String) return Argument_Ptr;


   type Boolean_Argument is interface and Argument;
   function Value(A : Boolean_Argument) return Boolean is abstract;

private

   package Argument_Maps is new Ada.Containers.Indefinite_Hashed_Maps(Key_Type => String,
                                                           Element_Type => Argument_Ptr,
                                                           Hash => Ada.Strings.Hash,
                                                                      Equivalent_Keys => "=");

   type Argument_Parser is tagged limited record
      Parsed : Boolean := False;
      Command_Name : Unbounded_String;
      Arguments : Argument_Maps.Map;
      Long_Options : Argument_Maps.Map;
      Short_Options : Argument_Maps.Map;
   end record;

   --type Constant_Argument_Ref(E: not null access constant Argument'Class) is null record;

   type Concrete_Boolean_Option is new Boolean_Argument with record
      Value : Boolean;
      Default : Boolean;
   end record;
   function Is_Default(A : Concrete_Boolean_Option) return Boolean;
   function Value(A : Concrete_Boolean_Option) return Boolean;

end Parse_Args;
