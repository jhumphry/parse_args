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
   function Parse_Success(A : in Argument_Parser) return Boolean;
   function Parse_Message(A : in Argument_Parser) return String;
   function Command_Name(A : Argument_Parser) return String;
   function Boolean_Value(A : Argument_Parser; Name : String) return Boolean;

   type Option(Min_Args : Natural; Max_Args : Natural) is abstract tagged limited record
      Set : Boolean := False;
      Actual_Args : Natural := 0;
      end record;

   type Option_Ptr is not null access Option'Class;

   function Constant_Reference(C : aliased in Argument_Parser;
                               Name : String) return Option_Ptr;


   type Boolean_Option is abstract new Option(Min_Args => 0, Max_Args =>0) with null record;
   function Value(A : Boolean_Option) return Boolean is abstract;

private

   package Option_Maps is new Ada.Containers.Indefinite_Hashed_Maps(Key_Type => String,
                                                           Element_Type => Option_Ptr,
                                                           Hash => Ada.Strings.Hash,
                                                           Equivalent_Keys => "=");

   type Argument_Parser_State is (Init,
                                  Ready,
                                  Required_Argument,
                                  Possible_Argument,
                                  Positional_Only,
                                  Finish_Success,
                                  Finish_Erroneous);

   type Argument_Parser is tagged limited record
      State : Argument_Parser_State := Init;
      Current_Option : access Option'Class;
      Command_Name : Unbounded_String;
      Arguments : Option_Maps.Map;
      Long_Options : Option_Maps.Map;
      Short_Options : Option_Maps.Map;
      Message : Unbounded_String;
   end record;

   type Concrete_Boolean_Option is new Boolean_Option with record
      Value : Boolean;
   end record;
   function Value(A : Concrete_Boolean_Option) return Boolean;

end Parse_Args;
