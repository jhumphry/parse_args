-- parse_args-concrete.ads

-- A simple command line option parser
-- Copyright James Humphry 2014

-- This private child package contains some concrete implementations of
-- different options. It is not expected that package users will create these
-- types directly. Instead the factory functions in the parent package are used,
-- as these ensure that the options will be indexed and referenced correctly.

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


   type Concrete_Natural_Option is new Option_With_Argument and Natural_Option with record
      Value : Natural := 0;
      Default : Natural := 0;
   end record;
   procedure Set_Option_Argument(O : in out Concrete_Natural_Option;
                                 Arg : in String;
                                 A : in out Argument_Parser'Class);
   function Image(O : in  Concrete_Natural_Option) return String is (Natural'Image(O.Value));
   function Value(O : in Concrete_Natural_Option) return Natural is (O.Value);

   type Repeated_Option is new Concrete_Natural_Option with null record;
   procedure Set_Option(O : in out Repeated_Option; A : in out Argument_Parser'Class);

   type Concrete_Integer_Option is new Option_With_Argument and Integer_Option with record
      Value : Integer := 0;
      Default : Integer := 0;
   end record;
   procedure Set_Option_Argument(O : in out Concrete_Integer_Option;
                                 Arg : in String;
                                 A : in out Argument_Parser'Class);
   function Image(O : in  Concrete_Integer_Option) return String is (Integer'Image(O.Value));
   function Value(O : in Concrete_Integer_Option) return Integer is (O.Value);


   type Concrete_String_Option is new Option_With_Argument and String_Option with record
      Value : Unbounded_String := Null_Unbounded_String;
      Default : Unbounded_String := Null_Unbounded_String;
   end record;
   procedure Set_Option_Argument(O : in out Concrete_String_Option;
                                 Arg : in String;
                                 A : in out Argument_Parser'Class);
   function Value(O : in Concrete_String_Option) return String is (To_String(O.Value));
   function Image(O : in Concrete_String_Option) return String renames Value;

end Parse_Args.Concrete;
