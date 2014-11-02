-- parse_args-concrete.adb

-- A simple command line option parser
-- Copyright James Humphry 2014

package body Parse_Args.Concrete is

   ----------------
   -- Set_Option --
   ----------------

   procedure Set_Option(O : in out Option_With_Argument;
                        A : in out Argument_Parser'Class) is

   begin
      if O.Set then
         A.State := Finish_Erroneous;
         A.Message := To_Unbounded_String("Argument cannot be specified twice.");
      else
         A.State := Required_Argument;
      end if;
   end Set_Option;

   ----------------
   -- Set_Option --
   ----------------

   procedure Set_Option(O : in out Concrete_Boolean_Option;
                        A : in out Argument_Parser'Class) is

   begin
      O.Set := True;
      O.Value := not O.Default;
   end Set_Option;

   ----------------
   -- Set_Option --
   ----------------

   procedure Set_Option(O : in out Repeated_Option;
                        A : in out Argument_Parser'Class) is

   begin
      O.Set := True;
      O.Value := O.Value + 1;
   end Set_Option;

   -------------------------
   -- Set_Option_Argument --
   -------------------------

   procedure Set_Option_Argument(O : in out Concrete_Natural_Option;
                                 Arg : in String;
                                 A : in out Argument_Parser'Class) is
   begin
      O.Set := True;
      O.Value := Natural'Value(Arg);
   exception
      when Constraint_Error =>
         A.State := Finish_Erroneous;
         A.Message := To_Unbounded_String("Not a valid natural number: " & Arg);
   end Set_Option_Argument;

   -------------------------
   -- Set_Option_Argument --
   -------------------------

   procedure Set_Option_Argument(O : in out Concrete_Integer_Option;
                                 Arg : in String;
                                 A : in out Argument_Parser'Class) is
   begin
      O.Set := True;
      O.Value := Integer'Value(Arg);
   exception
      when Constraint_Error =>
         A.State := Finish_Erroneous;
         A.Message := To_Unbounded_String("Not a valid integer: " & Arg);
   end Set_Option_Argument;

   -------------------------
   -- Set_Option_Argument --
   -------------------------

   procedure Set_Option_Argument(O : in out Concrete_String_Option;
                                 Arg : in String;
                                 A : in out Argument_Parser'Class) is
   begin
      O.Set := True;
      O.Value := To_Unbounded_String(Arg);
   end Set_Option_Argument;


end Parse_Args.Concrete;
