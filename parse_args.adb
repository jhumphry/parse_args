-- parse_args.adb

-- A simple command line option parser
-- Copyright James Humphry 2014

with Ada.Command_Line;

package body Parse_Args is

   ------------------------
   -- Add_Boolean_Option --
   ------------------------

   procedure Add_Boolean_Option
     (A : in out Argument_Parser;
      Name : in String;
      Short_Option : in Character := '-';
      Default : in Boolean := False;
      Long_Option : in String := "")
   is
      New_Arg : Argument_Ptr := new Concrete_Boolean_Option'(Value => Default,
                                                        Default => Default);
   begin
      A.Arguments.Insert(Name, New_Arg);
      if Short_Option /= '-' then
         A.Short_Options.Insert("" & Short_Option, New_Arg);
      end if;
      if Long_Option'Length > 0  then
         A.Long_Options.Insert(Long_Option, New_Arg);
      else
         A.Long_Options.Insert(Name, New_Arg);
      end if;
   end Add_Boolean_Option;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line (A : in out Argument_Parser) is
   begin
      A.Command_Name := To_Unbounded_String(Ada.Command_Line.Command_Name);
      for I in 1..Ada.Command_Line.Argument_Count loop
         declare
            Arg : String := Ada.Command_Line.Argument(I);
         begin
            if Arg'Length > 2 and then Arg(1..2) = "--" then
               if A.Long_Options.Contains(Arg(3..Arg'Last)) then
                  Concrete_Boolean_Option(A.Long_Options.Element(Arg(3..Arg'Last)).all).Value := True;
               end if;
            end if;
         end;
      end loop;
   end Parse_Command_Line;

   ------------------
   -- Command_Name --
   ------------------

   function Command_Name (A : Argument_Parser) return String is
   begin
      return To_String(A.Command_Name);
   end Command_Name;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (C : aliased in Argument_Parser;
      Name : String)
      return Argument_Ptr
   is
   begin
      return C.Arguments(Name);
   end Constant_Reference;

   ----------------
   -- Is_Default --
   ----------------

   function Is_Default (A : Concrete_Boolean_Option) return Boolean is
   begin
      return A.Default;
   end Is_Default;

   -----------
   -- Value --
   -----------

   function Value (A : Concrete_Boolean_Option) return Boolean is
   begin
      return A.Value;
   end Value;

end Parse_Args;
