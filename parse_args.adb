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
      New_Arg : Option_Ptr := new Concrete_Boolean_Option'(Min_Args => 0,
                                                           Max_Args => 0,
                                                           Set => False,
                                                           Actual_Args => 0,
                                                           Value => Default
                                                          );
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

      -- Currently no support for grouped short options such as ls -au
      function is_short_option(A : String) return Boolean is
        (A'Length = 2 and then A(A'First) = '-' and then A(A'First+1) /= '-');

      function short_option(A : String) return String is ("" & A(A'Last));

      function is_long_option(A : String) return Boolean is
        (A'Length > 2 and then A(A'First..A'First+1) = "--");

      function long_option(A : String) return String is
         (A(A'First+2..A'Last));

      function is_options_end(A : String) return Boolean is
         (A = "--");

   begin
      if A.State /= Init then
         raise Program_Error with "Argument Parser has already been fed data!";
      end if;
      A.State := Ready;

      A.Command_Name := To_Unbounded_String(Ada.Command_Line.Command_Name);

      for I in 1..Ada.Command_Line.Argument_Count loop
         declare
            Arg : String := Ada.Command_Line.Argument(I);
            Opt : access Option'Class;
         begin
            case A.State is
               when Init | Finish_Success =>
                  raise Program_Error with "Argument Parser in impossible state!";

               when Ready =>
                  if is_options_end(Arg) then
                     A.State := Positional_Only;

                  elsif is_long_option(Arg)
                    and then A.Long_Options.Contains(long_option(Arg)) then
                        Opt := A.Long_Options.Element(long_option(Arg));
                        Opt.Set := True;
                        if Opt.all in Concrete_Boolean_Option'Class then
                           Concrete_Boolean_Option(Opt.all).Value := True;
                        end if;

                  elsif is_short_option(Arg)
                    and then A.Short_Options.Contains(short_option(Arg)) then
                        Opt := A.Short_Options.Element(short_option(Arg));
                        Opt.Set := True;
                        if Opt.all in Concrete_Boolean_Option'Class then
                           Concrete_Boolean_Option(Opt.all).Value := True;
                        end if;

                  else
                     A.Message := To_Unbounded_String("Unrecognised argument: " & Arg);
                     A.State := Finish_Erroneous;
                  end if;

               when Finish_Erroneous =>
                  null; -- When an problem is encountered, skip all subsequent arguments

               when others =>
                  raise Program_Error with "Not implemented yet...";

            end case;
         end;
      end loop;

      case A.State is
         when Init | Required_Argument | Finish_Success | Finish_Erroneous =>
            A.State := Finish_Erroneous;
         when Ready | Possible_Argument | Positional_Only =>
            A.State := Finish_Success;
      end case;

   end Parse_Command_Line;

   -------------------
   -- Parse_Success --
   -------------------

   function Parse_Success(A : in Argument_Parser) return Boolean is
   begin
      if A.State = Finish_Success then
         return True;
      else
         return False;
      end if;
   end Parse_Success;

   -------------------
   -- Parse_Message --
   -------------------

   function Parse_Message(A : in Argument_Parser) return String is
   begin
      case A.State is
         when Finish_Success | Finish_Erroneous =>
            return To_String(A.Message);
         when others =>
            raise Program_Error with "No parse yet taken place?";
      end case;
   end Parse_Message;

   ------------------
   -- Command_Name --
   ------------------

   function Command_Name (A : Argument_Parser) return String is
   begin
      return To_String(A.Command_Name);
   end Command_Name;

   -------------------
   -- Boolean_Value --
   -------------------

   function Boolean_Value(A : Argument_Parser; Name : String) return Boolean is
   begin
      if A.Arguments.Contains(Name) and then A.Arguments(Name).all in Boolean_Option'Class then
         return Boolean_Option'Class(A.Arguments(Name).all).Value;
      else
         raise Constraint_Error with "No suitable argument: " & Name & " with boolean result.";
      end if;
   end Boolean_Value;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (C : aliased in Argument_Parser;
      Name : String)
      return Option_Ptr
   is
   begin
      return C.Arguments(Name);
   end Constant_Reference;

   -----------
   -- Value --
   -----------

   function Value (A : Concrete_Boolean_Option) return Boolean is
   begin
      return A.Value;
   end Value;

end Parse_Args;
