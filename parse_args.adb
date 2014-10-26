-- parse_args.adb

-- A simple command line option parser
-- Copyright James Humphry 2014

with Ada.Command_Line;

package body Parse_Args is

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line (A : in out Argument_Parser) is

      -- Separating these out into expression functions keeps the mechanics
      -- of the state machine below a little clean.

      function is_short_option(A : String) return Boolean is
        (A'Length = 2 and then A(A'First) = '-' and then A(A'First+1) /= '-');

      function short_option(A : String) return Character is (A(A'Last));

      function is_short_option_group(A : String) return Boolean is
        (A'Length > 2 and then
         A(A'First) = '-' and then (for all I in A'First+1..A'Last => A(I) /= '-'));

      function short_option_group(A : String) return String is (A(A'First+1..A'Last));

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
      A.Last_Option := null;
      -- Last_Option holds a pointer to the option seen in the previous loop
      -- iteration so we know where to direct any arguments.

      for I in 1..Ada.Command_Line.Argument_Count loop
         declare
            Arg : String := Ada.Command_Line.Argument(I);
         begin
            case A.State is
               when Init | Finish_Success =>
                  raise Program_Error with "Argument Parser in impossible state!";

               when Ready =>
                  if is_options_end(Arg) then
                     A.State := Positional_Only;

                  elsif is_long_option(Arg)
                    and then A.Long_Options.Contains(long_option(Arg)) then
                     Set_Option(A.Long_Options.Element(long_option(Arg)).all, A);
                     A.Last_Option := A.Long_Options.Element(long_option(Arg));

                  elsif is_short_option(Arg)
                    and then A.Short_Options.Contains(short_option(Arg)) then
                     Set_Option(A.Short_Options.Element(short_option(Arg)).all, A);
                     A.Last_Option := A.Short_Options.Element(short_option(Arg));

                  elsif is_short_option_group(Arg) then
                     for C of short_option_group(Arg) loop
                        if A.State /= Ready then
                           -- If one of the options specified previously in the
                           -- option group takes an argument it will have left
                           -- the parser state as Required_Argument. The only
                           -- time this isn't a problem is if such an option is
                           -- the last one in the group.
                           A.Message := To_Unbounded_String("Option requiring an argument must be last in the group: " &
                                                              short_option_group(Arg));
                           A.State := Finish_Erroneous;
                           exit;

                        elsif A.Short_Options.Contains(C) then
                           Set_Option(A.Short_Options.Element(C).all, A);
                           A.Last_Option := A.Short_Options.Element(C);

                        else
                           A.Message := To_Unbounded_String("Unrecognised argument: " & C);
                           A.State := Finish_Erroneous;
                           exit;

                        end if;
                     end loop;

                  else
                     A.Message := To_Unbounded_String("Unrecognised argument: " & Arg);
                     A.State := Finish_Erroneous;

                  end if;

               when Required_Argument =>
                  -- The parser can never get into the Required_Argument state
                  -- without going through the Ready state above, and all the
                  -- branches set Last_Option to a valid, non-null option.
                  Set_Option_Argument(A.Last_Option.all, Arg, A);

               when Finish_Erroneous =>
                  -- When an problem is encountered, skip all subsequent arguments
                  -- Hopefully A.message has also been filled out with an informative
                  -- message.
                  null;

               when others =>
                  raise Program_Error with "Not implemented yet...";

            end case;
         end;
      end loop;

      case A.State is
         when Init | Required_Argument | Finish_Success | Finish_Erroneous =>
            A.State := Finish_Erroneous;
         when Ready | Positional_Only =>
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

   -------------------
   -- Natural_Value --
   -------------------

   function Natural_Value(A : Argument_Parser; Name : String) return Natural is
   begin
      if A.Arguments.Contains(Name) and then A.Arguments(Name).all in Natural_Option'Class then
         return Natural_Option'Class(A.Arguments(Name).all).Value;
      else
         raise Constraint_Error with "No suitable argument: " & Name & " with natural result.";
      end if;
   end Natural_Value;

   -------------------
   -- String_Value --
   -------------------

   function String_Value(A : Argument_Parser; Name : String) return String is
   begin
      if A.Arguments.Contains(Name) and then A.Arguments(Name).all in String_Option'Class then
         return String_Option'Class(A.Arguments(Name).all).Value;
      else
         raise Constraint_Error with "No suitable argument: " & Name & " with string result.";
      end if;
   end String_Value;

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
      New_Opt : Option_Ptr := new Concrete_Boolean_Option'(Set => False,
                                                           Value => Default,
                                                           Default => Default
                                                          );
   begin
      Add_Option(A, Name, Short_Option, Long_Option, New_Opt);
   end Add_Boolean_Option;

   ------------------------
   -- Add_Repeated_Option --
   ------------------------

   procedure Add_Repeated_Option
     (A : in out Argument_Parser;
      Name : in String;
      Short_Option : in Character := '-';
      Default : in Natural := 0;
      Long_Option : in String := "")
   is
      New_Opt : Option_Ptr := new Repeated_Option'(Set => False,
                                                   Value => Default,
                                                   Default => Default
                                                  );
   begin
      Add_Option(A, Name, Short_Option, Long_Option, New_Opt);
   end Add_Repeated_Option;

   ------------------------
   -- Add_Natural_Option --
   ------------------------

   procedure Add_Natural_Option
     (A : in out Argument_Parser;
      Name : in String;
      Short_Option : in Character := '-';
      Default : in Natural := 0;
      Long_Option : in String := "")
   is
      New_Opt : Option_Ptr := new Concrete_Natural_Option'(Set => False,
                                                           Value => Default,
                                                           Default => Default
                                                          );
   begin
      Add_Option(A, Name, Short_Option, Long_Option, New_Opt);
   end Add_Natural_Option;

   -----------------------
   -- Add_String_Option --
   -----------------------

   procedure Add_String_Option
     (A : in out Argument_Parser;
      Name : in String;
      Short_Option : in Character := '-';
      Default : in String := "";
      Long_Option : in String := "")
   is
      Default_US : Unbounded_String := To_Unbounded_String(Default);
      New_Opt : Option_Ptr := new Concrete_String_Option'(Set => False,
                                                           Value => Default_US,
                                                           Default => Default_US
                                                          );
   begin
      Add_Option(A, Name, Short_Option, Long_Option, New_Opt);
   end Add_String_Option;

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
         O.Set := True;
         A.State := Required_Argument;
      end if;
   end Set_Option;

   ----------------
   -- Add_Option --
   ----------------

   procedure Add_Option(A : in out Argument_Parser;
                        Name : in String;
                        Short_Option : in Character := '-';
                        Long_Option : in String := "";
                        O : in Option_Ptr) is
   begin
      A.Arguments.Insert(Name, O);
      if Short_Option /= '-' then
         A.Short_Options.Insert(Short_Option, O);
      end if;
      if Long_Option'Length > 0  then
         A.Long_Options.Insert(Long_Option, O);
      else
         A.Long_Options.Insert(Name, O);
      end if;
   end Add_Option;

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
      O.Value := Natural'Value(Arg);
      A.State := Ready;
   exception
      when Constraint_Error =>
         A.State := Finish_Erroneous;
         A.Message := To_Unbounded_String("Not a valid natural number: " & Arg);
   end Set_Option_Argument;

   -------------------------
   -- Set_Option_Argument --
   -------------------------

   procedure Set_Option_Argument(O : in out Concrete_String_Option;
                                 Arg : in String;
                                 A : in out Argument_Parser'Class) is
   begin
      O.Value := To_Unbounded_String(Arg);
      A.State := Ready;
   end Set_Option_Argument;

end Parse_Args;
