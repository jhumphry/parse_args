# Parse_Args

## Introduction

This is an Ada 2012 package that provides simple command-line argument
parsing. It was inspired by the argparse module in Python, but it is
not as ambitious. The aims are:

1.  to provide a commonly used subset of command-line argument features,
    such as support for long (`--option`) and short (`-o`) options,
    grouped options (in the style of `tar -xvf f.tar`), positional
    arguments, repeated arguments etc.

2.  to be easy to use from the perspective of the application programmer
    without the need to understand the internals of the package.

3.  to use the new features in Ada 2005 and Ada 2012 to their best
    advantage.

## Using the `Parse_Args` package

The core of the package is the `Argument_Parser` type. An object of
this type is created and populated with objects from the `Option` type
hierarchy. The processing of the command line arguments is then
triggered. If successful, the `Argument_Parser` will then contain the
values for the arguments specified by the user (or the specified
default values where appropriate). The values can be iterated over, can
be retrieved by treating the `Argument_Parser` object as a map from
option names to string values, or can be retrieved as the correct types
by type-specific functions.

The built-in option types cover Boolean options, whose presence or
absence on the command line sets the value, repeated options where a
counter is incremented each type the argument is specified, and
arguments of Integer and String types. Various generics are provided so
arguments of any type can be parsed, provided there is an unambiguous
conversion from a string. Options can either be named type or can be
identified by their position on the command line. The 'tail' of
unprocessed arguments can also be retrieved in order, which may, for
example, represent a number of input files that the user wishes to be
processed.

### Example usage

The following demonstrates the basic usage of the Parse_Args package.

```ada
procedure Trivial_Example is
   AP : Argument_Parser;
begin
   AP.Add_Option(Make_Boolean_Option(False), "help", 'h',
                 Usage => "Display this help text");
   AP.Add_Option(Make_Boolean_Option(False), "foo", 'f',
                 Usage => "The foo option");
   AP.Set_Prologue("A demonstration of the Parse_Args library.");

   AP.Parse_Command_Line;

   if AP.Parse_Success then
      if AP.Boolean_Value("help") then
         AP.Usage;
      else
         Put("Option foo is: ");
         Put((if AP.Boolean_Value("foo") then "true" else "false"));
         New_Line;
      end if;
   else
      Put_Line("Error while parsing command-line arguments: ");
      Put_Line(AP.Parse_Message);
   end if;

end Trivial_Example;
```

### `Option` types

The actual options/arguments themselves are in the form of objects
derived from the base `Option` type. These objects are not intended to
be instantiated directly. Instead factory functions are provided which
allocate new objects and return `Option_Ptr` values. These `Option_Ptr`
values are then passed to the `Add_Option` or `Append_Positional`
procedures of `Argument_Parser` as appropriate.

#### Operations of `Option` types

```ada
function Set(O : in Option) return Boolean;
```

This function indicates whether a particular option was set by the user
on the command line. If a value was not set, the default value may be
returned.

```ada
function Image(O : in Option) return String is abstract;
```

This returns a string giving the value of the option. This is a
default representation of the converted value, so is not necessarily
the same as the representation accepted from the user.

#### Basic types of option in `Parse_Args`

```ada
function Make_Boolean_Option(Default : in Boolean := False)
   return Option_Ptr;
function Make_Repeated_Option(Default : in Natural := 0)
   return Option_Ptr;
```

These factories return options that are intended to be specified on the
command line without additional values. The Boolean type can only be
specified by the user once. If not specified the default value will be
returned, and if specified the negation of the default value will be
returned. The Repeated type can be specified more than once and the
return value is an integer giving the number of times it appeared on
the command line.

```ada
function Make_Integer_Option(Default : in Integer := 0;
                             Min : in Integer := Integer'First;
                             Max : in Integer := Integer'Last
                            ) return Option_Ptr;
function Make_Natural_Option(Default : in Natural := 0)
   return Option_Ptr;
function Make_Positive_Option(Default : in Positive := 1)
   return Option_Ptr;
```

The Integer option type takes a default value which is returned if the
option is not specified by the user. It also allows minimum and maximum
valid values to be set. The Natural and Positive factories are a
convenience which have the limits appropriately set for the relevant
sub-type.

```ada
function Make_String_Option(Default : in String := "")
   return Option_Ptr;
```

This factory returns an option that holds a string.

### `Argument_Parser`

The `Argument_Parser` type is responsible for holding references to and
organising the collections of `Option` objects. During the processing
of the command line it manages a state machine and sends each argument
and any associated value to the appropriate `Option` object. After
processing is complete it is equipped with indexing and iteration
interfaces to allow the results to be retrieved.

#### Initialising the object

```ada
procedure Add_Option(A : in out Argument_Parser;
                     O : in Option_Ptr;
                     Name : in String;
                     Short_Option : in Character := '-';
                     Long_Option : in String := "";
                     Usage : in String := "";
                     Prepend_Usage : in Boolean := False
                    );
```

This procedure is used to add a member of `Option'Class`. The `Name` of
the option will be used when retrieving the result. `Short_Option` and
`Long_Option` give the single character short option(for example `-x`)
and the equivalent longer option (for example `--exclude`). If either
is specified as a single character `-` then the option does not have a
short or long form. If `Long_Option` is left as an empty string then
the `Name` will be re-used as the `Long_Option`.

`Usage` is a string which gives a short explanation of what this option
is for. The order in which options are added to the `Argument_Parser`
does not affect the order in which the user can invoke the options on
the command line, but it does affect the order in which the usage text
is produced. If you want to add an option with the `Usage` text
appearing before any previously added options, the flag `Prepend_Usage`
should be set.

```ada
procedure Append_Positional(A : in out Argument_Parser;
                            O : in Option_Ptr;
                            Name : in String
                           );
```

Adding positional arguments is simpler, but only makes sense for
options that take a value (so not `Boolean_Option`, for example). The
`Name` parameter is used to retrieve the result, and to label the
positional argument when producing the usage text. The
`Argument_Parser` package supports the use of `--` on the command line
to mean that all further arguments are considered against the
positional arguments only. This is useful if the user wishes to specify
an argument that happens to start with a `-`.

```ada
procedure Allow_Tail_Arguments(A : in out Argument_Parser;
                               Usage : in String := "ARGUMENTS";
                               Allow : in Boolean := True);
```

`Allow_Tail_Arguments` is used if you wish to allow the user to specify
an unbounded number of arguments after the expected set of arguments.

```ada
procedure Set_Prologue(A: in out Argument_Parser;
                       Prologue : in String);
```

This procedure sets the prologue text that is output before the list of
options when the usage text is displayed.

#### Processing the command line

```ada
procedure Parse_Command_Line(A : in out Argument_Parser);
```

This procedure carries out the processing of the command line arguments.

```ada
function Parse_Success(A : in Argument_Parser) return Boolean;
function Parse_Message(A : in Argument_Parser) return String;
```

Once the command line has been processed, `Parse_Success` will be true
if the parsing was completed without error. If not, `Parse_Message` will
return a string giving more information for display to the user.

#### Retrieving the results

```ada
function Command_Name(A : in Argument_Parser) return String is
   (Ada.Command_Line.Command_Name);
```

This function returns the name by which the program was invoked.

```ada
function Boolean_Value(A : in Argument_Parser; Name : in String)
   return Boolean;
function Integer_Value(A : in Argument_Parser; Name : in String)
   return Integer;
function String_Value(A : in Argument_Parser; Name : in String)
   return String;
```

These functions look up the value of a particular option by name and
return the appropriate type. They will complain if the option under
that name is not of the appropriate type.

```ada
function Tail(A: in Argument_Parser)
   return String_Doubly_Linked_Lists.List;
```

If tail arguments were enabled before processing the command line, this
will return a `Doubly_Linked_List` from the standard `Ada.Containers`,
each element of which is a string giving an unprocessed argument.

#### Printing usage text

```ada
procedure Usage(A : in Argument_Parser);
```

This procedure will produce an explanation of the options and arguments
accepted by the program. Typically this is invoked if a particular
`Boolean_Option` (traditionally `-h` or `--help`)  was specified on the
command line.

## Defining new option types using the generic packages

As well as the basic option types, new option types can be created using
generic child packages of `Parse_Args`. One package supports definite
types and the other indefinite types. There are also helper packages for
the common cases where support for discrete types or indefinite arrays
of discrete types is required.

### `Generic_Options`

```ada
generic
   type Element is private;
   Fallback_Default : Element;
   with function Value (S : String) return Element'Base;
   with function Image (Arg : Element'Base) return String;
   with procedure Valid (Arg : in Element; Result : in out Boolean)
      is null;
package Parse_Args.Generic_Options
```

This package supports definite types supplied as the `Element` type
parameter. The `Fallback_Default` value is used as the default for
options if a more specific value is not provided. The `Value` and
`Image` functions are used to convert values to/from string
representations.

The `Valid` procedure should set the `Result` parameter based on
whether the `Arg` parameter is a valid example of the `Element` type -
this can be used to create options that have more stringent
requirements than the underlying `Element` data type. This might be
more practicable than creating many subtypes of `Element` with different
`Dynamic_Predicate` aspects, or creating several versions of `Value`
that only differ in validity checks. Alternatively, it can be left
`null` if not required.

When instantiated, the resulting package contains a new private type
`Element_Option` which is part of `Option'Class` and has the usual
primative operations.

```ada
function Value (O : in Element_Option) return Element;
```

The `Value` function will extract return a value of the correct type, as
expected.

```ada
function Value(A : in Argument_Parser; Name : in String)
   return Element;
function Make_Option(Default : in Element := Fallback_Default)
                     return Option_Ptr;
```

These functions complement ther equivalents in `Parse_Args` for the new
option type.

### `Generic_Discrete_Options`

```ada
generic
   type Element is (<>);
   Fallback_Default : Element;
   with procedure Valid (Arg : in Element; Result : in out Boolean)
      is null;
package Parse_Args.Generic_Discrete_Options
```

The `Generic_Discrete_Options` package wraps the `Generic_Options`
package. As discrete types always have `'Image` and `'Value` attributes,
these do not have to be supplied.

### `Generic_Indefinite_Options`

```ada
generic
   type Element(<>);
   type Element_Access is access Element;
   with function Value (S : String) return Element_Access;
   with function Image (Arg : Element_Access) return String;
   with procedure Free_Element(X : in out Element_Access) is null;
   with procedure Valid (Arg : in Element_Access;
                         Result : in out Boolean) is null;
package Parse_Args.Generic_Indefinite_Options
```

The generic package for supporting options with indefinite types is very
similar. The main difference is that rather than returning `Element`
values, and access value will be returned. If you want to avoid memory
leaks, it is also necessary to provide a `Free_Element` procedure (which
will probably be an instance of `Ada.Unchecked_Deallocation`). There is
also no `Fallback_Default` value - if no value is set on the command
line a `null` pointer will be returned.

```ada
function Value (O : in Element_Option) return Element_Access;
function Value(A : in Argument_Parser; Name : in String)
   return Element_Access;
function Make_Option return Option_Ptr;
```

These functions operate like their definite counterparts, except for
the use of access values rather than direct values. The memory
allocated to hold values retrieved from the command line is consider to
belong to the `Argument_Parser` object, and *when the parser is
destroyed all storage associated with indefinite command line arguments
will be deallocated*. If you wish to destroy the `Argument_Parser`
object before the termination of the program, you must copy any
indefinite values which are still required to new storage allocations.

### `Generic_Discrete_Array_Options` and `Integer_Array_Options`

```ada
generic
   type Element is (<>);
   type Element_Array is array (Integer range <>) of Element;
   type Element_Array_Access is access Element_Array;
   with procedure Valid (Arg : in Element_Array_Access;
                         Result : in out Boolean
                        ) is null;
package Parse_Args.Generic_Discrete_Array_Options
```

This package is a wrapper aroung `Generic_Discrete_Array_Options` that
reduces the number of boilerplate declarations needed in the common
case that the indefinite type is an array of discrete types, which
will be provided on the command line as a single comma-separated list
of values.

```ada
generic
   type Element is private;
   type Element_Array is array(Integer range <>) of Element;
   type Element_Array_Access is access Element_Array;
   with function Value (S : String) return Element'Base;
function Parse_Args.Split_CSV (S : String)
   return Element_Array_Access;
```

The code for creating arrays from comma-separated lists may be
useful so has also be exposed as a generic unit.

`Integer_Array_Options` is simply a demonstration of
`Generic_Discrete_Array_Options` for the case of arrays of the built-in
`Integer` type.

## Examples and unit tests

Two example programs are provided. `simple_example.adb` shows the usage
of the inbuilt option types.

`generic_example.adb` shows more sophisticated usage with custom option
types. The option types are defined in `generic_example_options.ads`,
showing both discrete, floating-point and indefinite (array) option
types.

Some unit tests built using the AUnit framework are compiled into
`parse_args_tests`.

## Acknowledgements

Thanks to Anton Fediushin for packaging help and the Python argparse creators
for inspiration.

## Copyright license

The package is provided under the ISC license:

> Copyright (c) 2014 - 2022, James Humphry
>
> Permission to use, copy, modify, and/or distribute this software for any
> purpose with or without fee is hereby granted, provided that the above
> copyright notice and this permission notice appear in all copies.
>
> THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
> REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
> AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
> INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
> LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
> OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
> PERFORMANCE OF THIS SOFTWARE.
