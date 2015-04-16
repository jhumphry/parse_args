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

## Copyright

The package is provided under the permissive ISC-style license:

> Copyright (c) 2014, James Humphry
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

## Using the `Parse_Args` package

The core of the package is the `Argument_Parser` type. An object of
this type is created and populated with objects from the `Option` type
hierarchy. The processing of the command line arguments is then
triggered. If successful, the `Argument_Parser` will then contain the
actual arguments passed (or default values where appropriate). These
values can be iterated over, can be retrieved by treating the
`Argument_Parser` object as a map from option names to string values,
or can be retrieved as the correct types by specific functions.

The built-in option types cover Boolean options, whose presence or
absence on the command line sets the value, repeated options where a
counter is incremented each type the argument is specified, and
arguments of Integer and String types. Options can either be of the
named type or can be positional. The remaining unprocessed 'tail' of
specified arguments can be retrieved, which is useful in the common
case that a command can be given a unlimited number of files to operate
over.

### `Argument_Parser`

The `Argument_Parser` type is responsible for holding references to and
organising the collections of `Option` objects. During the processing
of the command line it manages a state machine and sends each argument
to the appropriate `Option` object. After processing it is equipped
with indexing and iteration interfaces to allow the results to be
retrieved.

#### Initialising the object

    procedure Add_Option(A : in out Argument_Parser;
                         O : in Option_Ptr;
                         Name : in String;
                         Short_Option : in Character := '-';
                         Long_Option : in String := "";
                         Usage : in String := "";
                         Prepend_Usage : in Boolean := False
                        );

This procedure is used to add a member of `Option'Class`. The `Name` of
the option will be used when retrieving the result. `Short_Option` and
`Long_Option` give the single character short option(for example `-x`)
and the equivalent longer option (for example `--exclude`). If either
is specified as a single character `-` then the option does not have a
short or long form. If `Long_Option` is left as an empty string then
the `Name` will be re-used as the `Long_Option`.

`Usage` is a string which gives a short explanation of what this option
is for. The order of adding options to the `Argument_Parser` does not
affect the order in which the user can invoke the options on the
command line, but it does affect the order in which the usage text is
produced. If you want to add an option with the `Usage` text appearing
before any previously added options, the flag `Prepend_Usage` should be
set.

    procedure Append_Positional(A : in out Argument_Parser;
                                O : in Option_Ptr;
                                Name : in String
                               );

Adding positional arguments is simpler. The `Name` parameter is used to
retrieve the result, and to label the positional argument when
producing the usage text. The `Argument_Parser` package supports the
usage of `--` on the command line which means that all further arguments
are considered against the positional arguments only. This is useful if
the user wishes to specify an argument that happens to start with a `-`.

    procedure Allow_Tail_Arguments(A : in out Argument_Parser;
                                   Usage : in String := "ARGUMENTS";
                                   Allow : in Boolean := True);

`Allow_Tail_Arguments` is used if you wish to allow the user to specify
an unbounded number of arguments after the expected set of arguments.

    procedure Set_Prologue(A: in out Argument_Parser;
                           Prologue : in String);

This procedure sets the prologue text that is output before the list of
options when the usage text is displayed.

#### Processing the command line

    procedure Parse_Command_Line(A : in out Argument_Parser);

This procedure carries out the processing of the command line arguments.

    function Parse_Success(A : in Argument_Parser) return Boolean;
    function Parse_Message(A : in Argument_Parser) return String;

Once the command line has been processed, `Parse_Success` will be true
if the parsing was completed without error. If not, `Parse_Message` will
return a string giving more information for display to the user.

#### Retrieving the results

    function Command_Name(A : in Argument_Parser) return String is
       (Ada.Command_Line.Command_Name);

This function returns the name by which the program was invoked.

    function Boolean_Value(A : in Argument_Parser; Name : in String)
       return Boolean;
    function Integer_Value(A : in Argument_Parser; Name : in String)
       return Integer;
    function String_Value(A : in Argument_Parser; Name : in String)
       return String;

These functions look up the value of a particular option by name and
return the appropriate type. They will complain if the option under
that name is not of the appropriate type.

    function Tail(A: in Argument_Parser)
       return String_Doubly_Linked_Lists.List;

If tail arguments were enabled before processing the command line, this
will return a Doubly_Linked_List from the standard Ada.Containers, each
element of which is a string giving an unprocessed argument.

#### Printing usage text

    procedure Usage(A : in Argument_Parser);

This procedure will produce an explanation of the options and arguments
accepted by the program. Typically this is invoked if a particular
Boolean_Option (traditionally `-h` or `--help`)  was specified on the
command line.

### `Option` types

The actual options/arguments themselves are in the form of objects
derived from the base `Option` type. These objects are not intended to
be instantiated directly. Instead factory functions will be provided to
allocate new objects and return `Option_Ptr` values which are passed to
the `Add_Option` or `Append_Positional` procedures of
`Argument_Parser`.

#### Basic types of option in `Parse_Args`

    function Make_Boolean_Option(Default : in Boolean := False)
       return Option_Ptr;
    function Make_Repeated_Option(Default : in Natural := 0)
       return Option_Ptr;

These factories return options that are intended to be specified
without additional values. The Boolean type can only be specified once.
If not specified the default value will be returned, and if specified
the opposite will be returned. The Repeated type can be specified more
than once and the return value is an integer giving the number of times
it appeared on the command line.

    function Make_Integer_Option(Default : in Integer := 0;
                                 Min : in Integer := Integer'First;
                                 Max : in Integer := Integer'Last
                                ) return Option_Ptr;
    function Make_Natural_Option(Default : in Natural := 0)
       return Option_Ptr;
    function Make_Positive_Option(Default : in Positive := 1)
       return Option_Ptr;

The Integer option type takes a default value to be returned if nothing
is specified by the user, together with minimum and maximum valid
values. The Natural and Positive factories are simply a convenience
with the limits pre-set.

    function Make_String_Option(Default : in String := "")
       return Option_Ptr;

This factory returns an option that holds a string.

#### Operations of option types

    function Set(O : in Option) return Boolean;

This function indicates whether a particular option was set. If not, the
value returned may be a default value.

    function Image(O : in Option) return String is abstract;

This returns a string giving the value of the option. This is the
default representation of the converted value, so is not necessarily
the same as the representation accepted from the user.

## Defining new option types using the generic packages

TBC...

## Examples

Two example programs are provided. `simple_example.adb` shows the usage
of the inbuilt option types.

`generic_example.adb` shows more sophisticated usage with custom option
types. The option types are defined in `generic_example_options.ads`,
showing both discrete, floating-point and indefinite (array) option
types.
