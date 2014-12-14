# Parse_Args

This is an Ada 2012 package that provides simple command-line argument
parsing. It was inspired by the argparse module in Python, but it is not as
ambitious. The aims are:

1.	to provide a commonly used subset of command-line argument features,
	such as support for long (`--option`) and short (`-o`) options,
	grouped options (in the style of `tar -xvf f.tar`), positional
	arguments, repeated arguments etc.

2.	to be easy to use from the perspective of the application programmer
	without the need to understand the internals of the package.

3.	to use the new features in Ada 2005 and Ada 2012 to their best
	advantage.

While not finished, the package in its current state is quite usable.
The use of Ada 2012 features makes it feel quite flexible.

### Copyright

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

## Use of the `Parse_Args` package

An example program that demonstrates the usage of the package is in
`simple_example.adb`. The general approach to using the package is:

1.	Declare a `Parse_Args.Argument_Parser` object in your main
	subroutine. The examples in this document assume that the object is
	named `AP`.

2.	Use the `Add_Option` method to add option objects to the parser in the
	order in which they should be reported:

		AP.Add_Option(Make_Boolean_Option(False), "foo", 'f', Usage => "The foo option");

	or add positional arguments with the Append_Positional method:

		AP.Append_Positional(Make_String_Option("INFILE"), "infile");

	A series of factory functions `Make_Type_Option` are used to make
	the option objects, and `Add_Option` and `Append_Positional` ensure
	that they are cross-referenced in the `AP` object in such a way
	that they will be identified by the parser at the correct times.

3.	Call `Allow_Tail_Arguments` if it is necessary to support an unlimited
	number of arguments that should be appended to a list.

4.	Use `Set_Prologue` to set the text of a very short overview of the
	pupose of the program to be displayed before the usage text.

5.	Call `AP.Parse_Command_Line` to actually parse the command line.

6.	Whether this was successful can be determined from `AP.Parse_Success`.
	If it was not then `AP.Parse_Message` will return a string with more
	information.

7.	If the user has requested usage information then this can be
	generated with `AP.Usage`.

8.	Option or argument values can then be read from `AP` using various
	functions depending on the output type required. For example:

		Iterations = AP.Natural_Value("iterations");

9.	If you need to retrieve the tail arguments (for example, a list of
	filenames to process) then `AP.Tail` will return a `Double_Linked_List`
	of `String` values which you can iterate over.

10.	Alternatively, the new features in Ada 2012 mean that the
	`Argument_Parser` object can be both a container, returning the option
	object corresponding to a given option name, and an iterator.

## Internals

The record `Argument_Parser`holds a variety of lists and maps that are
filled up with references to `Option` objects. The actual parsing is done
with a finite state machine which should either end up in the state
`Finish_Success` or `Finish_Erroneous`.
