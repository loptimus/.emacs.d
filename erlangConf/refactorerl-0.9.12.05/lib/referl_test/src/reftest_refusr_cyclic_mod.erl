%%% basis, WITHOUT  WARRANTY OF ANY  KIND, either expressed or  implied. See
%%% the License  for the specific language governing  rights and limitations
%%% under the License.
%%%
%%% The Original Code is RefactorErl.
%%%
%%% The Initial Developer of the  Original Code is Eötvös Loránd University.
%%% Portions created  by Eötvös  Loránd University are  Copyright 2010,
%%% Eötvös Loránd University. All Rights Reserved.

%%% @doc Unit test for refusr_cyclic_fun
%%%
%%% @author Kinga Száva <guca04@gmail.com>

-module(reftest_refusr_cyclic_mod).
-compile(export_all).

-include("test.hrl").



files()->
	[{module, "test1.erl", 
	"-module(test1).\n"
	"fv1()-> ok.\n"
	"fv2()-> ok.\n"},
	{module, "cycle1.erl",
	"-module(cycle1).\n"
	"-export([fv1/0]).\n"
	"-import(cycle2, [fv2/1]).\n"
	"fv1()-> cycle2:fv2(50).\n"	
	"fv2()-> cycle2:fv2(100).\n"},
	{module, "cycle2.erl",
	"-module(cycle2).\n"
	"-export([fv2/1]).\n"
	"-import(cycle1, [fv1/0]).\n"
	"fv2(A)-> cycle1:fv1() + A.\n"},
	{module, "cycle3.erl",
	"-module(cycle3).\n"
	"-export([f1/1, f2/1, f3/1, f6/1]).\n"
	"-import(cycle4, [f4/1, f5/1]).\n"
	"f1(A)-> A+f2(A).\n"
	"f2(A)-> A*2.\n"
	"f3(A)-> cycle4:f4(A)+5.\n"
	"f6(A)-> f5(A).\n"},
	{module, "cycle4.erl",
	"-module(cycle4).\n"
	"-export([f4/1, f5/1]).\n"
	"-import(cycle3, [f6/1, f3/1]).\n"
	"f4(A)-> cycle3:f3(A).\n"
	"f5(A)-> cycle3:f6(A).\n"}].	
	




test_no_cycle_mod()->
	{false, _} =  refusr_cyclic_mod:check_module(test1),
	ok.

test_cycle_mod1()->
	{true, _} =  refusr_cyclic_mod:check_module(cycle1),
	ok.

test_cycle_mod2()->
	{true, _} =  refusr_cyclic_mod:check_module('cycle1'),
	ok.

%%test_cycle_mod3()->
%%	{true, _} =  refusr_cyclic_mod:check_module({'$gn', mod, 3}),
%%	ok.

test_wrong_mod1()->
	try
		refusr_cyclic_mod:check_module("cycle3:fv6/jhjkjb"),
		error
	catch
		{error, _} ->
			ok
	end.

test_wrong_mod2()->
	try
		refusr_cyclic_mod:check_module(ianatom),
		error
	catch
		{error, _} ->
			ok
	end.

test_wrong_mod3()->
	try
		refusr_cyclic_mod:check_module({'$gn', func, 1}),
		error
	catch
		{error, _} ->
			ok
	end.

test_bad_node()->
	try
		refusr_cyclic_fun:check_module({'$gn', mod, 3000}),
		error
	catch
		Err->
			ok
	end.


%% !! refusr_cyclic_fun:check_function({'$gn', func, 3}),

test_print()->
	ok = refusr_cyclic_mod:print_cycle(),
	ok.

test_check_cycles()->
	ok = refusr_cyclic_mod:check_cycle(),
	ok.

test_draw()->
	refusr_cyclic_mod:draw(),
	ok.
