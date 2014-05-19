% ----------------------------------------------------------------------
% BEGIN LICENSE BLOCK
% Version: CMPL 1.1
%
% The contents of this file are subject to the Cisco-style Mozilla Public
% License Version 1.1 (the "License"); you may not use this file except
% in compliance with the License.  You may obtain a copy of the License
% at www.eclipse-clp.org/license.
% 
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
% the License for the specific language governing rights and limitations
% under the License. 
% 
% The Original Code is  The ECLiPSe Constraint Logic Programming System. 
% The Initial Developer of the Original Code is  Cisco Systems, Inc. 
% Portions created by the Initial Developer are
% Copyright (C) 2000-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: eclipse_language.ecl,v 1.3 2008/08/20 22:57:32 jschimpf Exp $
% Author/s:	Joachim Schimpf, IC-Parc
% ----------------------------------------------------------------------

:- module(eclipse_language,[],[sepia_kernel]).

:- pragma(deprecated_warnings(off)).

:- reexport (?-) / 2 from sepia_kernel. % dummy
:- reexport (!) / 0 from sepia_kernel.
:- reexport (*) / 3 from sepia_kernel.
:- reexport (+) / 2 from sepia_kernel.
:- reexport (+) / 3 from sepia_kernel.
:- reexport (',') / 2 from sepia_kernel.
:- reexport (-) / 2 from sepia_kernel.
:- reexport (-) / 3 from sepia_kernel.
:- reexport (->) / 2 from sepia_kernel.
:- reexport (-->) / 2 from sepia_kernel. % dummy
:- reexport (*->) / 2 from sepia_kernel.
:- reexport '.' / 2 from sepia_kernel.
:- reexport (/) / 3 from sepia_kernel.
:- reexport (//) / 3 from sepia_kernel.
:- reexport (/\) / 3 from sepia_kernel.
:- reexport (:) / 2 from sepia_kernel.
:- reexport (;) / 2 from sepia_kernel.
:- reexport (<) / 2 from sepia_kernel.
:- reexport (<<) / 3 from sepia_kernel.
:- reexport (=) / 2 from sepia_kernel.
:- reexport (=..) / 2 from sepia_kernel.
:- reexport (=:=) / 2 from sepia_kernel.
:- reexport (=<) / 2 from sepia_kernel.
:- reexport (==) / 2 from sepia_kernel.
:- reexport (=\=) / 2 from sepia_kernel.
:- reexport (>) / 2 from sepia_kernel.
:- reexport (>=) / 2 from sepia_kernel.
:- reexport (>>) / 3 from sepia_kernel.
:- reexport (@) / 2 from sepia_kernel.
:- reexport (@<) / 2 from sepia_kernel.
:- reexport (@=<) / 2 from sepia_kernel.
:- reexport (@>) / 2 from sepia_kernel.
:- reexport (@>=) / 2 from sepia_kernel.
:- reexport 'C' / 3 from sepia_kernel.
:- reexport (\) / 2 from sepia_kernel.
:- reexport (\+) / 1 from sepia_kernel.
:- reexport (\/) / 3 from sepia_kernel.
:- reexport (\=) / 2 from sepia_kernel.
:- reexport (\==) / 2 from sepia_kernel.
:- reexport (^) / 2 from sepia_kernel.
:- reexport (^) / 3 from sepia_kernel.
:- reexport (abolish) / 1 from sepia_kernel.
:- reexport abolish_op / 2 from sepia_kernel.
:- reexport abolish_record / 1 from sepia_kernel.
:- reexport abort / 0 from sepia_kernel.
:- reexport abs / 2 from sepia_kernel.
:- reexport accept / 3 from sepia_kernel.
:- reexport acos / 2 from sepia_kernel.
:- reexport acyclic_term / 1 from sepia_kernel.
:- reexport add_attribute / 2 from sepia_kernel.
:- reexport add_attribute / 3 from sepia_kernel.
:- reexport alarm / 1 from sepia_kernel.
:- reexport als / 1 from sepia_kernel.
:- reexport append_strings / 3 from sepia_kernel.
:- reexport arg / 3 from sepia_kernel.
:- reexport argc / 1 from sepia_kernel.
:- reexport argv / 2 from sepia_kernel.
:- reexport arity / 2 from sepia_kernel.
:- reexport asin / 2 from sepia_kernel.
:- reexport assert / 1 from sepia_kernel.
:- reexport asserta / 1 from sepia_kernel.
:- reexport assertz / 1 from sepia_kernel.
:- reexport at / 2 from sepia_kernel.
:- reexport at_eof / 1 from sepia_kernel.
:- reexport atan / 2 from sepia_kernel.
:- reexport atan / 3 from sepia_kernel.
:- reexport atom / 1 from sepia_kernel.
:- reexport atom_length / 2 from sepia_kernel.
:- reexport atom_string / 2 from sepia_kernel.
:- reexport atomic / 1 from sepia_kernel.
:- reexport attach_suspensions / 2 from sepia_kernel.
:- reexport attached_suspensions / 2 from sepia_kernel.
:- reexport b_external / 1 from sepia_kernel.
:- reexport b_external / 2 from sepia_kernel.
:- reexport bag_abolish / 1 from sepia_kernel.
:- reexport bag_count / 2 from sepia_kernel.
:- reexport bag_create / 1 from sepia_kernel.
:- reexport bag_dissolve / 2 from sepia_kernel.
:- reexport bag_enter / 2 from sepia_kernel.
:- reexport bag_erase / 1 from sepia_kernel.
:- reexport bag_retrieve / 2 from sepia_kernel.
:- reexport bagof / 3 from sepia_kernel.
:- reexport begin_module / 1 from sepia_kernel.
:- reexport between / 4 from sepia_kernel.
:- reexport bind / 2 from sepia_kernel.
:- reexport block / 3 from sepia_kernel.
:- reexport breal / 1 from sepia_kernel.
:- reexport breal / 2 from sepia_kernel.
:- reexport breal_from_bounds / 3 from sepia_kernel.
:- reexport breal_min / 2 from sepia_kernel.
:- reexport breal_max / 2 from sepia_kernel.
:- reexport breal_bounds / 3 from sepia_kernel.
:- reexport bytes_to_term / 2 from sepia_kernel.
:- reexport call / 1 from sepia_kernel.
:- reexport call / 2 from sepia_kernel.
:- reexport call_c / 2 from sepia_kernel.
:- reexport call_explicit / 2 from sepia_kernel.
:- reexport call_local / 1 from sepia_kernel.
:- reexport call_priority / 2 from sepia_kernel.
:- reexport callable / 1 from sepia_kernel.
:- reexport cancel_after_event / 1 from sepia_kernel.
:- reexport cancel_after_event / 2 from sepia_kernel.
:- reexport canonical_path_name / 2 from sepia_kernel.
:- reexport cd / 1 from sepia_kernel.
:- reexport ceiling / 2 from sepia_kernel.
:- reexport char_code / 2 from sepia_kernel.
:- reexport char_int / 2 from sepia_kernel.
:- reexport clause / 1 from sepia_kernel.
:- reexport clause / 2 from sepia_kernel.
:- reexport close / 1 from sepia_kernel.
:- reexport clrbit / 3 from sepia_kernel.
:- reexport comment / 2 from sepia_kernel.
:- reexport compare / 3 from sepia_kernel.
:- reexport compare_instances / 3 from sepia_kernel.
:- reexport compiled_stream / 1 from sepia_kernel.
:- reexport compound / 1 from sepia_kernel.
:- reexport concat_atom / 2 from sepia_kernel.
:- reexport concat_atoms / 3 from sepia_kernel.
:- reexport concat_string / 2 from sepia_kernel.
:- reexport concat_strings / 3 from sepia_kernel.
:- reexport connect / 2 from sepia_kernel.
:- reexport copy_term / 2 from sepia_kernel.
:- reexport copy_term / 3 from sepia_kernel.
:- reexport copy_term_vars / 3 from sepia_kernel.
:- reexport coroutine / 0 from sepia_kernel.
:- reexport cos / 2 from sepia_kernel.
:- reexport coverof / 3 from sepia_kernel.
:- reexport cputime / 1 from sepia_kernel.
:- reexport create_module / 1 from sepia_kernel.
:- reexport create_module / 3 from sepia_kernel.
:- reexport current_after_event / 1 from sepia_kernel.
:- reexport current_after_events / 1 from sepia_kernel.
:- reexport current_array / 2 from sepia_kernel.
:- reexport current_atom / 1 from sepia_kernel.
:- reexport current_built_in / 1 from sepia_kernel.
:- reexport current_compiled_file / 3 from sepia_kernel.
:- reexport current_domain / 3 from sepia_kernel.
:- reexport current_error / 1 from sepia_kernel.
:- reexport current_functor / 1 from sepia_kernel.
:- reexport current_interrupt / 2 from sepia_kernel.
:- reexport current_macro / 4 from sepia_kernel.
:- reexport current_module / 1 from sepia_kernel.
:- reexport current_module_predicate / 2 from sepia_kernel.
:- reexport current_op / 3 from sepia_kernel.
:- reexport current_pragma / 1 from sepia_kernel.
:- reexport current_predicate / 1 from sepia_kernel.
:- reexport current_record / 1 from sepia_kernel.
:- reexport current_store / 1 from sepia_kernel.
:- reexport current_stream / 1 from sepia_kernel.
:- reexport current_stream / 3 from sepia_kernel.
:- reexport current_struct / 1 from sepia_kernel.
:- reexport current_struct / 2 from sepia_kernel.
:- reexport current_suspension / 1 from sepia_kernel.
:- reexport current_trigger / 1 from sepia_kernel.
:- reexport date / 1 from sepia_kernel.
:- reexport dbgcomp / 0 from sepia_kernel.
:- reexport debug / 1 from sepia_kernel.
:- reexport debug_reset / 0 from sepia_kernel.
:- reexport debugging / 0 from sepia_kernel.
:- reexport decval / 1 from sepia_kernel.
:- reexport define_error / 2 from sepia_kernel.
:- reexport define_macro / 3 from sepia_kernel.
:- reexport (delay) / 1 from sepia_kernel.
:- reexport delayed_goals / 1 from sepia_kernel.
:- reexport delayed_goals / 2 from sepia_kernel.
:- reexport delayed_goals_number / 2 from sepia_kernel.
:- reexport delete / 1 from sepia_kernel.
:- reexport (demon) / 1 from sepia_kernel.
:- reexport denominator / 2 from sepia_kernel.
:- reexport deprecated / 2 from sepia_kernel.
:- reexport dim / 2 from sepia_kernel.
:- reexport discontiguous / 1 from sepia_kernel.
:- reexport display / 1 from sepia_kernel.
:- reexport display / 2 from sepia_kernel.
:- reexport (div) / 3 from sepia_kernel.
:- reexport (do) / 2 from sepia_kernel.
:- reexport (dynamic) / 1 from sepia_kernel.
:- reexport ensure_loaded / 1 from sepia_kernel.
:- reexport enter_suspension_list / 3 from sepia_kernel.
:- reexport env / 0 from sepia_kernel.
:- reexport erase / 1 from sepia_kernel.
:- reexport erase / 2 from sepia_kernel.
:- reexport erase_all / 1 from sepia_kernel.
:- reexport erase_array / 1 from sepia_kernel.
:- reexport erase_macro / 1 from sepia_kernel.
:- reexport erase_macro / 2 from sepia_kernel.
:- reexport erase_module / 1 from sepia_kernel.
:- reexport errno_id / 1 from sepia_kernel.
:- reexport errno_id / 2 from sepia_kernel.
:- reexport error / 2 from sepia_kernel.
:- reexport error / 3 from sepia_kernel.
:- reexport error_id / 2 from sepia_kernel.
:- reexport eval / 2 from sepia_kernel.
:- reexport event / 1 from sepia_kernel.
:- reexport event_after / 2 from sepia_kernel.
:- reexport event_after / 3 from sepia_kernel.
:- reexport event_after_every / 2 from sepia_kernel.
:- reexport event_create / 2 from sepia_kernel.
:- reexport event_create / 3 from sepia_kernel.
:- reexport event_retrieve / 2 from sepia_kernel.
:- reexport event_retrieve / 3 from sepia_kernel.
:- reexport events_defer / 0 from sepia_kernel.
:- reexport events_nodefer / 0 from sepia_kernel.
:- reexport event_enable / 1 from sepia_kernel.
:- reexport event_disable / 1 from sepia_kernel.
:- reexport events_after / 1 from sepia_kernel.
:- reexport exec / 2 from sepia_kernel.
:- reexport exec / 3 from sepia_kernel.
:- reexport exec_group / 3 from sepia_kernel.
:- reexport existing_file / 4 from sepia_kernel.
:- reexport exists / 1 from sepia_kernel.
:- reexport exit / 1 from sepia_kernel.
:- reexport exit_block / 1 from sepia_kernel.
:- reexport exp / 2 from sepia_kernel.
:- reexport expand_clause / 2 from sepia_kernel.
:- reexport expand_goal / 2 from sepia_kernel.
:- reexport expand_macros / 2 from sepia_kernel.
:- reexport (export) / 1 from sepia_kernel.
:- reexport external / 1 from sepia_kernel.
:- reexport external / 2 from sepia_kernel.
:- reexport fail / 0 from sepia_kernel.
:- reexport fail_if / 1 from sepia_kernel.
:- reexport false / 0 from sepia_kernel.
:- reexport findall / 3 from sepia_kernel.
:- reexport fix / 2 from sepia_kernel.
:- reexport flatten_array / 2 from sepia_kernel.
:- reexport float / 1 from sepia_kernel.
:- reexport float / 2 from sepia_kernel.
:- reexport floor / 2 from sepia_kernel.
:- reexport flush / 1 from sepia_kernel.
:- reexport fork / 2 from sepia_kernel.
:- reexport frandom / 1 from sepia_kernel.
:- reexport free / 1 from sepia_kernel.
:- reexport functor / 3 from sepia_kernel.
:- reexport garbage_collect / 0 from sepia_kernel.
:- reexport gcd / 3 from sepia_kernel.
:- reexport gcd / 5 from sepia_kernel.
:- reexport get / 1 from sepia_kernel.
:- reexport get / 2 from sepia_kernel.
:- reexport get_char / 1 from sepia_kernel.
:- reexport get_char / 2 from sepia_kernel.
:- reexport get_chtab / 2 from sepia_kernel.
:- reexport get_error_handler / 3 from sepia_kernel.
:- reexport get_event_handler / 3 from sepia_kernel.
:- reexport get_file_info / 3 from sepia_kernel.
:- reexport get_flag / 2 from sepia_kernel.
:- reexport get_flag / 3 from sepia_kernel.
:- reexport get_interrupt_handler / 3 from sepia_kernel.
:- reexport get_leash / 2 from sepia_kernel.
:- reexport get_module_info / 3 from sepia_kernel.
:- reexport get_priority / 1 from sepia_kernel.
:- reexport get_prompt / 3 from sepia_kernel.
:- reexport get_stream / 2 from sepia_kernel.
:- reexport get_stream_info / 3 from sepia_kernel.
:- reexport get_suspension_data / 3 from sepia_kernel.
:- reexport get_timer / 2 from sepia_kernel.
:- reexport get_var_info / 3 from sepia_kernel.
:- reexport get_var_bounds / 3 from sepia_kernel.
:- reexport getbit / 3 from sepia_kernel.
:- reexport getcwd / 1 from sepia_kernel.
:- reexport getenv / 2 from sepia_kernel.
:- reexport getval / 2 from sepia_kernel.
:- reexport (global) / 1 from sepia_kernel.
:- reexport global_op / 3 from sepia_kernel.
:- reexport ground / 1 from sepia_kernel.
:- reexport halt / 0 from sepia_kernel.
:- reexport (help) / 0 from sepia_kernel.
:- reexport (help) / 1 from sepia_kernel.
:- reexport (import) / 1 from sepia_kernel.
:- reexport incval / 1 from sepia_kernel.
:- reexport init_suspension_list / 2 from sepia_kernel.
:- reexport inline / 2 from sepia_kernel.
:- reexport insert_suspension / 3 from sepia_kernel.
:- reexport insert_suspension / 4 from sepia_kernel.
:- reexport instance / 2 from sepia_kernel.
:- reexport integer / 1 from sepia_kernel.
:- reexport integer / 2 from sepia_kernel.
:- reexport integer_atom / 2 from sepia_kernel.
:- reexport (is) / 2 from sepia_kernel.
:- reexport is_built_in / 1 from sepia_kernel.
:- reexport is_dynamic / 1 from sepia_kernel.
:- reexport is_event / 1 from sepia_kernel.
:- reexport is_handle / 1 from sepia_kernel.
:- reexport is_list / 1 from sepia_kernel.
:- reexport is_locked / 1 from sepia_kernel.
:- reexport is_predicate / 1 from sepia_kernel.
:- reexport is_record / 1 from sepia_kernel.
:- reexport is_suspension / 1 from sepia_kernel.
:- reexport join_string / 3 from sepia_kernel.
:- reexport keysort / 2 from sepia_kernel.
:- reexport kill / 2 from sepia_kernel.
:- reexport kill_display_matrix / 1 from sepia_kernel.
:- reexport kill_suspension / 1 from sepia_kernel.
:- reexport lcm / 3 from sepia_kernel.
:- reexport lib / 1 from sepia_kernel.
:- reexport lib / 2 from sepia_kernel.
:- reexport listen / 2 from sepia_kernel.
:- reexport (listing) / 0 from sepia_kernel.
:- reexport (listing) / 1 from sepia_kernel.
:- reexport ln / 2 from sepia_kernel.
:- reexport load / 1 from sepia_kernel.
:- reexport (local) / 1 from sepia_kernel.
:- reexport local_record / 1 from sepia_kernel.
:- reexport local_time / 8 from sepia_kernel.
:- reexport local_time_string / 3 from sepia_kernel.
:- reexport lock / 0 from sepia_kernel.
:- reexport lock / 1 from sepia_kernel.		% obsolete
:- reexport lock / 2 from sepia_kernel.		% obsolete
:- reexport lock_pass / 1 from sepia_kernel.
:- reexport make / 0 from sepia_kernel.
:- reexport make_array / 1 from sepia_kernel.
:- reexport make_array / 2 from sepia_kernel.
:- reexport make_display_matrix / 2 from sepia_kernel.
:- reexport make_display_matrix / 5 from sepia_kernel.
:- reexport make_local_array / 1 from sepia_kernel.
:- reexport make_local_array / 2 from sepia_kernel.
:- reexport make_suspension / 3 from sepia_kernel.
:- reexport make_suspension / 4 from sepia_kernel.
:- reexport max / 2 from sepia_kernel.
:- reexport max / 3 from sepia_kernel.
:- reexport merge / 3 from sepia_kernel.
:- reexport merge / 5 from sepia_kernel.
:- reexport merge_suspension_lists / 4 from sepia_kernel.
:- reexport meta / 1 from sepia_kernel.
:- reexport meta_attribute / 2 from sepia_kernel.
:- reexport min / 2 from sepia_kernel.
:- reexport min / 3 from sepia_kernel.
:- reexport mkdir / 1 from sepia_kernel.
:- reexport (mod) / 3 from sepia_kernel.
:- reexport (mode) / 1 from sepia_kernel.
:- reexport msort / 2 from sepia_kernel.
:- reexport mutex / 2 from sepia_kernel.
:- reexport mutex_init / 1 from sepia_kernel.
:- reexport name / 2 from sepia_kernel.
:- reexport new_socket_server / 3 from sepia_kernel.
:- reexport nl / 0 from sepia_kernel.
:- reexport nl / 1 from sepia_kernel.
:- reexport nodbgcomp / 0 from sepia_kernel.
:- reexport nonground / 1 from sepia_kernel.
:- reexport nonground / 2 from sepia_kernel.
:- reexport nonground / 3 from sepia_kernel.
:- reexport nonvar / 1 from sepia_kernel.
:- reexport (nospy) / 1 from sepia_kernel.
:- reexport (not) / 1 from sepia_kernel.
:- reexport not_unify / 2 from sepia_kernel.
:- reexport notify_constrained / 1 from sepia_kernel.
:- reexport number / 1 from sepia_kernel.
:- reexport number_merge / 3 from sepia_kernel.
:- reexport number_merge / 5 from sepia_kernel.
:- reexport number_sort / 2 from sepia_kernel.
:- reexport number_sort / 4 from sepia_kernel.
:- reexport number_string / 2 from sepia_kernel.
:- reexport numerator / 2 from sepia_kernel.
:- reexport occurs / 2 from sepia_kernel.
:- reexport (once) / 1 from sepia_kernel.
:- reexport op / 3 from sepia_kernel.
:- reexport open / 3 from sepia_kernel.
:- reexport open / 4 from sepia_kernel.
:- reexport os_file_name / 2 from sepia_kernel.
:- reexport (parallel) / 1 from sepia_kernel.
:- reexport pathname / 2 from sepia_kernel.
:- reexport pathname / 3 from sepia_kernel.
:- reexport pathname / 4 from sepia_kernel.
:- reexport pause / 0 from sepia_kernel.
:- reexport peer / 1 from sepia_kernel.
:- reexport peer_get_property / 3 from sepia_kernel.
:- reexport peer_queue_create / 5 from sepia_kernel.
:- reexport peer_queue_close / 1 from sepia_kernel.
:- reexport peer_queue_get_property / 3 from sepia_kernel.
:- reexport peer_multitask_confirm / 0 from sepia_kernel.
:- reexport peer_multitask_terminate / 0 from sepia_kernel.
:- reexport peer_register_multitask / 2 from sepia_kernel.
:- reexport peer_deregister_multitask / 1 from sepia_kernel.
:- reexport peer_do_multitask / 1 from sepia_kernel.
:- reexport phrase / 2 from sepia_kernel.
:- reexport phrase / 3 from sepia_kernel.
:- reexport pipe / 2 from sepia_kernel.
:- reexport plus / 3 from sepia_kernel.
:- reexport portray_goal / 2 from sepia_kernel.
:- reexport portray_term / 3 from sepia_kernel.
:- reexport pred / 1 from sepia_kernel.
:- reexport print / 1 from sepia_kernel.
:- reexport print / 2 from sepia_kernel.
:- reexport printf / 2 from sepia_kernel.
:- reexport printf / 3 from sepia_kernel.
:- reexport prune_instances / 2 from sepia_kernel.
:- reexport put / 1 from sepia_kernel.
:- reexport put / 2 from sepia_kernel.
:- reexport put_char / 1 from sepia_kernel.
:- reexport put_char / 2 from sepia_kernel.
:- reexport random / 1 from sepia_kernel.
:- reexport rational / 1 from sepia_kernel.
:- reexport rational / 2 from sepia_kernel.
:- reexport rationalize / 2 from sepia_kernel.
:- reexport read / 1 from sepia_kernel.
:- reexport read / 2 from sepia_kernel.
:- reexport read_annotated / 2 from sepia_kernel.
:- reexport read_annotated / 3 from sepia_kernel.
:- reexport read_directory / 4 from sepia_kernel.
:- reexport read_exdr / 2 from sepia_kernel.
:- reexport read_string / 3 from sepia_kernel.
:- reexport read_string / 4 from sepia_kernel.
:- reexport read_token / 2 from sepia_kernel.
:- reexport read_token / 3 from sepia_kernel.
:- reexport readvar / 3 from sepia_kernel.
:- reexport read_term / 2 from sepia_kernel.
:- reexport read_term / 3 from sepia_kernel.
:- reexport real / 1 from sepia_kernel.
:- reexport record / 2 from sepia_kernel.
:- reexport record_create / 1 from sepia_kernel.
:- reexport recorda / 2 from sepia_kernel.
:- reexport recorda / 3 from sepia_kernel.
:- reexport recorded / 2 from sepia_kernel.
:- reexport recorded / 3 from sepia_kernel.
:- reexport recorded_list / 2 from sepia_kernel.
:- reexport recordz / 2 from sepia_kernel.
:- reexport recordz / 3 from sepia_kernel.
:- reexport (reexport) / 1 from sepia_kernel.
:- reexport referenced_record / 2 from sepia_kernel.
:- reexport rename / 2 from sepia_kernel.
:- reexport (rem) / 3 from sepia_kernel.
:- reexport remote_connect / 3 from sepia_kernel.
:- reexport remote_connect_setup / 3 from sepia_kernel.
:- reexport remote_connect_accept / 6 from sepia_kernel.
:- reexport remote_disconnect / 1 from sepia_kernel.
:- reexport remote_yield / 1 from sepia_kernel.
:- reexport repeat / 0 from sepia_kernel.
:- reexport rerecord / 2 from sepia_kernel.
:- reexport reset_error_handler / 1 from sepia_kernel.
:- reexport reset_error_handlers / 0 from sepia_kernel.
:- reexport reset_event_handler / 1 from sepia_kernel.
:- reexport retract / 1 from sepia_kernel.
:- reexport retract_all / 1 from sepia_kernel.
:- reexport retractall / 1 from sepia_kernel.
:- reexport round / 2 from sepia_kernel.
:- reexport schedule_suspensions / 1 from sepia_kernel.
:- reexport schedule_suspensions / 2 from sepia_kernel.
:- reexport schedule_woken / 1 from sepia_kernel.
:- reexport seed / 1 from sepia_kernel.
:- reexport seek / 2 from sepia_kernel.
:- reexport select / 3 from sepia_kernel.
:- reexport set_chtab / 2 from sepia_kernel.
:- reexport set_error_handler / 2 from sepia_kernel.
:- reexport set_event_handler / 2 from sepia_kernel.
:- reexport set_flag / 2 from sepia_kernel.
:- reexport set_flag / 3 from sepia_kernel.
:- reexport set_interrupt_handler / 2 from sepia_kernel.
:- reexport set_leash / 2 from sepia_kernel.
:- reexport set_prompt / 3 from sepia_kernel.
:- reexport set_stream / 2 from sepia_kernel.
:- reexport set_stream_property / 3 from sepia_kernel.
:- reexport set_suspension_data / 3 from sepia_kernel.
:- reexport set_suspension_priority / 2 from sepia_kernel.
:- reexport set_timer / 2 from sepia_kernel.
:- reexport set_var_bounds / 3 from sepia_kernel.
:- reexport setarg / 3 from sepia_kernel.
:- reexport setbit / 3 from sepia_kernel.
:- reexport setenv / 2 from sepia_kernel.
:- reexport setof / 3 from sepia_kernel.
:- reexport setval / 2 from sepia_kernel.
:- reexport sgn / 2 from sepia_kernel.
:- reexport sh / 1 from sepia_kernel.
:- reexport shelf_abolish / 1 from sepia_kernel.
:- reexport shelf_create / 2 from sepia_kernel.
:- reexport shelf_create / 3 from sepia_kernel.
:- reexport shelf_dec / 2 from sepia_kernel.
:- reexport shelf_get / 3 from sepia_kernel.
:- reexport shelf_inc / 2 from sepia_kernel.
:- reexport shelf_set / 3 from sepia_kernel.
:- reexport sin / 2 from sepia_kernel.
:- reexport (skipped) / 1 from sepia_kernel.
:- reexport sleep / 1 from sepia_kernel.
:- reexport socket / 3 from sepia_kernel.
:- reexport sort / 2 from sepia_kernel.
:- reexport sort / 4 from sepia_kernel.
:- reexport split_string / 4 from sepia_kernel.
:- reexport sprintf / 3 from sepia_kernel.
:- reexport (spy) / 1 from sepia_kernel.
:- reexport spy_term / 2 from sepia_kernel.
:- reexport spy_var / 1 from sepia_kernel.
:- reexport sqrt / 2 from sepia_kernel.
:- reexport statistics / 0 from sepia_kernel.
:- reexport statistics / 1 from sepia_kernel.
:- reexport statistics / 2 from sepia_kernel.
:- reexport store_create / 1 from sepia_kernel.
:- reexport store_count / 2 from sepia_kernel.
:- reexport store_erase / 1 from sepia_kernel.
:- reexport store_delete / 2 from sepia_kernel.
:- reexport store_get / 3 from sepia_kernel.
:- reexport store_inc / 2 from sepia_kernel.
:- reexport store_set / 3 from sepia_kernel.
:- reexport store_contains / 2 from sepia_kernel.
:- reexport stored_keys / 2 from sepia_kernel.
:- reexport stored_keys_and_values / 2 from sepia_kernel.
:- reexport stream_truncate / 1 from sepia_kernel.
:- reexport string / 1 from sepia_kernel.
:- reexport string_code / 3 from sepia_kernel.
:- reexport string_length / 2 from sepia_kernel.
:- reexport string_list / 2 from sepia_kernel.
:- reexport string_list / 3 from sepia_kernel.
:- reexport subcall / 2 from sepia_kernel.
:- reexport subscript / 3 from sepia_kernel.
:- reexport substring / 3 from sepia_kernel.
:- reexport substring / 4 from sepia_kernel.
:- reexport substring / 5 from sepia_kernel.
:- reexport succ / 2 from sepia_kernel.
:- reexport suffix / 2 from sepia_kernel.
:- reexport sum / 2 from sepia_kernel.
:- reexport suspend / 3 from sepia_kernel.
:- reexport suspend / 4 from sepia_kernel.
:- reexport suspension_to_goal / 3 from sepia_kernel.
:- reexport suspensions / 1 from sepia_kernel.
:- reexport suspensions / 2 from sepia_kernel.
:- reexport domain_index / 3 from sepia_kernel.
:- reexport system / 1 from sepia_kernel.
:- reexport tan / 2 from sepia_kernel.
:- reexport term_hash / 4 from sepia_kernel.
:- reexport term_string / 2 from sepia_kernel.
:- reexport term_to_bytes / 2 from sepia_kernel.
:- reexport term_variables / 2 from sepia_kernel.
:- reexport test_and_setval / 3 from sepia_kernel.
:- reexport times / 3 from sepia_kernel.
:- reexport tool / 1 from sepia_kernel.
:- reexport tool / 2 from sepia_kernel.
:- reexport tool_body / 3 from sepia_kernel.
:- reexport trace / 1 from sepia_kernel.
:- reexport trace_call_port / 3 from sepia_kernel.
:- reexport trace_exit_port / 0 from sepia_kernel.
:- reexport trace_parent_port / 1 from sepia_kernel.
:- reexport trace_point_port / 3 from sepia_kernel.
:- reexport (traceable) / 1 from sepia_kernel.
:- reexport trigger / 1 from sepia_kernel.
:- reexport trimcore / 0 from sepia_kernel.
:- reexport true / 0 from sepia_kernel.
:- reexport truncate / 2 from sepia_kernel.
:- reexport tyi / 1 from sepia_kernel.
:- reexport tyi / 2 from sepia_kernel.
:- reexport tyo / 1 from sepia_kernel.
:- reexport tyo / 2 from sepia_kernel.
:- reexport type_of / 2 from sepia_kernel.
:- reexport unget / 1 from sepia_kernel.
:- reexport unlock / 2 from sepia_kernel.
:- reexport (unskipped) / 1 from sepia_kernel.
:- reexport (untraceable) / 1 from sepia_kernel.
:- reexport update_struct / 4 from sepia_kernel.
:- reexport use_module / 1 from sepia_kernel.
:- reexport var / 1 from sepia_kernel.
:- reexport variant / 2 from sepia_kernel.
:- reexport wait / 2 from sepia_kernel.
:- reexport wait / 3 from sepia_kernel.
:- reexport wake / 0 from sepia_kernel.
:- reexport write / 1 from sepia_kernel.
:- reexport write / 2 from sepia_kernel.
:- reexport write_canonical / 1 from sepia_kernel.
:- reexport write_canonical / 2 from sepia_kernel.
:- reexport write_exdr / 2 from sepia_kernel.
:- reexport write_term / 2 from sepia_kernel.
:- reexport write_term / 3 from sepia_kernel.
:- reexport writeclause / 1 from sepia_kernel.
:- reexport writeclause / 2 from sepia_kernel.
:- reexport writeln / 1 from sepia_kernel.
:- reexport writeln / 2 from sepia_kernel.
:- reexport writeq / 1 from sepia_kernel.
:- reexport writeq / 2 from sepia_kernel.
:- reexport xget / 3 from sepia_kernel.
:- reexport xor / 3 from sepia_kernel.
:- reexport xset / 3 from sepia_kernel.
:- reexport yield / 2 from sepia_kernel.
:- reexport (~) / 1 from sepia_kernel.
:- reexport (~=) / 2 from sepia_kernel.

:- reexport
	append/3,
	delete/3,
	length/2,
	member/2,
	memberchk/2,
	nonmember/2,
	subtract/3,
	reverse/2
    from sepia_kernel.

:- reexport struct(suspend{}) from sepia_kernel.
:- reexport struct(annotated_term{}) from sepia_kernel.

:- reexport
	tr_if_suspend/3,
	macro((if)/2,_,_)
    from suspend.

/********* Autoloading library predicates ***************/

:- autoload_system(ecl_compiler, [
    	compile/1->compile_/2,
	compile/2->compile_/3,
	compile_stream/1->compile_stream_/2,
	compile_term/1->compile_term_/2,
	compile_term/2->compile_term_/3,
	compile_term_annotated/3->compile_term_annotated_/4
    ]).
:- reexport ecl_compiler.

:- autoload_system(lists, [
	collection_to_list/2, flatten/2, intersection/3, subset/2, union/3,
	checklist/2->checklist_body/3, maplist/3->maplist_body/4, print_list/1->print_list_/2]).
:- autoload_system(profile, [profile/1->profile_body/2, profile/2->profile_body/3]).

:- (reexport
	append/3,
	delete/3,
	length/2,
	member/2,
	memberchk/2,
	nonmember/2,
	subtract/3,
	reverse/2
    from sepia_kernel) @ lists.

:- reexport lists.
:- reexport profile.

