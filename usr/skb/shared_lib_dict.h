
#include <dlfcn.h>

extern void p_get_threshold();
extern void p_set_threshold();
extern void create_ic_attr();
extern void make_ic_var_attr();
extern void get_var_info();
extern void make_var_info();
extern void get_var_info_from_attr();
extern void set_type_integral();
extern void ic_lwb();
extern void p_ic_impose_min();
extern void ic_upb();
extern void p_ic_impose_max();
extern void p_ic_impose_bounds();
extern void make_constrained_ic_var();
extern void p_make_bool();
extern void set_integral();
extern void p_set_var_integer();
extern void set_up_exclude_delayed_goal();
extern void ic_exclude();
extern void p_ic_exclude();
extern void set_up_exclude_range_delayed_goal();
extern void ic_exclude_range();
extern void p_ic_exclude_range();
extern void impose_coef_bounds();
extern void solve_equal();
extern void solve_not_equal();
extern void update_con_data_buf();
extern void con_struct_vec_to_con_info();
extern void finish_setting_up_con_struct();
extern void swap_entries();
extern void update_ef();
extern void evaluate_reified();
extern void check_ic_0v_con();
extern void prop_ic_1v_con();
extern void prop_pass_1();
extern void prop_pass_2();
extern void prop_ic_neq_con();
extern void short_cuts();
extern void sync_attr_with_new_bitmap();
extern void p_ac_eq_init();
extern void p_ac_eq_prop();
extern void p_prop_ic_con();
extern void type_check_lin_terms();
extern void setup_pass_1();
extern void process_lin_terms_to_vectors_and_prop();
extern void setup_1v_con();
extern void p_set_up_ic_con();
extern void p_get_print_info();
extern void p_ic_init();
extern void p_ic_constraints_init();
extern void p_get_ic_attr();
extern void unify_integer_bounds();
extern void p_get_bounds();
extern void p_get_integer_bounds1();
extern void p_get_domain_size();
extern void unify_number_ic();
extern void unify_ic_ic();
extern void p_unify_ic();
extern void p_indomain_init();
extern void p_indomain_try();
extern void bit_count();
extern void lsb();
extern void msb();
extern void p_create_bitmap();
extern void create_bitmap();
extern void p_set_bitmap_lwb();
extern void set_bitmap_lwb();
extern void p_set_bitmap_upb();
extern void set_bitmap_upb();
extern void p_remove_bitmap_element();
extern void remove_bitmap_element();
extern void p_remove_bitmap_range();
extern void remove_bitmap_range();
extern void p_bitmap_intersect_into();
extern void bitmap_intersect_into();
extern void p_bitmap_shifted_intersect_into();
extern void bitmap_shifted_intersect_into();
extern void p_bitmaps_have_non_empty_intersection();
extern void bitmaps_have_non_empty_intersection();
extern void p_bitmap_union();
extern void bitmap_union();
extern void p_copy_bitmap();
extern void copy_bitmap();
extern void p_copy_bitmap_shifted();
extern void copy_bitmap_shifted();
extern void p_bitmap_range();
extern void bitmap_range();
extern void p_get_bitmap_lwb();
extern void get_bitmap_lwb();
extern void p_get_bitmap_upb();
extern void get_bitmap_upb();
extern void p_next_greater_member();
extern void next_greater_member();
extern void p_next_smaller_member();
extern void next_smaller_member();
extern void p_next_greater_non_member();
extern void next_greater_non_member();
extern void p_next_smaller_non_member();
extern void next_smaller_non_member();
extern void p_bitmap_size();
extern void bitmap_size();
extern void p_bitmap_contains();
extern void bitmap_contains();
extern void p_bitmap_contains_range();
extern void bitmap_contains_range();
extern void p_compare_bitmaps();
extern void compare_bitmaps();
extern void ec_init_ef();
extern void ec_init_task();
extern void ec_ef_disj();
extern void ec_ef_cum();
extern void ec_ef_quad();
extern void ec_regcomp();
extern void ec_regmatch();
extern void ec_regmatch4();
extern void ec_regmatchsub();
extern void ec_regmatchall();
extern void ec_regsplit();


struct function_entry funcs[] = {

{.name="p_get_threshold",.f=p_get_threshold},
{.name="p_set_threshold",.f=p_set_threshold},
{.name="create_ic_attr",.f=create_ic_attr},
{.name="make_ic_var_attr",.f=make_ic_var_attr},
{.name="get_var_info",.f=get_var_info},
{.name="make_var_info",.f=make_var_info},
{.name="get_var_info_from_attr",.f=get_var_info_from_attr},
{.name="set_type_integral",.f=set_type_integral},
{.name="ic_lwb",.f=ic_lwb},
{.name="p_ic_impose_min",.f=p_ic_impose_min},
{.name="ic_upb",.f=ic_upb},
{.name="p_ic_impose_max",.f=p_ic_impose_max},
{.name="p_ic_impose_bounds",.f=p_ic_impose_bounds},
{.name="make_constrained_ic_var",.f=make_constrained_ic_var},
{.name="p_make_bool",.f=p_make_bool},
{.name="set_integral",.f=set_integral},
{.name="p_set_var_integer",.f=p_set_var_integer},
{.name="set_up_exclude_delayed_goal",.f=set_up_exclude_delayed_goal},
{.name="ic_exclude",.f=ic_exclude},
{.name="p_ic_exclude",.f=p_ic_exclude},
{.name="set_up_exclude_range_delayed_goal",.f=set_up_exclude_range_delayed_goal},
{.name="ic_exclude_range",.f=ic_exclude_range},
{.name="p_ic_exclude_range",.f=p_ic_exclude_range},
{.name="impose_coef_bounds",.f=impose_coef_bounds},
{.name="solve_equal",.f=solve_equal},
{.name="solve_not_equal",.f=solve_not_equal},
{.name="update_con_data_buf",.f=update_con_data_buf},
{.name="con_struct_vec_to_con_info",.f=con_struct_vec_to_con_info},
{.name="finish_setting_up_con_struct",.f=finish_setting_up_con_struct},
{.name="swap_entries",.f=swap_entries},
{.name="update_ef",.f=update_ef},
{.name="evaluate_reified",.f=evaluate_reified},
{.name="check_ic_0v_con",.f=check_ic_0v_con},
{.name="prop_ic_1v_con",.f=prop_ic_1v_con},
{.name="prop_pass_1",.f=prop_pass_1},
{.name="prop_pass_2",.f=prop_pass_2},
{.name="prop_ic_neq_con",.f=prop_ic_neq_con},
{.name="short_cuts",.f=short_cuts},
{.name="sync_attr_with_new_bitmap",.f=sync_attr_with_new_bitmap},
{.name="p_ac_eq_init",.f=p_ac_eq_init},
{.name="p_ac_eq_prop",.f=p_ac_eq_prop},
{.name="p_prop_ic_con",.f=p_prop_ic_con},
{.name="type_check_lin_terms",.f=type_check_lin_terms},
{.name="setup_pass_1",.f=setup_pass_1},
{.name="process_lin_terms_to_vectors_and_prop",.f=process_lin_terms_to_vectors_and_prop},
{.name="setup_1v_con",.f=setup_1v_con},
{.name="p_set_up_ic_con",.f=p_set_up_ic_con},
{.name="p_get_print_info",.f=p_get_print_info},
{.name="p_ic_init",.f=p_ic_init},
{.name="p_ic_constraints_init",.f=p_ic_constraints_init},
{.name="p_get_ic_attr",.f=p_get_ic_attr},
{.name="unify_integer_bounds",.f=unify_integer_bounds},
{.name="p_get_bounds",.f=p_get_bounds},
{.name="p_get_integer_bounds1",.f=p_get_integer_bounds1},
{.name="p_get_domain_size",.f=p_get_domain_size},
{.name="unify_number_ic",.f=unify_number_ic},
{.name="unify_ic_ic",.f=unify_ic_ic},
{.name="p_unify_ic",.f=p_unify_ic},
{.name="p_indomain_init",.f=p_indomain_init},
{.name="p_indomain_try",.f=p_indomain_try},
{.name="bit_count",.f=bit_count},
{.name="lsb",.f=lsb},
{.name="msb",.f=msb},
{.name="p_create_bitmap",.f=p_create_bitmap},
{.name="create_bitmap",.f=create_bitmap},
{.name="p_set_bitmap_lwb",.f=p_set_bitmap_lwb},
{.name="set_bitmap_lwb",.f=set_bitmap_lwb},
{.name="p_set_bitmap_upb",.f=p_set_bitmap_upb},
{.name="set_bitmap_upb",.f=set_bitmap_upb},
{.name="p_remove_bitmap_element",.f=p_remove_bitmap_element},
{.name="remove_bitmap_element",.f=remove_bitmap_element},
{.name="p_remove_bitmap_range",.f=p_remove_bitmap_range},
{.name="remove_bitmap_range",.f=remove_bitmap_range},
{.name="p_bitmap_intersect_into",.f=p_bitmap_intersect_into},
{.name="bitmap_intersect_into",.f=bitmap_intersect_into},
{.name="p_bitmap_shifted_intersect_into",.f=p_bitmap_shifted_intersect_into},
{.name="bitmap_shifted_intersect_into",.f=bitmap_shifted_intersect_into},
{.name="p_bitmaps_have_non_empty_intersection",.f=p_bitmaps_have_non_empty_intersection},
{.name="bitmaps_have_non_empty_intersection",.f=bitmaps_have_non_empty_intersection},
{.name="p_bitmap_union",.f=p_bitmap_union},
{.name="bitmap_union",.f=bitmap_union},
{.name="p_copy_bitmap",.f=p_copy_bitmap},
{.name="copy_bitmap",.f=copy_bitmap},
{.name="p_copy_bitmap_shifted",.f=p_copy_bitmap_shifted},
{.name="copy_bitmap_shifted",.f=copy_bitmap_shifted},
{.name="p_bitmap_range",.f=p_bitmap_range},
{.name="bitmap_range",.f=bitmap_range},
{.name="p_get_bitmap_lwb",.f=p_get_bitmap_lwb},
{.name="get_bitmap_lwb",.f=get_bitmap_lwb},
{.name="p_get_bitmap_upb",.f=p_get_bitmap_upb},
{.name="get_bitmap_upb",.f=get_bitmap_upb},
{.name="p_next_greater_member",.f=p_next_greater_member},
{.name="next_greater_member",.f=next_greater_member},
{.name="p_next_smaller_member",.f=p_next_smaller_member},
{.name="next_smaller_member",.f=next_smaller_member},
{.name="p_next_greater_non_member",.f=p_next_greater_non_member},
{.name="next_greater_non_member",.f=next_greater_non_member},
{.name="p_next_smaller_non_member",.f=p_next_smaller_non_member},
{.name="next_smaller_non_member",.f=next_smaller_non_member},
{.name="p_bitmap_size",.f=p_bitmap_size},
{.name="bitmap_size",.f=bitmap_size},
{.name="p_bitmap_contains",.f=p_bitmap_contains},
{.name="bitmap_contains",.f=bitmap_contains},
{.name="p_bitmap_contains_range",.f=p_bitmap_contains_range},
{.name="bitmap_contains_range",.f=bitmap_contains_range},
{.name="p_compare_bitmaps",.f=p_compare_bitmaps},
{.name="compare_bitmaps",.f=compare_bitmaps},
{.name="ec_init_ef",.f=ec_init_ef},
{.name="ec_init_task",.f=ec_init_task},
{.name="ec_ef_disj",.f=ec_ef_disj},
{.name="ec_ef_cum",.f=ec_ef_cum},
{.name="ec_ef_quad",.f=ec_ef_quad},
{.name="ec_regcomp",.f=ec_regcomp},
{.name="ec_regmatch",.f=ec_regmatch},
{.name="ec_regmatch4",.f=ec_regmatch4},
{.name="ec_regmatchsub",.f=ec_regmatchsub},
{.name="ec_regmatchall",.f=ec_regmatchall},
{.name="ec_regsplit",.f=ec_regsplit},
};
