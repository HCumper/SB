#include "sbruntime_c.h"

DataValue sb_data[] = {  };
int sb_data_count = 0;
RestorePoint sb_restore_points[] = {  };
int sb_restore_point_count = 0;
int sb_data_pointer = 0;
char sb_input_buffer[4096];
char* sb_input_parts[256];
int sb_input_part_count = 0;

static Cell v34_A;
static Cell v35_B;
static Cell v37_DUMMY_;
static Cell v38_FINISHED;
static Cell v40_LO;
static Cell v41_N;
static Cell v42_NOW;
static Cell v43_PARTITIONS;
static Cell v50_SHORTLIST;
static Cell v51_SORTS;
static Cell v53_TEMP;

static Value r36_check(void);
static Value r49_set_up_tabLe(void);
static Value r44_print_table(void);
static Value r46_quicksort(ParamBinding v27_L_arg, ParamBinding v29_R_arg);
static Value r52_swap(ParamBinding v33_THIS_arg, ParamBinding v32_THAT_arg);
static Value r39_insertionsort(ParamBinding v18_FIRST_arg, ParamBinding v20_LAST_arg);
static Value r47_QUICKSORT1(ParamBinding v8_BOTTOM_arg, ParamBinding v13_TOP_arg);
static Value r48_QUICKSORT2(ParamBinding v14_LOWER_arg, ParamBinding v16_UPPER_arg);
static Value r45_QS2_PARTITION(ParamBinding v2_I_arg, ParamBinding v4_J_arg);

int main(void);

static Value r36_check(void)
{
    Value __result = make_null();
    Cell v17_LOOP = make_cell(make_null());
    DynamicBinding __frame_bindings[1];
    __frame_bindings[0].name = "LOOP";
    __frame_bindings[0].cell = &v17_LOOP;
    push_dynamic_frame(__frame_bindings, 1);
    stmt_0: ;
    stmt_1: ;
    stmt_2: ;
    stmt_3: ;
    {
        int loop_0_step = as_int(make_int(1));
        int loop_0_end = as_int(subtract_value((lookup_dynamic_cell("N"))->value, make_int(1)));
        int loop_0_index = as_int(make_int(1));
        for (; (loop_0_step >= 0) ? (loop_0_index <= loop_0_end) : (loop_0_index >= loop_0_end); loop_0_index += loop_0_step)
        {
            (&v17_LOOP)->value = make_int(loop_0_index);
            stmt_6: ;
            stmt_7: ;
            execute_builtin_statement("PRINT", make_null(), 1, get_array_value(lookup_dynamic_cell("B"), 1, (&v17_LOOP)->value));
            stmt_8: ;
            stmt_9: ;
            if (is_true(compare_greater_than(get_array_value(lookup_dynamic_cell("B"), 1, (&v17_LOOP)->value), get_array_value(lookup_dynamic_cell("B"), 1, add_value((&v17_LOOP)->value, make_int(1))))))
            {
                stmt_10: ;
                execute_builtin_statement("PRINT", make_null(), 2, make_string("Error at"), (&v17_LOOP)->value);
            }
            loop_0_next: ;
        }
        loop_0_exit: ;
    }
    stmt_4: ;
    stmt_5: ;
    execute_builtin_statement("PRINT", make_null(), 2, make_string("Time taken"), subtract_value(invoke_builtin_function("DATE", 0), (lookup_dynamic_cell("NOW"))->value));
sb_exit:
    pop_dynamic_frame();
    return __result;
}

static Value r49_set_up_tabLe(void)
{
    Value __result = make_null();
    Cell v30_ENTRY = make_cell(make_null());
    DynamicBinding __frame_bindings[1];
    __frame_bindings[0].name = "ENTRY";
    __frame_bindings[0].cell = &v30_ENTRY;
    push_dynamic_frame(__frame_bindings, 1);
    stmt_11: ;
    stmt_12: ;
    stmt_13: ;
    (lookup_dynamic_cell("N"))->value = make_int(100);
    stmt_14: ;
    /* REMark  INPUT \"How many numbers? \";n */
    stmt_15: ;
    stmt_16: ;
    {
        int loop_0_step = as_int(make_int(1));
        int loop_0_end = as_int((lookup_dynamic_cell("N"))->value);
        int loop_0_index = as_int(make_int(1));
        for (; (loop_0_step >= 0) ? (loop_0_index <= loop_0_end) : (loop_0_index >= loop_0_end); loop_0_index += loop_0_step)
        {
            (&v30_ENTRY)->value = make_int(loop_0_index);
            stmt_19: ;
            stmt_20: ;
            set_array_value(lookup_dynamic_cell("A"), invoke_builtin_function("RND", 2, make_int(1), make_int(10000)), 1, (&v30_ENTRY)->value);
            loop_0_next: ;
        }
        loop_0_exit: ;
    }
    stmt_17: ;
    stmt_18: ;
    (lookup_dynamic_cell("NOW"))->value = invoke_builtin_function("DATE", 0);
sb_exit:
    pop_dynamic_frame();
    return __result;
}

static Value r44_print_table(void)
{
    Value __result = make_null();
    Cell v23_POS = make_cell(make_null());
    DynamicBinding __frame_bindings[1];
    __frame_bindings[0].name = "POS";
    __frame_bindings[0].cell = &v23_POS;
    push_dynamic_frame(__frame_bindings, 1);
    stmt_21: ;
    stmt_22: ;
    stmt_23: ;
    (lookup_dynamic_cell("FINISHED"))->value = subtract_value(invoke_builtin_function("DATE", 0), (lookup_dynamic_cell("NOW"))->value);
    stmt_24: ;
    stmt_25: ;
    execute_builtin_statement("PRINT", make_null(), 0);
    stmt_26: ;
    execute_builtin_statement("PRINT", make_null(), 0);
    stmt_27: ;
    stmt_28: ;
    execute_builtin_statement("BEEP", make_null(), 2, make_int(10000), make_int(100));
    stmt_29: ;
    stmt_30: ;
    {
        int loop_0_step = as_int(make_int(1));
        int loop_0_end = as_int((lookup_dynamic_cell("N"))->value);
        int loop_0_index = as_int(subtract_value((lookup_dynamic_cell("N"))->value, make_int(4)));
        for (; (loop_0_step >= 0) ? (loop_0_index <= loop_0_end) : (loop_0_index >= loop_0_end); loop_0_index += loop_0_step)
        {
            (&v23_POS)->value = make_int(loop_0_index);
            stmt_33: ;
            stmt_34: ;
            execute_builtin_statement("PRINT", make_null(), 1, get_array_value(lookup_dynamic_cell("A"), 1, (&v23_POS)->value));
            loop_0_next: ;
        }
        loop_0_exit: ;
    }
    stmt_31: ;
    stmt_32: ;
    execute_builtin_statement("PRINT", make_null(), 2, make_string("Time taken"), (lookup_dynamic_cell("FINISHED"))->value);
sb_exit:
    pop_dynamic_frame();
    return __result;
}

static Value r46_quicksort(ParamBinding v27_L_arg, ParamBinding v29_R_arg)
{
    Value __result = make_null();
    Cell v27_L_local = make_cell(make_null());
    Cell* v27_L = bind_parameter(&v27_L_arg, &v27_L_local);
    Cell v29_R_local = make_cell(make_null());
    Cell* v29_R = bind_parameter(&v29_R_arg, &v29_R_local);
    Cell v24_EXCHANGE = make_cell(make_null());
    Cell v25_I = make_cell(make_null());
    Cell v26_J = make_cell(make_null());
    Cell v28_PIVOT = make_cell(make_null());
    DynamicBinding __frame_bindings[6];
    __frame_bindings[0].name = "L";
    __frame_bindings[0].cell = v27_L;
    __frame_bindings[1].name = "R";
    __frame_bindings[1].cell = v29_R;
    __frame_bindings[2].name = "EXCHANGE";
    __frame_bindings[2].cell = &v24_EXCHANGE;
    __frame_bindings[3].name = "I";
    __frame_bindings[3].cell = &v25_I;
    __frame_bindings[4].name = "J";
    __frame_bindings[4].cell = &v26_J;
    __frame_bindings[5].name = "PIVOT";
    __frame_bindings[5].cell = &v28_PIVOT;
    push_dynamic_frame(__frame_bindings, 6);
    stmt_35: ;
    stmt_36: ;
    stmt_37: ;
    (&v25_I)->value = (v27_L)->value;
    stmt_38: ;
    (&v26_J)->value = (v29_R)->value;
    stmt_39: ;
    stmt_40: ;
    /* REMark IF b(i)<b(i+1) : swap b(i),b(i+1) */
    stmt_41: ;
    stmt_42: ;
    /* REMark IF b(i+1)>b(j) : swap b(i+1),b(j) */
    stmt_43: ;
    stmt_44: ;
    /* REMark IF b(i)>b(i+1) : swap b(i),b(i+1) */
    stmt_45: ;
    stmt_46: ;
    (&v28_PIVOT)->value = get_array_value(lookup_dynamic_cell("B"), 1, (&v25_I)->value);
    stmt_47: ;
    stmt_48: ;
    while (1)
    {
        stmt_53: ;
        stmt_54: ;
        while (1)
        {
            stmt_63: ;
            stmt_64: ;
            if (is_true(compare_less_than_or_equal((&v28_PIVOT)->value, get_array_value(lookup_dynamic_cell("B"), 1, (&v25_I)->value))))
            {
                stmt_67: ;
                goto loop_1_exit;
            }
            stmt_65: ;
            stmt_66: ;
            (&v25_I)->value = add_value((&v25_I)->value, make_int(1));
            loop_1_next: ;
        }
        loop_1_exit: ;
        stmt_55: ;
        stmt_56: ;
        while (1)
        {
            stmt_68: ;
            stmt_69: ;
            if (is_true(compare_greater_than_or_equal((&v28_PIVOT)->value, get_array_value(lookup_dynamic_cell("B"), 1, (&v26_J)->value))))
            {
                stmt_72: ;
                goto loop_2_exit;
            }
            stmt_70: ;
            stmt_71: ;
            (&v26_J)->value = subtract_value((&v26_J)->value, make_int(1));
            loop_2_next: ;
        }
        loop_2_exit: ;
        stmt_57: ;
        stmt_58: ;
        if (is_true(compare_less_than((&v25_I)->value, (&v26_J)->value)))
        {
            stmt_73: ;
            (&v24_EXCHANGE)->value = get_array_value(lookup_dynamic_cell("B"), 1, (&v25_I)->value);
            stmt_74: ;
            set_array_value(lookup_dynamic_cell("B"), get_array_value(lookup_dynamic_cell("B"), 1, (&v26_J)->value), 1, (&v25_I)->value);
            stmt_75: ;
            set_array_value(lookup_dynamic_cell("B"), (&v24_EXCHANGE)->value, 1, (&v26_J)->value);
        }
        stmt_59: ;
        stmt_60: ;
        if (is_true(compare_less_than_or_equal((&v25_I)->value, (&v26_J)->value)))
        {
            stmt_76: ;
            (&v25_I)->value = add_value((&v25_I)->value, make_int(1));
            stmt_77: ;
            (&v26_J)->value = subtract_value((&v26_J)->value, make_int(1));
        }
        stmt_61: ;
        stmt_62: ;
        if (is_true(compare_greater_than((&v25_I)->value, (&v26_J)->value)))
        {
            stmt_78: ;
            goto loop_0_exit;
        }
        loop_0_next: ;
    }
    loop_0_exit: ;
    stmt_49: ;
    stmt_50: ;
    if (is_true(compare_less_than((v27_L)->value, (&v26_J)->value)))
    {
        stmt_79: ;
        stmt_80: ;
        if (is_true(compare_greater_than_or_equal(subtract_value((&v26_J)->value, (v27_L)->value), (lookup_dynamic_cell("SHORTLIST"))->value)))
        {
            stmt_81: ;
            stmt_82: ;
            r46_quicksort(make_ref_arg(v27_L), make_ref_arg(&v26_J));
        }
        else
        {
            stmt_83: ;
            stmt_84: ;
            r39_insertionsort(make_ref_arg(v27_L), make_ref_arg(&v26_J));
        }
    }
    stmt_51: ;
    stmt_52: ;
    if (is_true(compare_less_than((&v25_I)->value, (v29_R)->value)))
    {
        stmt_85: ;
        stmt_86: ;
        if (is_true(compare_greater_than_or_equal(subtract_value((v29_R)->value, make_int(1)), (lookup_dynamic_cell("SHORTLIST"))->value)))
        {
            stmt_87: ;
            stmt_88: ;
            r46_quicksort(make_ref_arg(&v25_I), make_ref_arg(v29_R));
        }
        else
        {
            stmt_89: ;
            stmt_90: ;
            r39_insertionsort(make_ref_arg(&v25_I), make_ref_arg(v29_R));
        }
    }
sb_exit:
    pop_dynamic_frame();
    return __result;
}

static Value r52_swap(ParamBinding v33_THIS_arg, ParamBinding v32_THAT_arg)
{
    Value __result = make_null();
    Cell v33_THIS_local = make_cell(make_null());
    Cell* v33_THIS = bind_parameter(&v33_THIS_arg, &v33_THIS_local);
    Cell v32_THAT_local = make_cell(make_null());
    Cell* v32_THAT = bind_parameter(&v32_THAT_arg, &v32_THAT_local);
    Cell v31_T = make_cell(make_null());
    DynamicBinding __frame_bindings[3];
    __frame_bindings[0].name = "THIS";
    __frame_bindings[0].cell = v33_THIS;
    __frame_bindings[1].name = "THAT";
    __frame_bindings[1].cell = v32_THAT;
    __frame_bindings[2].name = "T";
    __frame_bindings[2].cell = &v31_T;
    push_dynamic_frame(__frame_bindings, 3);
    stmt_91: ;
    stmt_92: ;
    stmt_93: ;
    (&v31_T)->value = (v33_THIS)->value;
    stmt_94: ;
    stmt_95: ;
    (v33_THIS)->value = (v32_THAT)->value;
    stmt_96: ;
    stmt_97: ;
    (v32_THAT)->value = (&v31_T)->value;
sb_exit:
    pop_dynamic_frame();
    return __result;
}

static Value r39_insertionsort(ParamBinding v18_FIRST_arg, ParamBinding v20_LAST_arg)
{
    Value __result = make_null();
    Cell v18_FIRST_local = make_cell(make_null());
    Cell* v18_FIRST = bind_parameter(&v18_FIRST_arg, &v18_FIRST_local);
    Cell v20_LAST_local = make_cell(make_null());
    Cell* v20_LAST = bind_parameter(&v20_LAST_arg, &v20_LAST_local);
    Cell v19_ITEM = make_cell(make_null());
    Cell v21_P = make_cell(make_null());
    Cell v22_TEMPORARY = make_cell(make_null());
    DynamicBinding __frame_bindings[5];
    __frame_bindings[0].name = "FIRST";
    __frame_bindings[0].cell = v18_FIRST;
    __frame_bindings[1].name = "LAST";
    __frame_bindings[1].cell = v20_LAST;
    __frame_bindings[2].name = "ITEM";
    __frame_bindings[2].cell = &v19_ITEM;
    __frame_bindings[3].name = "P";
    __frame_bindings[3].cell = &v21_P;
    __frame_bindings[4].name = "TEMPORARY";
    __frame_bindings[4].cell = &v22_TEMPORARY;
    push_dynamic_frame(__frame_bindings, 5);
    stmt_98: ;
    stmt_99: ;
    stmt_100: ;
    {
        int loop_0_step = as_int(make_int(1));
        int loop_0_end = as_int((v20_LAST)->value);
        int loop_0_index = as_int(add_value((v18_FIRST)->value, make_int(1)));
        for (; (loop_0_step >= 0) ? (loop_0_index <= loop_0_end) : (loop_0_index >= loop_0_end); loop_0_index += loop_0_step)
        {
            (&v19_ITEM)->value = make_int(loop_0_index);
            stmt_101: ;
            stmt_102: ;
            (&v21_P)->value = (&v19_ITEM)->value;
            stmt_103: ;
            stmt_104: ;
            (&v22_TEMPORARY)->value = get_array_value(lookup_dynamic_cell("B"), 1, (&v21_P)->value);
            stmt_105: ;
            stmt_106: ;
            while (1)
            {
                stmt_109: ;
                stmt_110: ;
                if (is_true(compare_greater_than_or_equal((&v22_TEMPORARY)->value, get_array_value(lookup_dynamic_cell("B"), 1, subtract_value((&v21_P)->value, make_int(1))))))
                {
                    stmt_117: ;
                    goto loop_1_exit;
                }
                stmt_111: ;
                stmt_112: ;
                if (is_true(compare_equal((&v21_P)->value, (v18_FIRST)->value)))
                {
                    stmt_118: ;
                    goto loop_1_exit;
                }
                stmt_113: ;
                stmt_114: ;
                set_array_value(lookup_dynamic_cell("B"), get_array_value(lookup_dynamic_cell("B"), 1, subtract_value((&v21_P)->value, make_int(1))), 1, (&v21_P)->value);
                stmt_115: ;
                stmt_116: ;
                (&v21_P)->value = subtract_value((&v21_P)->value, make_int(1));
                loop_1_next: ;
            }
            loop_1_exit: ;
            stmt_107: ;
            stmt_108: ;
            set_array_value(lookup_dynamic_cell("B"), (&v22_TEMPORARY)->value, 1, (&v21_P)->value);
            loop_0_next: ;
        }
        loop_0_exit: ;
    }
sb_exit:
    pop_dynamic_frame();
    return __result;
}

static Value r47_QUICKSORT1(ParamBinding v8_BOTTOM_arg, ParamBinding v13_TOP_arg)
{
    Value __result = make_null();
    Cell v8_BOTTOM_local = make_cell(make_null());
    Cell* v8_BOTTOM = bind_parameter(&v8_BOTTOM_arg, &v8_BOTTOM_local);
    Cell v13_TOP_local = make_cell(make_null());
    Cell* v13_TOP = bind_parameter(&v13_TOP_arg, &v13_TOP_local);
    Cell v9_HIGH = make_cell(make_null());
    Cell v10_LOW = make_cell(make_null());
    Cell v11_PTR = make_cell(make_null());
    Cell v12_SORT_LOOP = make_cell(make_null());
    DynamicBinding __frame_bindings[6];
    __frame_bindings[0].name = "BOTTOM";
    __frame_bindings[0].cell = v8_BOTTOM;
    __frame_bindings[1].name = "TOP";
    __frame_bindings[1].cell = v13_TOP;
    __frame_bindings[2].name = "HIGH";
    __frame_bindings[2].cell = &v9_HIGH;
    __frame_bindings[3].name = "LOW";
    __frame_bindings[3].cell = &v10_LOW;
    __frame_bindings[4].name = "PTR";
    __frame_bindings[4].cell = &v11_PTR;
    __frame_bindings[5].name = "SORT_LOOP";
    __frame_bindings[5].cell = &v12_SORT_LOOP;
    push_dynamic_frame(__frame_bindings, 6);
    stmt_119: ;
    stmt_120: ;
    stmt_121: ;
    (&v10_LOW)->value = (v8_BOTTOM)->value;
    stmt_122: ;
    (&v9_HIGH)->value = (v13_TOP)->value;
    stmt_123: ;
    (&v11_PTR)->value = (v8_BOTTOM)->value;
    stmt_124: ;
    stmt_125: ;
    while (1)
    {
        stmt_132: ;
        stmt_133: ;
        if (is_true(compare_greater_than_or_equal((&v10_LOW)->value, (&v9_HIGH)->value)))
        {
            stmt_136: ;
            goto loop_0_exit;
        }
        stmt_134: ;
        stmt_135: ;
        if (is_true(compare_greater_than(get_array_value(lookup_dynamic_cell("B"), 1, (&v10_LOW)->value), get_array_value(lookup_dynamic_cell("B"), 1, (&v9_HIGH)->value))))
        {
            stmt_137: ;
            stmt_138: ;
            (lookup_dynamic_cell("TEMP"))->value = get_array_value(lookup_dynamic_cell("B"), 1, (&v10_LOW)->value);
            stmt_139: ;
            stmt_140: ;
            set_array_value(lookup_dynamic_cell("B"), get_array_value(lookup_dynamic_cell("B"), 1, (&v9_HIGH)->value), 1, (&v10_LOW)->value);
            stmt_141: ;
            stmt_142: ;
            set_array_value(lookup_dynamic_cell("B"), (lookup_dynamic_cell("TEMP"))->value, 1, (&v9_HIGH)->value);
            stmt_143: ;
            stmt_144: ;
            if (is_true(compare_equal((&v11_PTR)->value, (&v10_LOW)->value)))
            {
                stmt_145: ;
                stmt_146: ;
                (&v10_LOW)->value = add_value((&v10_LOW)->value, make_int(1));
                stmt_147: ;
                (&v11_PTR)->value = (&v9_HIGH)->value;
            }
            else
            {
                stmt_148: ;
                stmt_149: ;
                (&v9_HIGH)->value = subtract_value((&v9_HIGH)->value, make_int(1));
                stmt_150: ;
                (&v11_PTR)->value = (&v10_LOW)->value;
            }
        }
        else
        {
            stmt_151: ;
            stmt_152: ;
            if (is_true(compare_equal((&v11_PTR)->value, (&v10_LOW)->value)))
            {
                stmt_153: ;
                (&v9_HIGH)->value = subtract_value((&v9_HIGH)->value, make_int(1));
            }
            else
            {
                stmt_154: ;
                (&v10_LOW)->value = add_value((&v10_LOW)->value, make_int(1));
            }
        }
        loop_0_next: ;
    }
    loop_0_exit: ;
    stmt_126: ;
    stmt_127: ;
    if (is_true(compare_less_than(invoke_builtin_function("ABS", 1, subtract_value((v13_TOP)->value, (v8_BOTTOM)->value)), make_int(2))))
    {
        stmt_155: ;
        __result = make_null();
        goto sb_exit;
    }
    stmt_128: ;
    stmt_129: ;
    r47_QUICKSORT1(make_ref_arg(v8_BOTTOM), make_value_arg(subtract_value((&v11_PTR)->value, make_int(1))));
    stmt_130: ;
    stmt_131: ;
    r47_QUICKSORT1(make_value_arg(add_value((&v11_PTR)->value, make_int(1))), make_ref_arg(v13_TOP));
sb_exit:
    pop_dynamic_frame();
    return __result;
}

static Value r48_QUICKSORT2(ParamBinding v14_LOWER_arg, ParamBinding v16_UPPER_arg)
{
    Value __result = make_null();
    Cell v14_LOWER_local = make_cell(make_null());
    Cell* v14_LOWER = bind_parameter(&v14_LOWER_arg, &v14_LOWER_local);
    Cell v16_UPPER_local = make_cell(make_null());
    Cell* v16_UPPER = bind_parameter(&v16_UPPER_arg, &v16_UPPER_local);
    Cell v15_PART_ELEM = make_cell(make_null());
    DynamicBinding __frame_bindings[3];
    __frame_bindings[0].name = "LOWER";
    __frame_bindings[0].cell = v14_LOWER;
    __frame_bindings[1].name = "UPPER";
    __frame_bindings[1].cell = v16_UPPER;
    __frame_bindings[2].name = "PART_ELEM";
    __frame_bindings[2].cell = &v15_PART_ELEM;
    push_dynamic_frame(__frame_bindings, 3);
    stmt_156: ;
    stmt_157: ;
    stmt_158: ;
    (lookup_dynamic_cell("SORTS"))->value = add_value((lookup_dynamic_cell("SORTS"))->value, make_int(1));
    stmt_159: ;
    stmt_160: ;
    if (is_true(compare_less_than((v14_LOWER)->value, (v16_UPPER)->value)))
    {
        stmt_161: ;
        stmt_162: ;
        (&v15_PART_ELEM)->value = add_value((v16_UPPER)->value, make_int(1));
        stmt_163: ;
        stmt_164: ;
        r45_QS2_PARTITION(make_ref_arg(v14_LOWER), make_ref_arg(&v15_PART_ELEM));
        stmt_165: ;
        stmt_166: ;
        r48_QUICKSORT2(make_ref_arg(v14_LOWER), make_value_arg(subtract_value((&v15_PART_ELEM)->value, make_int(1))));
        stmt_167: ;
        stmt_168: ;
        r48_QUICKSORT2(make_value_arg(add_value((&v15_PART_ELEM)->value, make_int(1))), make_ref_arg(v16_UPPER));
    }
sb_exit:
    pop_dynamic_frame();
    return __result;
}

static Value r45_QS2_PARTITION(ParamBinding v2_I_arg, ParamBinding v4_J_arg)
{
    Value __result = make_null();
    Cell v2_I_local = make_cell(make_null());
    Cell* v2_I = bind_parameter(&v2_I_arg, &v2_I_local);
    Cell v4_J_local = make_cell(make_null());
    Cell* v4_J = bind_parameter(&v4_J_arg, &v4_J_local);
    Cell v0_DEC_J = make_cell(make_null());
    Cell v1_EXCHANGE = make_cell(make_null());
    Cell v3_INC_K = make_cell(make_null());
    Cell v5_K = make_cell(make_null());
    Cell v6_LOOP = make_cell(make_null());
    Cell v7_TEMP = make_cell(make_null());
    DynamicBinding __frame_bindings[8];
    __frame_bindings[0].name = "I";
    __frame_bindings[0].cell = v2_I;
    __frame_bindings[1].name = "J";
    __frame_bindings[1].cell = v4_J;
    __frame_bindings[2].name = "DEC_J";
    __frame_bindings[2].cell = &v0_DEC_J;
    __frame_bindings[3].name = "EXCHANGE";
    __frame_bindings[3].cell = &v1_EXCHANGE;
    __frame_bindings[4].name = "INC_K";
    __frame_bindings[4].cell = &v3_INC_K;
    __frame_bindings[5].name = "K";
    __frame_bindings[5].cell = &v5_K;
    __frame_bindings[6].name = "LOOP";
    __frame_bindings[6].cell = &v6_LOOP;
    __frame_bindings[7].name = "TEMP";
    __frame_bindings[7].cell = &v7_TEMP;
    push_dynamic_frame(__frame_bindings, 8);
    stmt_169: ;
    stmt_170: ;
    stmt_171: ;
    (&v7_TEMP)->value = get_array_value(lookup_dynamic_cell("B"), 1, divide_value(add_value((v2_I)->value, (v4_J)->value), make_int(2)));
    stmt_172: ;
    (&v5_K)->value = (v2_I)->value;
    stmt_173: ;
    stmt_174: ;
    (lookup_dynamic_cell("PARTITIONS"))->value = add_value((lookup_dynamic_cell("PARTITIONS"))->value, make_int(1));
    stmt_175: ;
    stmt_176: ;
    while (1)
    {
        stmt_180: ;
        stmt_181: ;
        (lookup_dynamic_cell("PARTITIONS"))->value = add_value((lookup_dynamic_cell("PARTITIONS"))->value, make_int(1));
        stmt_182: ;
        stmt_183: ;
        while (1)
        {
            stmt_188: ;
            stmt_189: ;
            (&v5_K)->value = add_value((&v5_K)->value, make_int(1));
            stmt_190: ;
            stmt_191: ;
            /* REMark this is where the 'guaranteed highest value' comes in */
            stmt_192: ;
            stmt_193: ;
            if (is_true(compare_greater_than_or_equal(get_array_value(lookup_dynamic_cell("B"), 1, (&v5_K)->value), (&v7_TEMP)->value)))
            {
                stmt_194: ;
                goto loop_1_exit;
            }
            loop_1_next: ;
        }
        loop_1_exit: ;
        stmt_184: ;
        stmt_185: ;
        while (1)
        {
            stmt_195: ;
            stmt_196: ;
            (v4_J)->value = subtract_value((v4_J)->value, make_int(1));
            stmt_197: ;
            stmt_198: ;
            /* REMark conversely we don't need a 'guaranteed less than' value */
            stmt_199: ;
            stmt_200: ;
            /* REMark at the start of the array as the sorting process does */
            stmt_201: ;
            stmt_202: ;
            /* REMark this for us anyway */
            stmt_203: ;
            stmt_204: ;
            if (is_true(compare_less_than_or_equal(get_array_value(lookup_dynamic_cell("B"), 1, (v4_J)->value), (&v7_TEMP)->value)))
            {
                stmt_205: ;
                goto loop_2_exit;
            }
            loop_2_next: ;
        }
        loop_2_exit: ;
        stmt_186: ;
        stmt_187: ;
        if (is_true(compare_less_than((&v5_K)->value, (v4_J)->value)))
        {
            stmt_206: ;
            stmt_207: ;
            /* REMark swap the two entries around */
            stmt_208: ;
            stmt_209: ;
            (&v1_EXCHANGE)->value = get_array_value(lookup_dynamic_cell("B"), 1, (&v5_K)->value);
            stmt_210: ;
            set_array_value(lookup_dynamic_cell("B"), get_array_value(lookup_dynamic_cell("B"), 1, (v4_J)->value), 1, (&v5_K)->value);
            stmt_211: ;
            set_array_value(lookup_dynamic_cell("B"), (&v1_EXCHANGE)->value, 1, (v4_J)->value);
        }
        else
        {
            stmt_212: ;
            stmt_213: ;
            goto loop_0_exit;
        }
        loop_0_next: ;
    }
    loop_0_exit: ;
    stmt_177: ;
    stmt_178: ;
    set_array_value(lookup_dynamic_cell("B"), get_array_value(lookup_dynamic_cell("B"), 1, (v4_J)->value), 1, (v2_I)->value);
    stmt_179: ;
    set_array_value(lookup_dynamic_cell("B"), (&v7_TEMP)->value, 1, (v4_J)->value);
sb_exit:
    pop_dynamic_frame();
    return __result;
}

int main(void)
{
    sb_runtime_init();
    v34_A = make_cell(make_array());
    register_array_dimensions(&v34_A, 2, 10001, 17);
    register_global("A", &v34_A);
    v35_B = make_cell(make_array());
    register_array_dimensions(&v35_B, 1, 10001);
    register_global("B", &v35_B);
    v37_DUMMY_ = make_cell(make_null());
    register_global("DUMMY$", &v37_DUMMY_);
    v38_FINISHED = make_cell(make_null());
    register_global("FINISHED", &v38_FINISHED);
    v40_LO = make_cell(make_null());
    register_global("LO", &v40_LO);
    v41_N = make_cell(make_null());
    register_global("N", &v41_N);
    v42_NOW = make_cell(make_null());
    register_global("NOW", &v42_NOW);
    v43_PARTITIONS = make_cell(make_null());
    register_global("PARTITIONS", &v43_PARTITIONS);
    v50_SHORTLIST = make_cell(make_null());
    register_global("SHORTLIST", &v50_SHORTLIST);
    v51_SORTS = make_cell(make_null());
    register_global("SORTS", &v51_SORTS);
    v53_TEMP = make_cell(make_null());
    register_global("TEMP", &v53_TEMP);
    stmt_214: ;
    stmt_215: ;
    /* REMark CLS */
    stmt_216: ;
    stmt_217: ;
    execute_builtin_statement("PRINT", make_null(), 1, make_string("Thinkin"));
    stmt_218: ;
    stmt_219: ;
    /* TURBO_repfil */
    stmt_220: ;
    stmt_221: ;
    (&v51_SORTS)->value = make_int(0);
    stmt_222: ;
    (&v43_PARTITIONS)->value = make_int(0);
    stmt_223: ;
    stmt_224: ;
    stmt_225: ;
    stmt_226: ;
    stmt_227: ;
    (&v50_SHORTLIST)->value = make_int(9);
    stmt_228: ;
    stmt_229: ;
    r49_set_up_tabLe();
    stmt_230: ;
    stmt_231: ;
    {
        int loop_0_step = as_int(make_int(1));
        int loop_0_end = as_int((&v41_N)->value);
        int loop_0_index = as_int(make_int(1));
        for (; (loop_0_step >= 0) ? (loop_0_index <= loop_0_end) : (loop_0_index >= loop_0_end); loop_0_index += loop_0_step)
        {
            (&v40_LO)->value = make_int(loop_0_index);
            stmt_282: ;
            set_array_value(&v35_B, get_array_value(&v34_A, 1, (&v40_LO)->value), 1, (&v40_LO)->value);
            loop_0_next: ;
        }
        loop_0_exit: ;
    }
    stmt_232: ;
    stmt_233: ;
    (&v42_NOW)->value = invoke_builtin_function("DATE", 0);
    stmt_234: ;
    stmt_235: ;
    set_array_value(&v35_B, make_int(10001), 1, add_value((&v41_N)->value, make_int(1)));
    stmt_236: ;
    stmt_237: ;
    r46_quicksort(make_value_arg(make_int(1)), make_ref_arg(&v41_N));
    stmt_238: ;
    stmt_239: ;
    r36_check();
    stmt_240: ;
    stmt_241: ;
    execute_input(make_null(), 1, make_string("Press Enter to continue"));
    (&v37_DUMMY_)->value = read_input_value(0, TYPE_STRING);
    stmt_242: ;
    stmt_243: ;
    goto sb_exit;
    stmt_244: ;
    stmt_245: ;
    {
        int loop_1_step = as_int(make_int(1));
        int loop_1_end = as_int((&v41_N)->value);
        int loop_1_index = as_int(make_int(1));
        for (; (loop_1_step >= 0) ? (loop_1_index <= loop_1_end) : (loop_1_index >= loop_1_end); loop_1_index += loop_1_step)
        {
            (&v40_LO)->value = make_int(loop_1_index);
            stmt_283: ;
            set_array_value(&v35_B, get_array_value(&v34_A, 1, (&v40_LO)->value), 1, (&v40_LO)->value);
            loop_1_next: ;
        }
        loop_1_exit: ;
    }
    stmt_246: ;
    stmt_247: ;
    /* REMark b(n+1)=10001 */
    stmt_248: ;
    stmt_249: ;
    (&v51_SORTS)->value = make_int(0);
    stmt_250: ;
    (&v43_PARTITIONS)->value = make_int(0);
    stmt_251: ;
    stmt_252: ;
    (&v42_NOW)->value = invoke_builtin_function("DATE", 0);
    stmt_253: ;
    stmt_254: ;
    r48_QUICKSORT2(make_value_arg(make_int(1)), make_ref_arg(&v41_N));
    stmt_255: ;
    stmt_256: ;
    execute_builtin_statement("PRINT", make_null(), 3, (&v51_SORTS)->value, make_string("PRINT_COMMA"), (&v43_PARTITIONS)->value);
    stmt_257: ;
    stmt_258: ;
    r36_check();
    stmt_259: ;
    stmt_260: ;
    goto sb_exit;
    stmt_261: ;
    stmt_262: ;
    stmt_263: ;
    stmt_264: ;
    stmt_265: ;
    stmt_266: ;
    stmt_267: ;
    stmt_268: ;
    stmt_269: ;
    stmt_270: ;
    stmt_271: ;
    stmt_272: ;
    stmt_273: ;
    /* REMark Quicksort, faster variety, by Marcus Jeffery */
    stmt_274: ;
    stmt_275: ;
    /* REMark prepare an array to sort, remembering that this routine needs */
    stmt_276: ;
    stmt_277: ;
    /* REMark a predefined 'highest value entry' at the end of the array */
    stmt_278: ;
    stmt_279: ;
    /* REMark QUICKSORT2 also needs the procedure QS2_PARTITION below */
    stmt_280: ;
    stmt_281: ;
sb_exit:
    return 0;
}
