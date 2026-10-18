#include "sbruntime_c.h"

DataValue sb_data[] = { { TYPE_STRING, 0, 0.0, "N.Faldo   ", NULL }, { TYPE_STRING, 0, 0.0, "T.Watson  ", NULL }, { TYPE_STRING, 0, 0.0, "T.Kite    ", NULL }, { TYPE_STRING, 0, 0.0, "Balesteros", NULL }, { TYPE_STRING, 0, 0.0, "G.Norman", NULL } };
int sb_data_count = 5;
RestorePoint sb_restore_points[] = { { 4090, 0 } };
int sb_restore_point_count = 1;
int sb_data_pointer = 0;
char sb_input_buffer[4096];
char* sb_input_parts[256];
int sb_input_part_count = 0;

static Cell v18_A;
static Cell v19_AA;
static Cell v20_ADD;
static Cell v21_AN_;
static Cell v23_ANGLE;
static Cell v24_B;
static Cell v26_BALLX;
static Cell v27_BALLY;
static Cell v28_BIT;
static Cell v29_BITT;
static Cell v30_BLOCKX;
static Cell v31_BLOCKY;
static Cell v32_BM;
static Cell v35_C;
static Cell v36_C1;
static Cell v37_C2;
static Cell v38_C3;
static Cell v39_C4;
static Cell v40_COLOR;
static Cell v41_COLOR1;
static Cell v42_COURSE;
static Cell v43_CSE;
static Cell v44_D_;
static Cell v47_DIF;
static Cell v48_DIST;
static Cell v49_E;
static Cell v51_FINISH;
static Cell v52_FINISHED;
static Cell v55_GRN;
static Cell v56_HAZARD;
static Cell v57_HOLE;
static Cell v58_HOLED;
static Cell v59_HOLES;
static Cell v60_HOLEX;
static Cell v61_HOLEY;
static Cell v65_JI;
static Cell v67_LENGTH;
static Cell v70_MAX;
static Cell v71_NAME_;
static Cell v72_NEWCOL;
static Cell v73_NEWCOL1;
static Cell v74_OK;
static Cell v75_PAR;
static Cell v76_PARS;
static Cell v77_PLAYER_;
static Cell v78_POINTER_;
static Cell v80_POWER;
static Cell v82_PUTX;
static Cell v83_PUTY;
static Cell v84_R;
static Cell v85_RB;
static Cell v87_SC1;
static Cell v88_SCORE;
static Cell v91_SHOOTS;
static Cell v92_SHOTS;
static Cell v93_START;
static Cell v96_TOT;
static Cell v97_TOTPAR;
static Cell v101_X;
static Cell v102_X_;
static Cell v103_XZ;
static Cell v104_Y;
static Cell v105_ZX;

static Value r79_pow _P_(( void ));
static Value r22_ang _P_(( void ));
static Value r90_shoot _P_(( void ));
static Value r25_ball _P_(( void ));
static Value r53_getcol _P_(( ParamBinding v5_X_arg, ParamBinding v6_Y_arg ));
static Value r50_fairway _P_(( void ));
static Value r54_green _P_(( void ));
static Value r99_water _P_(( void ));
static Value r66_lake _P_(( void ));
static Value r98_trees _P_(( ParamBinding v16_EE_arg ));
static Value r68_look _P_(( void ));
static Value r94_test _P_(( void ));
static Value r34_bunker _P_(( void ));
static Value r100_wet _P_(( void ));
static Value r86_rough _P_(( void ));
static Value r81_putt _P_(( void ));
static Value r33_branches _P_(( void ));
static Value r69_lost _P_(( void ));
static Value r62_hole_in_one _P_(( void ));
static Value r95_top _P_(( void ));
static Value r89_scorecard _P_(( void ));
static Value r46_datasave _P_(( void ));
static Value r45_dataload _P_(( void ));
static Value r63_initialise _P_(( void ));
static Value r64_intro _P_(( void ));

int main _P_(( void ));

static Value r79_pow(void)
{
    Value __result = make_null();
    Cell v11_P = make_cell(make_null());
    DynamicBinding __frame_bindings[1];
    __frame_bindings[0].name = "P";
    __frame_bindings[0].cell = &v11_P;
    push_dynamic_frame(__frame_bindings, 1);
    line_830: ;
    execute_builtin_statement("CLS", make_int(0), 0);
    execute_builtin_statement("INK", make_int(0), 1, make_int(7));
    execute_builtin_statement("PRINT", make_int(0), 1, make_string("                0        50       100"));
    execute_builtin_statement("AT", make_int(0), 2, make_int(0), make_int(0));
    execute_builtin_statement("PRINT", make_int(0), 1, make_string("Space Bar "));
    execute_builtin_statement("AT", make_int(0), 2, make_int(1), make_int(0));
    execute_builtin_statement("PRINT", make_int(0), 1, make_string("Selects"));
    {
        int loop_0_step = as_int(make_int(1));
        int loop_0_end = as_int(make_int(200));
        for (int loop_0_index = as_int(make_int(0)); (loop_0_step >= 0) ? (loop_0_index <= loop_0_end) : (loop_0_index >= loop_0_end); loop_0_index += loop_0_step)
        {
            (&v11_P)->value = make_int(loop_0_index);
            if (is_true(compare_equal(invoke_builtin_function("CODE", 1, invoke_builtin_function("INKEY$", 1, make_int(1))), make_int(32))))
            {
                goto line_910;
            }
            execute_builtin_statement("BLOCK", make_int(0), 5, make_int(2), make_int(5), add_value((&v11_P)->value, make_int(200)), make_int(20), make_int(2));
            loop_0_next: ;
        }
        loop_0_exit: ;
    }
    if (is_true(compare_not_equal(invoke_builtin_function("INKEY$", 1, make_int(30)), make_string(" "))))
    {
        goto line_830;
    }
    line_910: ;
    (lookup_dynamic_cell("POWER"))->value = invoke_builtin_function("INT", 1, divide_value((&v11_P)->value, make_int(2)));
    if (is_true(compare_equal((lookup_dynamic_cell("POWER"))->value, make_int(0))))
    {
        goto line_830;
    }
sb_exit:
    pop_dynamic_frame();
    return __result;
}

static Value r22_ang(void)
{
    Value __result = make_null();
    execute_builtin_statement("CLS", make_int(0), 0);
    execute_builtin_statement("PRINT", make_int(0), 1, make_string("angle (0-12)"));
    execute_builtin_statement("PRINT", make_int(0), 0);
    execute_builtin_statement("PRINT", make_int(0), 1, make_string("e.g 12 = >"));
    execute_builtin_statement("AT", make_int(0), 2, make_int(0), make_int(14));
    execute_input(make_int(0), 0);
    (lookup_dynamic_cell("ANGLE"))->value = read_input_value(0, TYPE_NULL);
    (lookup_dynamic_cell("ANGLE"))->value = divide_value(multiply_value((lookup_dynamic_cell("ANGLE"))->value, invoke_builtin_function("PI", 0)), make_int(6));
    if (is_true(compare_greater_than((lookup_dynamic_cell("POWER"))->value, (lookup_dynamic_cell("MAX"))->value)))
    {
        execute_builtin_statement("CLS", make_int(0), 0);
        execute_builtin_statement("PRINT", make_int(0), 1, make_string("You Have Fluffed It!"));
        (lookup_dynamic_cell("POWER"))->value = divide_value((lookup_dynamic_cell("POWER"))->value, make_int(4));
        (lookup_dynamic_cell("ANGLE"))->value = add_value((lookup_dynamic_cell("ANGLE"))->value, divide_value(invoke_builtin_function("RND", 1, slice_range_value(negate_value(make_int(20)), make_int(20))), make_int(100)));
        execute_builtin_statement("PAUSE", make_null(), 1, make_int(30));
        (lookup_dynamic_cell("MAX"))->value = make_int(100);
    }
sb_exit:
    return __result;
}

static Value r90_shoot(void)
{
    Value __result = make_null();
    (lookup_dynamic_cell("BALLX"))->value = invoke_builtin_function("INT", 1, add_value((lookup_dynamic_cell("BALLX"))->value, divide_value(multiply_value(multiply_value(invoke_builtin_function("SIN", 1, (lookup_dynamic_cell("ANGLE"))->value), make_int(525)), (lookup_dynamic_cell("POWER"))->value), (lookup_dynamic_cell("LENGTH"))->value)));
    (lookup_dynamic_cell("BALLY"))->value = invoke_builtin_function("INT", 1, subtract_value((lookup_dynamic_cell("BALLY"))->value, divide_value(multiply_value(multiply_value(invoke_builtin_function("COS", 1, (lookup_dynamic_cell("ANGLE"))->value), make_int(525)), (lookup_dynamic_cell("POWER"))->value), (lookup_dynamic_cell("LENGTH"))->value)));
    execute_builtin_statement("POKE_W", make_null(), 2, (lookup_dynamic_cell("ADD"))->value, (lookup_dynamic_cell("COLOR"))->value);
    execute_builtin_statement("POKE_W", make_null(), 2, add_value((lookup_dynamic_cell("ADD"))->value, make_int(128)), (lookup_dynamic_cell("COLOR1"))->value);
    if (is_true(bitwise_or_value(bitwise_or_value(bitwise_or_value(compare_less_than((lookup_dynamic_cell("BALLY"))->value, make_int(26)), compare_greater_than((lookup_dynamic_cell("BALLY"))->value, make_int(204))), compare_less_than((lookup_dynamic_cell("BALLX"))->value, make_int(16))), compare_greater_than((lookup_dynamic_cell("BALLX"))->value, make_int(238)))))
    {
        r69_lost();
    }
    r68_look();
    r25_ball();
sb_exit:
    return __result;
}

static Value r25_ball(void)
{
    Value __result = make_null();
    Cell v0_N = make_cell(make_null());
    DynamicBinding __frame_bindings[1];
    __frame_bindings[0].name = "N";
    __frame_bindings[0].cell = &v0_N;
    push_dynamic_frame(__frame_bindings, 1);
    (lookup_dynamic_cell("ADD"))->value = invoke_builtin_function("INT", 1, add_value(add_value(make_int(131072), multiply_value(make_int(128), (lookup_dynamic_cell("BALLY"))->value)), multiply_value(invoke_builtin_function("INT", 1, divide_value((lookup_dynamic_cell("BALLX"))->value, make_int(4))), make_int(2))));
    (lookup_dynamic_cell("COLOR"))->value = invoke_builtin_function("PEEK_W", 1, (lookup_dynamic_cell("ADD"))->value);
    (lookup_dynamic_cell("COLOR1"))->value = invoke_builtin_function("PEEK_W", 1, add_value((lookup_dynamic_cell("ADD"))->value, make_int(128)));
    (lookup_dynamic_cell("BIT"))->value = multiply_value(modulo_value((lookup_dynamic_cell("BALLX"))->value, make_int(4)), make_int(2));
    (lookup_dynamic_cell("BITT"))->value = subtract_value(make_int(255), multiply_value(power_value(make_int(2), subtract_value(make_int(7), (lookup_dynamic_cell("BIT"))->value)), make_float(1.875)));
    if (is_true(subtract_value((lookup_dynamic_cell("BITT"))->value, invoke_builtin_function("INT", 1, (lookup_dynamic_cell("BITT"))->value))))
    {
        (lookup_dynamic_cell("BITT"))->value = multiply_value((lookup_dynamic_cell("BITT"))->value, make_float(1.6000000000000001));
    }
    (lookup_dynamic_cell("NEWCOL"))->value = add_value(multiply_value(bitwise_and_value((lookup_dynamic_cell("BITT"))->value, invoke_builtin_function("PEEK", 1, (lookup_dynamic_cell("ADD"))->value)), make_int(256)), bitwise_and_value((lookup_dynamic_cell("BITT"))->value, invoke_builtin_function("PEEK", 1, add_value((lookup_dynamic_cell("ADD"))->value, make_int(1)))));
    execute_builtin_statement("POKE_W", make_null(), 2, (lookup_dynamic_cell("ADD"))->value, (lookup_dynamic_cell("NEWCOL"))->value);
    (lookup_dynamic_cell("NEWCOL1"))->value = add_value(multiply_value(bitwise_and_value((lookup_dynamic_cell("BITT"))->value, invoke_builtin_function("PEEK", 1, add_value((lookup_dynamic_cell("ADD"))->value, make_int(128)))), make_int(256)), bitwise_and_value((lookup_dynamic_cell("BITT"))->value, invoke_builtin_function("PEEK", 1, add_value((lookup_dynamic_cell("ADD"))->value, make_int(129)))));
    execute_builtin_statement("POKE_W", make_null(), 2, add_value((lookup_dynamic_cell("ADD"))->value, make_int(128)), (lookup_dynamic_cell("NEWCOL1"))->value);
    {
        int loop_0_step = as_int(make_int(1));
        int loop_0_end = as_int(make_int(4));
        for (int loop_0_index = as_int(make_int(0)); (loop_0_step >= 0) ? (loop_0_index <= loop_0_end) : (loop_0_index >= loop_0_end); loop_0_index += loop_0_step)
        {
            (&v0_N)->value = make_int(loop_0_index);
            execute_builtin_statement("POKE_W", make_null(), 2, (lookup_dynamic_cell("ADD"))->value, (lookup_dynamic_cell("COLOR"))->value);
            execute_builtin_statement("POKE_W", make_null(), 2, add_value((lookup_dynamic_cell("ADD"))->value, make_int(128)), (lookup_dynamic_cell("COLOR1"))->value);
            execute_builtin_statement("PAUSE", make_null(), 1, make_int(10));
            execute_builtin_statement("POKE_W", make_null(), 2, (lookup_dynamic_cell("ADD"))->value, (lookup_dynamic_cell("NEWCOL"))->value);
            execute_builtin_statement("POKE_W", make_null(), 2, add_value((lookup_dynamic_cell("ADD"))->value, make_int(128)), (lookup_dynamic_cell("NEWCOL1"))->value);
            execute_builtin_statement("PAUSE", make_null(), 1, make_int(9));
            loop_0_next: ;
        }
        loop_0_exit: ;
    }
sb_exit:
    pop_dynamic_frame();
    return __result;
}

static Value r53_getcol(ParamBinding v5_X_arg, ParamBinding v6_Y_arg)
{
    Value __result = make_null();
    Cell v5_X_local = make_cell(make_null());
    Cell* v5_X = bind_parameter(&v5_X_arg, &v5_X_local);
    Cell v6_Y_local = make_cell(make_null());
    Cell* v6_Y = bind_parameter(&v6_Y_arg, &v6_Y_local);
    Cell v4_ADD = make_cell(make_null());
    DynamicBinding __frame_bindings[3];
    __frame_bindings[0].name = "X";
    __frame_bindings[0].cell = v5_X;
    __frame_bindings[1].name = "Y";
    __frame_bindings[1].cell = v6_Y;
    __frame_bindings[2].name = "ADD";
    __frame_bindings[2].cell = &v4_ADD;
    push_dynamic_frame(__frame_bindings, 3);
    (&v4_ADD)->value = invoke_builtin_function("INT", 1, add_value(add_value(make_int(131072), multiply_value(make_int(128), (v6_Y)->value)), multiply_value(invoke_builtin_function("INT", 1, divide_value((v5_X)->value, make_int(4))), make_int(2))));
    (lookup_dynamic_cell("BIT"))->value = multiply_value(modulo_value((v5_X)->value, make_int(4)), make_int(2));
    (lookup_dynamic_cell("BITT"))->value = power_value(make_int(2), subtract_value(make_int(7), (lookup_dynamic_cell("BIT"))->value));
    (lookup_dynamic_cell("GRN"))->value = multiply_value(make_int(4), compare_not_equal(bitwise_and_value((lookup_dynamic_cell("BITT"))->value, invoke_builtin_function("PEEK", 1, (&v4_ADD)->value)), make_int(0)));
    (lookup_dynamic_cell("BITT"))->value = multiply_value(power_value(make_int(2), subtract_value(make_int(7), (lookup_dynamic_cell("BIT"))->value)), make_float(1.5));
    (lookup_dynamic_cell("RB"))->value = multiply_value(divide_value(bitwise_and_value((lookup_dynamic_cell("BITT"))->value, invoke_builtin_function("PEEK", 1, add_value((&v4_ADD)->value, make_int(1)))), (lookup_dynamic_cell("BITT"))->value), make_int(3));
    (lookup_dynamic_cell("RB"))->value = multiply_value(divide_value(bitwise_and_value((lookup_dynamic_cell("BITT"))->value, invoke_builtin_function("PEEK", 1, add_value((&v4_ADD)->value, make_int(1)))), (lookup_dynamic_cell("BITT"))->value), make_int(3));
    __result = add_value((lookup_dynamic_cell("GRN"))->value, (lookup_dynamic_cell("RB"))->value);
    goto sb_exit;
sb_exit:
    pop_dynamic_frame();
    return __result;
}

static Value r50_fairway(void)
{
    Value __result = make_null();
    Cell v3_F = make_cell(make_null());
    DynamicBinding __frame_bindings[1];
    __frame_bindings[0].name = "F";
    __frame_bindings[0].cell = &v3_F;
    push_dynamic_frame(__frame_bindings, 1);
    execute_builtin_statement("INK", make_null(), 1, make_int(4));
    (lookup_dynamic_cell("A"))->value = make_int(50);
    (lookup_dynamic_cell("B"))->value = make_int(25);
    (lookup_dynamic_cell("BLOCKX"))->value = make_int(5);
    (lookup_dynamic_cell("BLOCKY"))->value = add_value((lookup_dynamic_cell("B"))->value, make_int(90));
    execute_builtin_statement("FILL", make_null(), 1, make_int(0));
    {
        int loop_0_step = as_int(make_float(0.69999999999999996));
        int loop_0_end = as_int(make_int(170));
        for (int loop_0_index = as_int(make_int(0)); (loop_0_step >= 0) ? (loop_0_index <= loop_0_end) : (loop_0_index >= loop_0_end); loop_0_index += loop_0_step)
        {
            (&v3_F)->value = make_int(loop_0_index);
            (lookup_dynamic_cell("B"))->value = invoke_builtin_function("INT", 1, add_value(subtract_value((lookup_dynamic_cell("B"))->value, make_int(1)), invoke_builtin_function("RND", 1, slice_range_value(make_int(0), make_int(2)))));
            if (is_true(compare_equal(invoke_builtin_function("INT", 1, (&v3_F)->value), make_int(150))))
            {
                (lookup_dynamic_cell("C"))->value = (lookup_dynamic_cell("B"))->value;
                (lookup_dynamic_cell("E"))->value = (lookup_dynamic_cell("A"))->value;
            }
            if (is_true(bitwise_and_value(compare_greater_than((&v3_F)->value, make_int(70)), compare_less_than(invoke_builtin_function("RND", 1, slice_range_value(make_int(0), make_int(100))), make_int(2)))))
            {
                execute_builtin_statement("INK", make_null(), 1, make_int(6));
                execute_builtin_statement("FILL", make_null(), 1, make_int(1));
                execute_builtin_statement("ELLIPSE", make_null(), 5, subtract_value((&v3_F)->value, make_int(10)), subtract_value((lookup_dynamic_cell("A"))->value, invoke_builtin_function("RND", 1, slice_range_value(make_int(0), (lookup_dynamic_cell("B"))->value))), make_int(4), make_float(0.69999999999999996), invoke_builtin_function("RND", 1, slice_range_value(make_int(0), make_int(7))));
                execute_builtin_statement("INK", make_null(), 1, make_int(4));
                execute_builtin_statement("FILL", make_null(), 1, make_int(0));
            }
            if (is_true(compare_less_than((lookup_dynamic_cell("B"))->value, make_int(15))))
            {
                (lookup_dynamic_cell("B"))->value = make_int(15);
            }
            if (is_true(compare_greater_than((lookup_dynamic_cell("B"))->value, make_int(35))))
            {
                (lookup_dynamic_cell("B"))->value = make_int(35);
            }
            if (is_true(compare_less_than(subtract_value((lookup_dynamic_cell("A"))->value, (lookup_dynamic_cell("B"))->value), make_int(10))))
            {
                (lookup_dynamic_cell("A"))->value = add_value(make_int(10), (lookup_dynamic_cell("B"))->value);
            }
            if (is_true(compare_greater_than((lookup_dynamic_cell("A"))->value, make_int(90))))
            {
                (lookup_dynamic_cell("A"))->value = make_int(90);
            }
            execute_builtin_statement("LINE", make_null(), 4, (&v3_F)->value, (lookup_dynamic_cell("A"))->value, (&v3_F)->value, subtract_value((lookup_dynamic_cell("A"))->value, (lookup_dynamic_cell("B"))->value));
            (lookup_dynamic_cell("A"))->value = invoke_builtin_function("INT", 1, add_value(subtract_value((lookup_dynamic_cell("A"))->value, make_int(2)), invoke_builtin_function("RND", 1, slice_range_value(make_int(0), make_int(4)))));
            loop_0_next: ;
        }
        loop_0_exit: ;
    }
sb_exit:
    pop_dynamic_frame();
    return __result;
}

static Value r54_green(void)
{
    Value __result = make_null();
    Cell v7_G = make_cell(make_null());
    DynamicBinding __frame_bindings[1];
    __frame_bindings[0].name = "G";
    __frame_bindings[0].cell = &v7_G;
    push_dynamic_frame(__frame_bindings, 1);
    {
        int loop_0_step = as_int(make_int(1));
        int loop_0_end = as_int(make_int(5));
        for (int loop_0_index = as_int(make_int(1)); (loop_0_step >= 0) ? (loop_0_index <= loop_0_end) : (loop_0_index >= loop_0_end); loop_0_index += loop_0_step)
        {
            (&v7_G)->value = make_int(loop_0_index);
            execute_builtin_statement("FILL", make_null(), 1, make_int(1));
            execute_builtin_statement("INK", make_null(), 3, make_int(6), make_int(4), make_int(3));
            execute_builtin_statement("ELLIPSE", make_null(), 5, make_int(150), subtract_value((lookup_dynamic_cell("E"))->value, divide_value((lookup_dynamic_cell("C"))->value, make_int(2))), divide_value(subtract_value((lookup_dynamic_cell("C"))->value, (&v7_G)->value), make_int(2)), subtract_value(make_float(0.69999999999999996), invoke_builtin_function("RND", 1, slice_range_value(make_int(0), make_float(0.10000000000000001)))), invoke_builtin_function("RND", 1, slice_range_value(make_int(0), make_int(10))));
            if (is_true(compare_equal(divide_value((&v7_G)->value, make_int(2)), invoke_builtin_function("INT", 1, divide_value((&v7_G)->value, make_int(2))))))
            {
                goto loop_0_next;
            }
            execute_builtin_statement("INK", make_null(), 1, make_int(6));
            execute_builtin_statement("FILL", make_null(), 1, make_int(1));
            (lookup_dynamic_cell("X"))->value = subtract_value(invoke_builtin_function("RND", 1, slice_range_value(make_int(0), (lookup_dynamic_cell("C"))->value)), divide_value((lookup_dynamic_cell("C"))->value, make_int(2)));
            (lookup_dynamic_cell("Y"))->value = invoke_builtin_function("SQRT", 1, subtract_value(divide_value(power_value((lookup_dynamic_cell("C"))->value, make_int(2)), make_int(4)), power_value((lookup_dynamic_cell("X"))->value, make_int(2))));
            (lookup_dynamic_cell("AA"))->value = invoke_builtin_function("RND", 1, slice_range_value(make_int(0), make_int(1)));
            if (is_true(compare_less_than((lookup_dynamic_cell("AA"))->value, make_float(0.5))))
            {
                (lookup_dynamic_cell("Y"))->value = negate_value((lookup_dynamic_cell("Y"))->value);
            }
            execute_builtin_statement("ELLIPSE", make_null(), 5, add_value((lookup_dynamic_cell("X"))->value, make_int(150)), subtract_value(add_value((lookup_dynamic_cell("Y"))->value, (lookup_dynamic_cell("E"))->value), divide_value((lookup_dynamic_cell("C"))->value, make_int(2))), divide_value((lookup_dynamic_cell("C"))->value, make_int(5)), subtract_value(make_float(0.59999999999999998), invoke_builtin_function("RND", 1, slice_range_value(make_int(0), make_float(0.20000000000000001)))), invoke_builtin_function("RND", 1, slice_range_value(make_int(0), make_int(100))));
            loop_0_next: ;
        }
        loop_0_exit: ;
    }
    (lookup_dynamic_cell("ZX"))->value = invoke_builtin_function("RND", 1, slice_range_value(divide_value(negate_value((lookup_dynamic_cell("C"))->value), make_int(4)), divide_value((lookup_dynamic_cell("C"))->value, make_int(4))));
    (lookup_dynamic_cell("XZ"))->value = invoke_builtin_function("RND", 1, slice_range_value(divide_value(negate_value((lookup_dynamic_cell("C"))->value), make_int(4)), divide_value((lookup_dynamic_cell("C"))->value, make_int(4))));
    execute_builtin_statement("FILL", make_null(), 1, make_int(0));
    execute_builtin_statement("INK", make_null(), 1, make_int(0));
    execute_builtin_statement("CIRCLE", make_null(), 3, add_value(make_int(150), (lookup_dynamic_cell("ZX"))->value), add_value(subtract_value((lookup_dynamic_cell("E"))->value, divide_value((lookup_dynamic_cell("C"))->value, make_int(2))), (lookup_dynamic_cell("XZ"))->value), make_float(0.59999999999999998));
    execute_builtin_statement("INK", make_null(), 1, make_int(7));
    execute_builtin_statement("LINE", make_null(), 4, add_value(make_int(150), (lookup_dynamic_cell("ZX"))->value), add_value(subtract_value((lookup_dynamic_cell("E"))->value, divide_value((lookup_dynamic_cell("C"))->value, make_int(2))), (lookup_dynamic_cell("XZ"))->value), add_value(make_int(150), (lookup_dynamic_cell("ZX"))->value), add_value(add_value(subtract_value((lookup_dynamic_cell("E"))->value, divide_value((lookup_dynamic_cell("C"))->value, make_int(2))), divide_value((lookup_dynamic_cell("C"))->value, make_int(5))), (lookup_dynamic_cell("XZ"))->value));
    execute_builtin_statement("INK", make_null(), 1, make_int(2));
    execute_builtin_statement("LINE", make_null(), 4, add_value(make_int(150), (lookup_dynamic_cell("ZX"))->value), add_value(add_value(subtract_value((lookup_dynamic_cell("E"))->value, divide_value((lookup_dynamic_cell("C"))->value, make_int(2))), divide_value((lookup_dynamic_cell("C"))->value, make_int(5))), (lookup_dynamic_cell("XZ"))->value), add_value(make_int(152), (lookup_dynamic_cell("ZX"))->value), add_value(add_value(subtract_value((lookup_dynamic_cell("E"))->value, divide_value((lookup_dynamic_cell("C"))->value, make_int(2))), divide_value((lookup_dynamic_cell("C"))->value, make_int(6))), (lookup_dynamic_cell("XZ"))->value));
    execute_builtin_statement("LINE", make_null(), 4, add_value(make_int(152), (lookup_dynamic_cell("ZX"))->value), add_value(add_value(subtract_value((lookup_dynamic_cell("E"))->value, divide_value((lookup_dynamic_cell("C"))->value, make_int(2))), divide_value((lookup_dynamic_cell("C"))->value, make_int(6))), (lookup_dynamic_cell("XZ"))->value), add_value(make_int(150), (lookup_dynamic_cell("ZX"))->value), add_value(add_value(subtract_value((lookup_dynamic_cell("E"))->value, divide_value((lookup_dynamic_cell("C"))->value, make_int(2))), divide_value((lookup_dynamic_cell("C"))->value, make_int(6))), (lookup_dynamic_cell("XZ"))->value));
    (lookup_dynamic_cell("HOLEX"))->value = add_value(make_int(150), (lookup_dynamic_cell("ZX"))->value);
    (lookup_dynamic_cell("HOLEY"))->value = add_value(subtract_value((lookup_dynamic_cell("E"))->value, divide_value((lookup_dynamic_cell("C"))->value, make_int(2))), (lookup_dynamic_cell("XZ"))->value);
    (lookup_dynamic_cell("HOLEX"))->value = invoke_builtin_function("INT", 1, divide_value(add_value(multiply_value(divide_value((lookup_dynamic_cell("HOLEX"))->value, make_int(166)), make_int(448)), make_int(33)), make_int(2)));
    (lookup_dynamic_cell("HOLEY"))->value = invoke_builtin_function("INT", 1, add_value(multiply_value(subtract_value(make_int(100), (lookup_dynamic_cell("HOLEY"))->value), make_int(2)), make_float(16.5)));
    execute_builtin_statement("BLOCK", make_null(), 5, divide_value((lookup_dynamic_cell("C"))->value, make_float(2.5)), divide_value((lookup_dynamic_cell("C"))->value, make_float(2.5)), subtract_value((lookup_dynamic_cell("BLOCKX"))->value, make_int(3)), (lookup_dynamic_cell("BLOCKY"))->value, make_int(7));
    (lookup_dynamic_cell("BALLX"))->value = make_int(19);
    (lookup_dynamic_cell("BALLY"))->value = add_value(make_int(129), invoke_builtin_function("INT", 1, divide_value((lookup_dynamic_cell("C"))->value, make_int(4))));
    r25_ball();
sb_exit:
    pop_dynamic_frame();
    return __result;
}

static Value r99_water(void)
{
    Value __result = make_null();
    (lookup_dynamic_cell("Y"))->value = make_int(0);
    (lookup_dynamic_cell("X"))->value = invoke_builtin_function("RND", 1, slice_range_value(make_int(50), make_int(350)));
    line_1930: ;
    execute_builtin_statement("BLOCK", make_null(), 5, make_int(7), make_int(4), (lookup_dynamic_cell("X"))->value, (lookup_dynamic_cell("Y"))->value, make_int(5));
    (lookup_dynamic_cell("Y"))->value = add_value((lookup_dynamic_cell("Y"))->value, make_int(4));
    (lookup_dynamic_cell("X"))->value = add_value(subtract_value((lookup_dynamic_cell("X"))->value, make_int(3)), invoke_builtin_function("RND", 1, slice_range_value(make_int(0), make_int(7))));
    if (is_true(compare_equal((lookup_dynamic_cell("Y"))->value, make_int(200))))
    {
        goto line_1970;
    }
    goto line_1930;
sb_exit:
    return __result;
}

static Value r66_lake(void)
{
    Value __result = make_null();
    Cell v10_O = make_cell(make_null());
    DynamicBinding __frame_bindings[1];
    __frame_bindings[0].name = "O";
    __frame_bindings[0].cell = &v10_O;
    push_dynamic_frame(__frame_bindings, 1);
    if (is_true(compare_equal(invoke_builtin_function("RND", 1, slice_range_value(make_int(0), make_int(2))), make_int(1))))
    {
        __result = make_null();
        goto sb_exit;
    }
    execute_builtin_statement("FILL", make_null(), 1, make_int(1));
    execute_builtin_statement("INK", make_null(), 1, make_int(5));
    (lookup_dynamic_cell("X"))->value = invoke_builtin_function("RND", 1, slice_range_value(make_int(30), make_int(175)));
    (lookup_dynamic_cell("Y"))->value = invoke_builtin_function("RND", 1, slice_range_value(make_int(30), make_int(70)));
    {
        int loop_0_step = as_int(make_int(1));
        int loop_0_end = as_int(make_int(2));
        for (int loop_0_index = as_int(negate_value(make_int(2))); (loop_0_step >= 0) ? (loop_0_index <= loop_0_end) : (loop_0_index >= loop_0_end); loop_0_index += loop_0_step)
        {
            (&v10_O)->value = make_int(loop_0_index);
            (lookup_dynamic_cell("JI"))->value = add_value((lookup_dynamic_cell("Y"))->value, invoke_builtin_function("RND", 1, slice_range_value(negate_value(make_int(5)), make_int(5))));
            execute_builtin_statement("ELLIPSE", make_null(), 5, add_value((lookup_dynamic_cell("X"))->value, multiply_value(make_int(4), (&v10_O)->value)), (lookup_dynamic_cell("JI"))->value, make_int(20), make_float(0.65000000000000002), invoke_builtin_function("RND", 1, slice_range_value(make_int(1), make_int(7))));
            loop_0_next: ;
        }
        loop_0_exit: ;
    }
    execute_builtin_statement("FILL", make_null(), 1, make_int(0));
sb_exit:
    pop_dynamic_frame();
    return __result;
}

static Value r98_trees(ParamBinding v16_EE_arg)
{
    Value __result = make_null();
    Cell v16_EE_local = make_cell(make_null());
    Cell* v16_EE = bind_parameter(&v16_EE_arg, &v16_EE_local);
    Cell v17_F = make_cell(make_null());
    DynamicBinding __frame_bindings[2];
    __frame_bindings[0].name = "EE";
    __frame_bindings[0].cell = v16_EE;
    __frame_bindings[1].name = "F";
    __frame_bindings[1].cell = &v17_F;
    push_dynamic_frame(__frame_bindings, 2);
    execute_builtin_statement("INK", make_null(), 3, make_int(4), make_int(3), make_int(3));
    {
        int loop_0_step = as_int(make_int(1));
        int loop_0_end = as_int((v16_EE)->value);
        for (int loop_0_index = as_int(make_int(1)); (loop_0_step >= 0) ? (loop_0_index <= loop_0_end) : (loop_0_index >= loop_0_end); loop_0_index += loop_0_step)
        {
            (&v17_F)->value = make_int(loop_0_index);
            execute_builtin_statement("FILL", make_null(), 1, make_int(1));
            (lookup_dynamic_cell("OK"))->value = invoke_builtin_function("RND", 1, slice_range_value(make_int(15), make_int(75)));
            execute_builtin_statement("CIRCLE", make_null(), 3, invoke_builtin_function("RND", 1, slice_range_value(make_int(1), make_int(175))), (lookup_dynamic_cell("OK"))->value, add_value(make_int(1), divide_value(invoke_builtin_function("RND", 1, slice_range_value(make_int(0), make_int(10))), make_int(7))));
            execute_builtin_statement("FILL", make_null(), 1, make_int(0));
            loop_0_next: ;
        }
        loop_0_exit: ;
    }
sb_exit:
    pop_dynamic_frame();
    return __result;
}

static Value r68_look(void)
{
    Value __result = make_null();
    (lookup_dynamic_cell("C1"))->value = r53_getcol(make_value_arg((lookup_dynamic_cell("BALLX"))->value), make_value_arg((lookup_dynamic_cell("BALLY"))->value));
    (lookup_dynamic_cell("C2"))->value = r53_getcol(make_value_arg(add_value((lookup_dynamic_cell("BALLX"))->value, make_int(1))), make_value_arg((lookup_dynamic_cell("BALLY"))->value));
    (lookup_dynamic_cell("C3"))->value = r53_getcol(make_value_arg((lookup_dynamic_cell("BALLX"))->value), make_value_arg(add_value((lookup_dynamic_cell("BALLY"))->value, make_int(1))));
    (lookup_dynamic_cell("C4"))->value = r53_getcol(make_value_arg((lookup_dynamic_cell("BALLX"))->value), make_value_arg(add_value((lookup_dynamic_cell("BALLY"))->value, make_int(1))));
sb_exit:
    return __result;
}

static Value r94_test(void)
{
    Value __result = make_null();
    (lookup_dynamic_cell("HAZARD"))->value = make_int(0);
    (lookup_dynamic_cell("BM"))->value = invoke_builtin_function("ABS", 1, subtract_value((lookup_dynamic_cell("BALLX"))->value, (lookup_dynamic_cell("HOLEX"))->value));
    if (is_true(bitwise_and_value(compare_less_than_or_equal((lookup_dynamic_cell("BM"))->value, make_int(1)), compare_less_than_or_equal(invoke_builtin_function("ABS", 1, subtract_value((lookup_dynamic_cell("BALLY"))->value, (lookup_dynamic_cell("HOLEY"))->value)), make_int(1)))))
    {
        execute_builtin_statement("POKE_W", make_null(), 2, (lookup_dynamic_cell("ADD"))->value, (lookup_dynamic_cell("COLOR"))->value);
        execute_builtin_statement("POKE_W", make_null(), 2, add_value((lookup_dynamic_cell("ADD"))->value, make_int(128)), (lookup_dynamic_cell("COLOR1"))->value);
        (lookup_dynamic_cell("FINISHED"))->value = make_int(1);
        __result = make_null();
        goto sb_exit;
    }
    if (is_true(bitwise_or_value(bitwise_and_value(compare_equal((lookup_dynamic_cell("C1"))->value, make_int(6)), compare_equal((lookup_dynamic_cell("C2"))->value, make_int(6))), bitwise_and_value(compare_equal((lookup_dynamic_cell("C1"))->value, make_int(6)), compare_equal((lookup_dynamic_cell("C3"))->value, make_int(6))))))
    {
        r34_bunker();
    }
    if (is_true(compare_equal((lookup_dynamic_cell("C1"))->value, make_int(5))))
    {
        r100_wet();
    }
    if (is_true(bitwise_or_value(bitwise_and_value(compare_equal((lookup_dynamic_cell("C1"))->value, make_int(7)), compare_equal((lookup_dynamic_cell("C3"))->value, make_int(4))), bitwise_and_value(compare_equal((lookup_dynamic_cell("C1"))->value, make_int(4)), compare_equal((lookup_dynamic_cell("C3"))->value, make_int(7))))))
    {
        r86_rough();
    }
    if (is_true(bitwise_or_value(bitwise_and_value(compare_equal((lookup_dynamic_cell("C1"))->value, make_int(4)), compare_equal((lookup_dynamic_cell("C2"))->value, make_int(4))), bitwise_and_value(compare_equal((lookup_dynamic_cell("C1"))->value, make_int(4)), compare_equal((lookup_dynamic_cell("C3"))->value, make_int(4))))))
    {
        __result = make_null();
        goto sb_exit;
    }
    if (is_true(bitwise_or_value(compare_equal((lookup_dynamic_cell("C1"))->value, make_int(3)), compare_equal((lookup_dynamic_cell("C2"))->value, make_int(3)))))
    {
        r33_branches();
    }
    if (is_true(bitwise_and_value(compare_equal((lookup_dynamic_cell("C1"))->value, make_int(7)), compare_equal((lookup_dynamic_cell("C2"))->value, make_int(7)))))
    {
        __result = make_null();
        goto sb_exit;
    }
    r81_putt();
    (lookup_dynamic_cell("HAZARD"))->value = make_int(0);
    execute_builtin_statement("PAUSE", make_null(), 1, make_int(100));
    execute_builtin_statement("CLS", make_int(0), 0);
sb_exit:
    return __result;
}

static Value r34_bunker(void)
{
    Value __result = make_null();
    if (is_true((lookup_dynamic_cell("HAZARD"))->value))
    {
        __result = make_null();
        goto sb_exit;
    }
    (lookup_dynamic_cell("HAZARD"))->value = make_int(1);
    execute_builtin_statement("CLS", make_int(0), 0);
    execute_builtin_statement("PRINT", make_int(0), 0);
    execute_builtin_statement("PRINT", make_int(0), 1, make_string("   BUNKERED!"));
    (lookup_dynamic_cell("MAX"))->value = make_int(25);
sb_exit:
    return __result;
}

static Value r100_wet(void)
{
    Value __result = make_null();
    if (is_true((lookup_dynamic_cell("HAZARD"))->value))
    {
        __result = make_null();
        goto sb_exit;
    }
    (lookup_dynamic_cell("HAZARD"))->value = make_int(1);
    execute_builtin_statement("CLS", make_int(0), 0);
    execute_builtin_statement("PRINT", make_int(0), 1, make_string("WATER !!!. Back to tee"));
    (lookup_dynamic_cell("BALLX"))->value = make_int(19);
    (lookup_dynamic_cell("BALLY"))->value = add_value(make_int(129), invoke_builtin_function("INT", 1, divide_value((lookup_dynamic_cell("C"))->value, make_int(4))));
    execute_builtin_statement("POKE_W", make_null(), 2, (lookup_dynamic_cell("ADD"))->value, (lookup_dynamic_cell("COLOR"))->value);
    execute_builtin_statement("POKE_W", make_null(), 2, add_value((lookup_dynamic_cell("ADD"))->value, make_int(128)), (lookup_dynamic_cell("COLOR1"))->value);
    r25_ball();
sb_exit:
    return __result;
}

static Value r86_rough(void)
{
    Value __result = make_null();
    if (is_true((lookup_dynamic_cell("HAZARD"))->value))
    {
        __result = make_null();
        goto sb_exit;
    }
    (lookup_dynamic_cell("HAZARD"))->value = make_int(1);
    execute_builtin_statement("CLS", make_int(0), 0);
    execute_builtin_statement("PRINT", make_int(0), 1, make_string("You Are In The Rough"));
    (lookup_dynamic_cell("MAX"))->value = make_int(75);
    execute_builtin_statement("PAUSE", make_null(), 1, make_int(50));
sb_exit:
    return __result;
}

static Value r81_putt(void)
{
    Value __result = make_null();
    Cell v12_ANG1 = make_cell(make_null());
    Cell v13_PLOT = make_cell(make_null());
    DynamicBinding __frame_bindings[2];
    __frame_bindings[0].name = "ANG1";
    __frame_bindings[0].cell = &v12_ANG1;
    __frame_bindings[1].name = "PLOT";
    __frame_bindings[1].cell = &v13_PLOT;
    push_dynamic_frame(__frame_bindings, 2);
    while (1)
    {
        (lookup_dynamic_cell("DIST"))->value = invoke_builtin_function("SQRT", 1, add_value(power_value(subtract_value((lookup_dynamic_cell("HOLEY"))->value, (lookup_dynamic_cell("BALLY"))->value), make_int(2)), power_value(subtract_value((lookup_dynamic_cell("HOLEX"))->value, (lookup_dynamic_cell("BALLX"))->value), make_int(2))));
        (lookup_dynamic_cell("DIST"))->value = divide_value((lookup_dynamic_cell("DIST"))->value, divide_value(make_int(224), (lookup_dynamic_cell("LENGTH"))->value));
        (lookup_dynamic_cell("DIST"))->value = divide_value(invoke_builtin_function("INT", 1, multiply_value((lookup_dynamic_cell("DIST"))->value, make_int(100))), make_int(100));
        if (is_true(compare_greater_than((lookup_dynamic_cell("DIST"))->value, make_int(50))))
        {
            __result = make_null();
            goto sb_exit;
        }
        if (is_true((lookup_dynamic_cell("HAZARD"))->value))
        {
            __result = make_null();
            goto sb_exit;
        }
        (lookup_dynamic_cell("HAZARD"))->value = make_int(1);
        execute_builtin_statement("CLS", make_int(0), 0);
        execute_builtin_statement("PRINT", make_int(0), 3, make_string("You are "), (lookup_dynamic_cell("DIST"))->value, make_string(" yards from the hole"));
        execute_builtin_statement("PAUSE", make_null(), 1, make_int(100));
        execute_builtin_statement("BLOCK", make_int(0), 5, make_int(440), make_int(4), make_int(0), make_int(32), make_int(4));
        execute_builtin_statement("OVER", make_int(0), 1, negate_value(make_int(1)));
        (lookup_dynamic_cell("ANGLE"))->value = invoke_builtin_function("ATAN", 1, divide_value(invoke_builtin_function("ABS", 1, subtract_value((lookup_dynamic_cell("HOLEX"))->value, (lookup_dynamic_cell("BALLX"))->value)), invoke_builtin_function("ABS", 1, add_value(subtract_value((lookup_dynamic_cell("BALLY"))->value, (lookup_dynamic_cell("HOLEY"))->value), make_float(1.0000000000000001E-05)))));
        if (is_true(compare_greater_than((lookup_dynamic_cell("HOLEY"))->value, (lookup_dynamic_cell("BALLY"))->value)))
        {
            if (is_true(compare_less_than_or_equal((lookup_dynamic_cell("HOLEX"))->value, (lookup_dynamic_cell("BALLX"))->value)))
            {
                (lookup_dynamic_cell("ANGLE"))->value = add_value((lookup_dynamic_cell("ANGLE"))->value, invoke_builtin_function("PI", 0));
            }
            else
            {
                (lookup_dynamic_cell("ANGLE"))->value = subtract_value(invoke_builtin_function("PI", 0), (lookup_dynamic_cell("ANGLE"))->value);
            }
        }
        else
        {
            if (is_true(compare_less_than_or_equal((lookup_dynamic_cell("HOLEX"))->value, (lookup_dynamic_cell("BALLX"))->value)))
            {
                (lookup_dynamic_cell("ANGLE"))->value = subtract_value(multiply_value(make_int(2), invoke_builtin_function("PI", 0)), (lookup_dynamic_cell("ANGLE"))->value);
            }
        }
        (lookup_dynamic_cell("PUTX"))->value = make_int(31);
        (lookup_dynamic_cell("PUTY"))->value = make_int(24);
        execute_builtin_statement("CURSOR", make_int(0), 2, (lookup_dynamic_cell("PUTX"))->value, (lookup_dynamic_cell("PUTY"))->value);
        execute_builtin_statement("PRINT", make_int(0), 1, make_string("o"));
        line_2890: ;
        {
            int loop_1_step = as_int(make_int(2));
            int loop_1_end = as_int(make_int(60));
            for (int loop_1_index = as_int(make_int(0)); (loop_1_step >= 0) ? (loop_1_index <= loop_1_end) : (loop_1_index >= loop_1_end); loop_1_index += loop_1_step)
            {
                (&v12_ANG1)->value = make_int(loop_1_index);
                {
                    int loop_2_step = as_int(make_int(1));
                    int loop_2_end = as_int(make_int(1));
                    for (int loop_2_index = as_int(make_int(0)); (loop_2_step >= 0) ? (loop_2_index <= loop_2_end) : (loop_2_index >= loop_2_end); loop_2_index += loop_2_step)
                    {
                        (&v13_PLOT)->value = make_int(loop_2_index);
                        execute_builtin_statement("LINE", make_int(0), 4, make_int(60), make_int(72), subtract_value(make_int(62), multiply_value(make_int(62), invoke_builtin_function("SIN", 1, invoke_builtin_function("RAD", 1, (&v12_ANG1)->value)))), subtract_value(make_int(72), multiply_value(make_int(62), invoke_builtin_function("COS", 1, invoke_builtin_function("RAD", 1, (&v12_ANG1)->value)))));
                        loop_2_next: ;
                    }
                    loop_2_exit: ;
                }
                if (is_true(compare_equal(invoke_builtin_function("INKEY$", 0), make_string(" "))))
                {
                    goto line_2960;
                }
                loop_1_next: ;
            }
            loop_1_exit: ;
        }
        if (is_true(compare_not_equal(invoke_builtin_function("INKEY$", 1, make_int(10)), make_string(" "))))
        {
            goto line_2890;
        }
        line_2960: ;
        {
            int loop_3_step = as_int(negate_value(make_int(2)));
            int loop_3_end = as_int(make_int(0));
            for (int loop_3_index = as_int((&v12_ANG1)->value); (loop_3_step >= 0) ? (loop_3_index <= loop_3_end) : (loop_3_index >= loop_3_end); loop_3_index += loop_3_step)
            {
                (&v18_A)->value = make_int(loop_3_index);
                {
                    int loop_4_step = as_int(make_int(1));
                    int loop_4_end = as_int(make_int(1));
                    for (int loop_4_index = as_int(make_int(0)); (loop_4_step >= 0) ? (loop_4_index <= loop_4_end) : (loop_4_index >= loop_4_end); loop_4_index += loop_4_step)
                    {
                        (&v13_PLOT)->value = make_int(loop_4_index);
                        execute_builtin_statement("LINE", make_int(0), 4, make_int(60), make_int(72), subtract_value(make_int(62), multiply_value(make_int(62), invoke_builtin_function("SIN", 1, invoke_builtin_function("RAD", 1, (lookup_dynamic_cell("A"))->value)))), add_value(subtract_value(make_int(72), make_int(62)), invoke_builtin_function("COS", 1, invoke_builtin_function("RAD", 1, (lookup_dynamic_cell("A"))->value))));
                        loop_4_next: ;
                    }
                    loop_4_exit: ;
                }
                loop_3_next: ;
            }
            loop_3_exit: ;
        }
        execute_builtin_statement("CURSOR", make_int(0), 2, (lookup_dynamic_cell("PUTX"))->value, (lookup_dynamic_cell("PUTY"))->value);
        execute_builtin_statement("PRINT", make_int(0), 1, make_string("o"));
        (lookup_dynamic_cell("POWER"))->value = divide_value((&v12_ANG1)->value, make_int(7));
        r90_shoot();
        r94_test();
        (lookup_dynamic_cell("SHOTS"))->value = add_value((lookup_dynamic_cell("SHOTS"))->value, make_int(1));
        execute_builtin_statement("PAUSE", make_null(), 1, add_value(make_int(1), multiply_value(make_int(49), bitwise_not_value((lookup_dynamic_cell("FINISHED"))->value))));
        if (is_true((lookup_dynamic_cell("FINISHED"))->value))
        {
            goto loop_0_exit;
        }
        loop_0_next: ;
    }
    loop_0_exit: ;
    execute_builtin_statement("OVER", make_int(0), 1, make_int(0));
sb_exit:
    pop_dynamic_frame();
    return __result;
}

static Value r33_branches(void)
{
    Value __result = make_null();
    if (is_true((lookup_dynamic_cell("HAZARD"))->value))
    {
        __result = make_null();
        goto sb_exit;
    }
    execute_builtin_statement("CLS", make_int(0), 0);
    execute_builtin_statement("PRINT", make_int(0), 1, make_string("You Have Hit A Tree!"));
    (lookup_dynamic_cell("BALLX"))->value = subtract_value((lookup_dynamic_cell("BALLX"))->value, make_int(2));
    (lookup_dynamic_cell("BALLY"))->value = add_value((lookup_dynamic_cell("BALLY"))->value, invoke_builtin_function("RND", 1, slice_range_value(negate_value(make_int(1)), make_int(1))));
    execute_builtin_statement("POKE_W", make_null(), 2, add_value((lookup_dynamic_cell("ADD"))->value, make_int(128)), (lookup_dynamic_cell("COLOR1"))->value);
    r68_look();
    r25_ball();
    r94_test();
    (lookup_dynamic_cell("HAZARD"))->value = make_int(1);
sb_exit:
    return __result;
}

static Value r69_lost(void)
{
    Value __result = make_null();
    execute_builtin_statement("CLS", make_int(0), 0);
    execute_builtin_statement("PRINT", make_int(0), 1, make_string("Ball Out Of Bounds"));
    execute_builtin_statement("PRINT", make_int(0), 1, make_string("Penalty Shot"));
    if (is_true(compare_less_than((lookup_dynamic_cell("BALLX"))->value, make_int(16))))
    {
        (lookup_dynamic_cell("BALLX"))->value = make_int(16);
    }
    if (is_true(compare_greater_than((lookup_dynamic_cell("BALLX"))->value, make_int(238))))
    {
        (lookup_dynamic_cell("BALLX"))->value = make_int(238);
    }
    if (is_true(compare_greater_than((lookup_dynamic_cell("BALLY"))->value, make_int(204))))
    {
        (lookup_dynamic_cell("BALLY"))->value = make_int(204);
    }
    if (is_true(compare_less_than((lookup_dynamic_cell("BALLY"))->value, make_int(26))))
    {
        (lookup_dynamic_cell("BALLY"))->value = make_int(26);
    }
    (lookup_dynamic_cell("SHOTS"))->value = add_value((lookup_dynamic_cell("SHOTS"))->value, make_int(1));
    execute_builtin_statement("PAUSE", make_null(), 1, make_int(50));
sb_exit:
    return __result;
}

static Value r62_hole_in_one(void)
{
    Value __result = make_null();
    Cell v8_F = make_cell(make_null());
    DynamicBinding __frame_bindings[1];
    __frame_bindings[0].name = "F";
    __frame_bindings[0].cell = &v8_F;
    push_dynamic_frame(__frame_bindings, 1);
    execute_builtin_statement("PAPER", make_null(), 1, make_int(1));
    execute_builtin_statement("INK", make_null(), 1, make_int(7));
    execute_builtin_statement("CLS", make_null(), 0);
    execute_builtin_statement("INK", make_null(), 1, make_int(7));
    {
        int loop_0_step = as_int(make_int(9));
        int loop_0_end = as_int(make_int(130));
        for (int loop_0_index = as_int(make_int(1)); (loop_0_step >= 0) ? (loop_0_index <= loop_0_end) : (loop_0_index >= loop_0_end); loop_0_index += loop_0_step)
        {
            (&v8_F)->value = make_int(loop_0_index);
            execute_builtin_statement("CURSOR", make_null(), 2, (&v8_F)->value, make_int(50));
            execute_builtin_statement("PRINT", make_null(), 1, make_string("Hole I"));
            loop_0_next: ;
        }
        loop_0_exit: ;
    }
    {
        int loop_1_step = as_int(negate_value(make_int(9)));
        int loop_1_end = as_int(make_int(212));
        for (int loop_1_index = as_int(make_int(320)); (loop_1_step >= 0) ? (loop_1_index <= loop_1_end) : (loop_1_index >= loop_1_end); loop_1_index += loop_1_step)
        {
            (&v8_F)->value = make_int(loop_1_index);
            execute_builtin_statement("CURSOR", make_null(), 2, (&v8_F)->value, make_int(50));
            execute_builtin_statement("PRINT", make_null(), 1, make_string("n One "));
            goto loop_1_next;
            loop_1_next: ;
        }
        loop_1_exit: ;
    }
    execute_builtin_statement("PRINT", make_null(), 0);
    execute_builtin_statement("PRINT", make_null(), 0);
    execute_builtin_statement("PRINT", make_null(), 1, make_string("           Superb Shot!!!"));
    execute_builtin_statement("PAUSE", make_null(), 1, make_int(200));
    execute_builtin_statement("CLS", make_null(), 0);
    execute_builtin_statement("PAUSE", make_null(), 1, make_int(200));
    execute_builtin_statement("CLS", make_null(), 0);
sb_exit:
    pop_dynamic_frame();
    return __result;
}

static Value r95_top(void)
{
    Value __result = make_null();
    Cell v15_F = make_cell(make_null());
    DynamicBinding __frame_bindings[1];
    __frame_bindings[0].name = "F";
    __frame_bindings[0].cell = &v15_F;
    push_dynamic_frame(__frame_bindings, 1);
    execute_builtin_statement("PAUSE", make_null(), 1, make_int(100));
    execute_builtin_statement("CLS", make_int(0), 0);
    execute_builtin_statement("CLS", make_null(), 0);
    if (is_true(compare_not_equal((lookup_dynamic_cell("POINTER$"))->value, make_string(" "))))
    {
        goto line_3480;
    }
    execute_builtin_statement("PRINT", make_null(), 1, make_string("Well done a course record"));
    execute_input(make_int(0), 1, make_string("Your name ? "));
    set_string_char_value(lookup_dynamic_cell("NAME$"), as_int((lookup_dynamic_cell("COURSE"))->value), read_input_value(0, TYPE_STRING));
    set_array_value(lookup_dynamic_cell("SCORE"), (lookup_dynamic_cell("SC1"))->value, 1, (lookup_dynamic_cell("COURSE"))->value);
    execute_builtin_statement("PAPER", make_null(), 1, make_int(1));
    execute_builtin_statement("INK", make_null(), 1, make_int(7));
    execute_builtin_statement("CLS", make_null(), 0);
    line_3480: ;
    (lookup_dynamic_cell("START"))->value = make_int(1);
    (lookup_dynamic_cell("FINISH"))->value = make_int(50);
    execute_builtin_statement("MODE", make_null(), 1, make_int(512));
    line_3490: ;
    execute_builtin_statement("CLS", make_null(), 0);
    {
        int loop_0_step = as_int(make_int(1));
        int loop_0_end = as_int((lookup_dynamic_cell("FINISH"))->value);
        for (int loop_0_index = as_int((lookup_dynamic_cell("START"))->value); (loop_0_step >= 0) ? (loop_0_index <= loop_0_end) : (loop_0_index >= loop_0_end); loop_0_index += loop_0_step)
        {
            (&v15_F)->value = make_int(loop_0_index);
            if (is_true(compare_equal((&v15_F)->value, (lookup_dynamic_cell("COURSE"))->value)))
            {
                execute_builtin_statement("INK", make_null(), 1, make_int(2));
                execute_builtin_statement("PAPER", make_null(), 1, make_int(6));
            }
            execute_builtin_statement("PRINT", make_null(), 5, (&v15_F)->value, make_string("  "), get_array_value(lookup_dynamic_cell("NAME$"), 2, (&v15_F)->value, slice_range_value(make_int(1), make_int(10))), make_string("  "), get_array_value(lookup_dynamic_cell("SCORE"), 1, (&v15_F)->value));
            execute_builtin_statement("INK", make_null(), 1, make_int(7));
            execute_builtin_statement("PAPER", make_null(), 1, make_int(0));
            loop_0_next: ;
        }
        loop_0_exit: ;
    }
    if (is_true(compare_equal((lookup_dynamic_cell("START"))->value, make_int(51))))
    {
        execute_builtin_statement("CLS", make_int(0), 0);
        execute_builtin_statement("CSIZE", make_int(0), 2, make_int(1), make_int(0));
        execute_builtin_statement("PRINT", make_int(0), 0);
        execute_builtin_statement("PRINT", make_int(0), 1, make_string("                  Hit Any Key"));
        execute_builtin_statement("PAUSE", make_null(), 1, make_int(10000));
        execute_builtin_statement("MODE", make_null(), 1, make_int(256));
        execute_builtin_statement("CSIZE", make_int(0), 2, make_int(0), make_int(0));
        __result = make_null();
        goto sb_exit;
    }
    else
    {
        execute_builtin_statement("CLS", make_int(0), 0);
        execute_builtin_statement("CSIZE", make_int(0), 2, make_int(1), make_int(0));
        execute_builtin_statement("PRINT", make_int(0), 0);
        execute_builtin_statement("PRINT", make_int(0), 1, make_string("            Hit Any Key To See Next 50"));
        execute_builtin_statement("PAUSE", make_null(), 1, make_int(10000));
        (lookup_dynamic_cell("START"))->value = make_int(51);
        (lookup_dynamic_cell("FINISH"))->value = make_int(100);
        execute_builtin_statement("CSIZE", make_int(0), 2, make_int(0), make_int(0));
        goto line_3490;
    }
sb_exit:
    pop_dynamic_frame();
    return __result;
}

static Value r89_scorecard(void)
{
    Value __result = make_null();
    Cell v14_F = make_cell(make_null());
    DynamicBinding __frame_bindings[1];
    __frame_bindings[0].name = "F";
    __frame_bindings[0].cell = &v14_F;
    push_dynamic_frame(__frame_bindings, 1);
    execute_builtin_statement("PRINT", make_null(), 0);
    execute_builtin_statement("PRINT", make_null(), 3, make_string(" Hole"), make_string("Par"), make_string("Score"));
    execute_builtin_statement("PRINT", make_null(), 0);
    (lookup_dynamic_cell("TOT"))->value = make_int(0);
    (lookup_dynamic_cell("TOTPAR"))->value = make_int(0);
    {
        int loop_0_step = as_int(make_int(1));
        int loop_0_end = as_int((lookup_dynamic_cell("HOLES"))->value);
        for (int loop_0_index = as_int(make_int(1)); (loop_0_step >= 0) ? (loop_0_index <= loop_0_end) : (loop_0_index >= loop_0_end); loop_0_index += loop_0_step)
        {
            (&v14_F)->value = make_int(loop_0_index);
            (lookup_dynamic_cell("DIF"))->value = subtract_value(get_array_value(lookup_dynamic_cell("SHOOTS"), 1, (&v14_F)->value), get_array_value(lookup_dynamic_cell("PARS"), 1, (&v14_F)->value));
            if (is_true(bitwise_and_value(compare_greater_than_or_equal((lookup_dynamic_cell("DIF"))->value, (lookup_dynamic_cell("DIF"))->value), compare_less_than_or_equal((lookup_dynamic_cell("DIF"))->value, negate_value(make_int(1))))))
            {
                (lookup_dynamic_cell("D$"))->value = make_string("Birdie");
            }
            else
            {
                if (is_true(bitwise_and_value(compare_greater_than_or_equal((lookup_dynamic_cell("DIF"))->value, (lookup_dynamic_cell("DIF"))->value), compare_less_than_or_equal((lookup_dynamic_cell("DIF"))->value, negate_value(make_int(2))))))
                {
                    (lookup_dynamic_cell("D$"))->value = make_string("Eagle");
                }
                else
                {
                    if (is_true(bitwise_and_value(compare_greater_than_or_equal((lookup_dynamic_cell("DIF"))->value, (lookup_dynamic_cell("DIF"))->value), compare_less_than_or_equal((lookup_dynamic_cell("DIF"))->value, negate_value(make_int(3))))))
                    {
                        (lookup_dynamic_cell("D$"))->value = make_string("Albetros");
                    }
                    else
                    {
                        if (is_true(bitwise_and_value(compare_greater_than_or_equal((lookup_dynamic_cell("DIF"))->value, (lookup_dynamic_cell("DIF"))->value), compare_less_than_or_equal((lookup_dynamic_cell("DIF"))->value, make_int(0)))))
                        {
                            (lookup_dynamic_cell("D$"))->value = make_string("Par");
                        }
                        else
                        {
                            if (is_true(bitwise_and_value(compare_greater_than_or_equal((lookup_dynamic_cell("DIF"))->value, (lookup_dynamic_cell("DIF"))->value), compare_less_than_or_equal((lookup_dynamic_cell("DIF"))->value, slice_range_value(make_int(1), make_int(20))))))
                            {
                                (lookup_dynamic_cell("D$"))->value = concat_value(make_string("Bogey "), (lookup_dynamic_cell("DIF"))->value);
                            }
                        }
                    }
                }
            }
            if (is_true(compare_equal(get_array_value(lookup_dynamic_cell("SHOOTS"), 1, (&v14_F)->value), make_int(0))))
            {
                (lookup_dynamic_cell("D$"))->value = make_string("------");
            }
            execute_builtin_statement("PRINT", make_null(), 6, make_string("  "), (&v14_F)->value, get_array_value(lookup_dynamic_cell("PARS"), 1, (&v14_F)->value), make_string(" "), get_array_value(lookup_dynamic_cell("SHOOTS"), 1, (&v14_F)->value), make_string("       "));
            execute_builtin_statement("INK", make_null(), 1, make_int(7));
            execute_builtin_statement("PRINT", make_null(), 1, (lookup_dynamic_cell("D$"))->value);
            execute_builtin_statement("INK", make_null(), 1, make_int(7));
            (lookup_dynamic_cell("TOT"))->value = add_value((lookup_dynamic_cell("TOT"))->value, get_array_value(lookup_dynamic_cell("SHOOTS"), 1, (&v14_F)->value));
            (lookup_dynamic_cell("TOTPAR"))->value = add_value((lookup_dynamic_cell("TOTPAR"))->value, get_array_value(lookup_dynamic_cell("PARS"), 1, (&v14_F)->value));
            loop_0_next: ;
        }
        loop_0_exit: ;
    }
    execute_builtin_statement("PRINT", make_null(), 0);
    execute_builtin_statement("PRINT", make_null(), 4, (lookup_dynamic_cell("TOTPAR"))->value, make_string(" "), (lookup_dynamic_cell("TOT"))->value, make_string("   "));
    execute_builtin_statement("PRINT", make_null(), 1, subtract_value((lookup_dynamic_cell("TOT"))->value, (lookup_dynamic_cell("TOTPAR"))->value));
    execute_builtin_statement("PRINT", make_null(), 0);
    execute_builtin_statement("PRINT", make_null(), 0);
    execute_builtin_statement("PRINT", make_null(), 3, make_string("Course Record "), get_array_value(lookup_dynamic_cell("SCORE"), 1, (lookup_dynamic_cell("COURSE"))->value), make_string(" by "));
    execute_builtin_statement("PRINT", make_null(), 1, get_array_value(lookup_dynamic_cell("NAME$"), 2, (lookup_dynamic_cell("COURSE"))->value, slice_range_value(make_int(1), make_int(10))));
sb_exit:
    pop_dynamic_frame();
    return __result;
}

static Value r46_datasave(void)
{
    Value __result = make_null();
    Cell v2_N = make_cell(make_null());
    DynamicBinding __frame_bindings[1];
    __frame_bindings[0].name = "N";
    __frame_bindings[0].cell = &v2_N;
    push_dynamic_frame(__frame_bindings, 1);
    execute_builtin_statement("DELETE", make_null(), 1, (lookup_dynamic_cell("FLP1_GOLF_DATA"))->value);
    execute_builtin_statement("OPEN_NEW", make_int(9), 1, make_string("flp1_golf_data"));
    {
        int loop_0_step = as_int(make_int(1));
        int loop_0_end = as_int(make_int(100));
        for (int loop_0_index = as_int(make_int(1)); (loop_0_step >= 0) ? (loop_0_index <= loop_0_end) : (loop_0_index >= loop_0_end); loop_0_index += loop_0_step)
        {
            (&v2_N)->value = make_int(loop_0_index);
            execute_builtin_statement("PRINT", make_int(9), 1, get_array_value(lookup_dynamic_cell("SCORE"), 1, (&v2_N)->value));
            goto loop_0_next;
            loop_0_next: ;
        }
        loop_0_exit: ;
    }
    {
        int loop_1_step = as_int(make_int(1));
        int loop_1_end = as_int(make_int(100));
        for (int loop_1_index = as_int(make_int(1)); (loop_1_step >= 0) ? (loop_1_index <= loop_1_end) : (loop_1_index >= loop_1_end); loop_1_index += loop_1_step)
        {
            (&v2_N)->value = make_int(loop_1_index);
            execute_builtin_statement("PRINT", make_int(9), 1, get_array_value(lookup_dynamic_cell("NAME$"), 2, (&v2_N)->value, slice_range_value(make_int(1), make_int(10))));
            loop_1_next: ;
        }
        loop_1_exit: ;
    }
    execute_builtin_statement("CLOSE", make_int(9), 0);
sb_exit:
    pop_dynamic_frame();
    return __result;
}

static Value r45_dataload(void)
{
    Value __result = make_null();
    Cell v1_N = make_cell(make_null());
    DynamicBinding __frame_bindings[1];
    __frame_bindings[0].name = "N";
    __frame_bindings[0].cell = &v1_N;
    push_dynamic_frame(__frame_bindings, 1);
    execute_builtin_statement("OPEN_IN", make_int(9), 1, make_string("flp1_golf_data"));
    {
        int loop_0_step = as_int(make_int(1));
        int loop_0_end = as_int(make_int(100));
        for (int loop_0_index = as_int(make_int(1)); (loop_0_step >= 0) ? (loop_0_index <= loop_0_end) : (loop_0_index >= loop_0_end); loop_0_index += loop_0_step)
        {
            (&v1_N)->value = make_int(loop_0_index);
            execute_input(make_int(9), 0);
            set_array_value(lookup_dynamic_cell("SCORE"), read_input_value(0, TYPE_INT), 1, (&v1_N)->value);
            goto loop_0_next;
            loop_0_next: ;
        }
        loop_0_exit: ;
    }
    {
        int loop_1_step = as_int(make_int(1));
        int loop_1_end = as_int(make_int(100));
        for (int loop_1_index = as_int(make_int(1)); (loop_1_step >= 0) ? (loop_1_index <= loop_1_end) : (loop_1_index >= loop_1_end); loop_1_index += loop_1_step)
        {
            (&v1_N)->value = make_int(loop_1_index);
            execute_input(make_int(9), 0);
            set_array_value(lookup_dynamic_cell("NAME$"), read_input_value(0, TYPE_STRING), 2, (&v1_N)->value, slice_range_value(make_int(1), make_int(10)));
            loop_1_next: ;
        }
        loop_1_exit: ;
    }
    execute_builtin_statement("CLOSE", make_int(9), 0);
sb_exit:
    pop_dynamic_frame();
    return __result;
}

static Value r63_initialise(void)
{
    Value __result = make_null();
    Cell v9_F = make_cell(make_null());
    DynamicBinding __frame_bindings[1];
    __frame_bindings[0].name = "F";
    __frame_bindings[0].cell = &v9_F;
    push_dynamic_frame(__frame_bindings, 1);
    execute_builtin_statement("RANDOMISE", make_null(), 1, make_int(20000));
    restore_to_line(as_int(make_int(4090)));
    {
        int loop_0_step = as_int(make_int(1));
        int loop_0_end = as_int(make_int(100));
        for (int loop_0_index = as_int(make_int(1)); (loop_0_step >= 0) ? (loop_0_index <= loop_0_end) : (loop_0_index >= loop_0_end); loop_0_index += loop_0_step)
        {
            (&v9_F)->value = make_int(loop_0_index);
            set_array_value(lookup_dynamic_cell("SCORE"), invoke_builtin_function("RND", 1, slice_range_value(negate_value(make_int(9)), negate_value(make_int(4)))), 1, (&v9_F)->value);
            (lookup_dynamic_cell("PLAYER$"))->value = read_data_value(TYPE_STRING);
            if (is_true(compare_equal(divide_value((&v9_F)->value, make_int(5)), invoke_builtin_function("INT", 1, divide_value((&v9_F)->value, make_int(5))))))
            {
                restore_to_line(as_int(make_int(4090)));
            }
            set_array_value(lookup_dynamic_cell("NAME$"), (lookup_dynamic_cell("PLAYER$"))->value, 2, (&v9_F)->value, slice_range_value(make_int(1), make_int(10)));
            line_4090: ;
            loop_0_next: ;
        }
        loop_0_exit: ;
    }
sb_exit:
    pop_dynamic_frame();
    return __result;
}

static Value r64_intro(void)
{
    Value __result = make_null();
    execute_builtin_statement("MODE", make_null(), 1, make_int(4));
    execute_builtin_statement("WINDOW", make_null(), 4, make_int(512), make_int(256), make_int(0), make_int(0));
    execute_builtin_statement("PAPER", make_null(), 1, make_int(0));
    execute_builtin_statement("INK", make_null(), 1, make_int(7));
    execute_builtin_statement("CLS", make_null(), 0);
    execute_builtin_statement("CSIZE", make_null(), 2, make_int(1), make_int(0));
    execute_builtin_statement("BORDER", make_null(), 2, make_int(18), make_int(0));
    execute_builtin_statement("PRINT", make_null(), 1, make_string("         QL Golfer"));
    execute_builtin_statement("PRINT", make_null(), 1, make_string("         ========="));
    execute_builtin_statement("PRINT", make_string("Instructions:"), 0);
    execute_builtin_statement("PRINT", make_null(), 1, make_string("The object of the game is to get your ball into"));
    execute_builtin_statement("PRINT", make_null(), 1, make_string("the hole in as little shots as you can."));
    execute_builtin_statement("PRINT", make_string("When you have chosen the course you want play,"), 1, make_string("at the bottom of the screen is a bar graph"));
    execute_builtin_statement("PRINT", make_null(), 1, make_string("indicating the power of your shot. When the"));
    execute_builtin_statement("PRINT", make_null(), 1, make_string("graph reaches the strength you want,"));
    execute_builtin_statement("PRINT", make_null(), 1, make_string("press the space bar to hit."));
    execute_builtin_statement("PRINT", make_null(), 1, make_string("You are then prompted for the angle of trajectory."));
    execute_builtin_statement("PRINT", make_null(), 1, make_string("This follows exactly the layout on a clock face,"));
    execute_builtin_statement("PRINT", make_null(), 1, make_string("so if you want to go straight up, type 12,"));
    execute_builtin_statement("PRINT", make_null(), 1, make_string("or 6 to go straight down etc."));
    execute_builtin_statement("PRINT", make_string("When you reach the green, a small golf club"), 2, make_string("appears, and you press the space bar"), make_string("when you want to hit the ball."));
    execute_builtin_statement("PRINT", make_string("Press any key to continue"), 0);
    (lookup_dynamic_cell("X$"))->value = invoke_builtin_function("INKEY$", 1, negate_value(make_int(1)));
    execute_builtin_statement("CLS", make_null(), 0);
sb_exit:
    return __result;
}

int main(void)
{
    sb_runtime_init();
    v18_A = make_cell(make_null());
    register_global("A", &v18_A);
    v19_AA = make_cell(make_null());
    register_global("AA", &v19_AA);
    v20_ADD = make_cell(make_null());
    register_global("ADD", &v20_ADD);
    v21_AN_ = make_cell(make_null());
    register_global("AN$", &v21_AN_);
    v23_ANGLE = make_cell(make_null());
    register_global("ANGLE", &v23_ANGLE);
    v24_B = make_cell(make_null());
    register_global("B", &v24_B);
    v26_BALLX = make_cell(make_null());
    register_global("BALLX", &v26_BALLX);
    v27_BALLY = make_cell(make_null());
    register_global("BALLY", &v27_BALLY);
    v28_BIT = make_cell(make_null());
    register_global("BIT", &v28_BIT);
    v29_BITT = make_cell(make_null());
    register_global("BITT", &v29_BITT);
    v30_BLOCKX = make_cell(make_null());
    register_global("BLOCKX", &v30_BLOCKX);
    v31_BLOCKY = make_cell(make_null());
    register_global("BLOCKY", &v31_BLOCKY);
    v32_BM = make_cell(make_null());
    register_global("BM", &v32_BM);
    v35_C = make_cell(make_null());
    register_global("C", &v35_C);
    v36_C1 = make_cell(make_null());
    register_global("C1", &v36_C1);
    v37_C2 = make_cell(make_null());
    register_global("C2", &v37_C2);
    v38_C3 = make_cell(make_null());
    register_global("C3", &v38_C3);
    v39_C4 = make_cell(make_null());
    register_global("C4", &v39_C4);
    v40_COLOR = make_cell(make_null());
    register_global("COLOR", &v40_COLOR);
    v41_COLOR1 = make_cell(make_null());
    register_global("COLOR1", &v41_COLOR1);
    v42_COURSE = make_cell(make_null());
    register_global("COURSE", &v42_COURSE);
    v43_CSE = make_cell(make_array());
    register_global("CSE", &v43_CSE);
    v44_D_ = make_cell(make_null());
    register_global("D$", &v44_D_);
    v47_DIF = make_cell(make_null());
    register_global("DIF", &v47_DIF);
    v48_DIST = make_cell(make_null());
    register_global("DIST", &v48_DIST);
    v49_E = make_cell(make_null());
    register_global("E", &v49_E);
    v51_FINISH = make_cell(make_null());
    register_global("FINISH", &v51_FINISH);
    v52_FINISHED = make_cell(make_null());
    register_global("FINISHED", &v52_FINISHED);
    v55_GRN = make_cell(make_null());
    register_global("GRN", &v55_GRN);
    v56_HAZARD = make_cell(make_null());
    register_global("HAZARD", &v56_HAZARD);
    v57_HOLE = make_cell(make_null());
    register_global("HOLE", &v57_HOLE);
    v58_HOLED = make_cell(make_null());
    register_global("HOLED", &v58_HOLED);
    v59_HOLES = make_cell(make_null());
    register_global("HOLES", &v59_HOLES);
    v60_HOLEX = make_cell(make_null());
    register_global("HOLEX", &v60_HOLEX);
    v61_HOLEY = make_cell(make_null());
    register_global("HOLEY", &v61_HOLEY);
    v65_JI = make_cell(make_null());
    register_global("JI", &v65_JI);
    v67_LENGTH = make_cell(make_null());
    register_global("LENGTH", &v67_LENGTH);
    v70_MAX = make_cell(make_null());
    register_global("MAX", &v70_MAX);
    v71_NAME_ = make_cell(make_array());
    register_global("NAME$", &v71_NAME_);
    v72_NEWCOL = make_cell(make_null());
    register_global("NEWCOL", &v72_NEWCOL);
    v73_NEWCOL1 = make_cell(make_null());
    register_global("NEWCOL1", &v73_NEWCOL1);
    v74_OK = make_cell(make_null());
    register_global("OK", &v74_OK);
    v75_PAR = make_cell(make_null());
    register_global("PAR", &v75_PAR);
    v76_PARS = make_cell(make_array());
    register_global("PARS", &v76_PARS);
    v77_PLAYER_ = make_cell(make_null());
    register_global("PLAYER$", &v77_PLAYER_);
    v78_POINTER_ = make_cell(make_null());
    register_global("POINTER$", &v78_POINTER_);
    v80_POWER = make_cell(make_null());
    register_global("POWER", &v80_POWER);
    v82_PUTX = make_cell(make_null());
    register_global("PUTX", &v82_PUTX);
    v83_PUTY = make_cell(make_null());
    register_global("PUTY", &v83_PUTY);
    v84_R = make_cell(make_null());
    register_global("R", &v84_R);
    v85_RB = make_cell(make_null());
    register_global("RB", &v85_RB);
    v87_SC1 = make_cell(make_null());
    register_global("SC1", &v87_SC1);
    v88_SCORE = make_cell(make_array());
    register_global("SCORE", &v88_SCORE);
    v91_SHOOTS = make_cell(make_array());
    register_global("SHOOTS", &v91_SHOOTS);
    v92_SHOTS = make_cell(make_null());
    register_global("SHOTS", &v92_SHOTS);
    v93_START = make_cell(make_null());
    register_global("START", &v93_START);
    v96_TOT = make_cell(make_null());
    register_global("TOT", &v96_TOT);
    v97_TOTPAR = make_cell(make_null());
    register_global("TOTPAR", &v97_TOTPAR);
    v101_X = make_cell(make_null());
    register_global("X", &v101_X);
    v102_X_ = make_cell(make_null());
    register_global("X$", &v102_X_);
    v103_XZ = make_cell(make_null());
    register_global("XZ", &v103_XZ);
    v104_Y = make_cell(make_null());
    register_global("Y", &v104_Y);
    v105_ZX = make_cell(make_null());
    register_global("ZX", &v105_ZX);
    /* REMark Golfer */
    /* REMark intro */
    execute_builtin_statement("MODE", make_null(), 1, make_int(8));
    execute_builtin_statement("CLS", make_null(), 0);
    execute_builtin_statement("WINDOW", make_null(), 4, make_int(512), make_int(256), make_int(0), make_int(0));
    execute_builtin_statement("PAPER", make_null(), 1, make_int(0));
    execute_builtin_statement("CLS", make_null(), 0);
    execute_builtin_statement("WINDOW", make_null(), 4, make_int(448), make_int(200), make_int(32), make_int(16));
    execute_builtin_statement("WINDOW", make_int(0), 4, make_int(448), make_int(40), make_int(32), make_int(216));
    execute_builtin_statement("PAPER", make_int(0), 1, make_int(0));
    execute_builtin_statement("CLS", make_int(0), 0);
    execute_builtin_statement("PAPER", make_int(2), 1, make_int(0));
    execute_builtin_statement("BORDER", make_int(2), 1, make_int(0));
    execute_builtin_statement("INK", make_int(0), 1, make_int(7));
    execute_builtin_statement("BORDER", make_null(), 2, make_int(0), make_int(0));
    execute_builtin_statement("SCALE", make_null(), 3, make_int(100), make_int(0), make_int(0));
    execute_builtin_statement("PRINT", make_null(), 1, make_string("Please Wait"));
    (&v59_HOLES)->value = make_int(9);
    r63_initialise();
    execute_builtin_statement("CLS", make_null(), 0);
    (&v70_MAX)->value = make_int(100);
    execute_builtin_statement("CLS", make_int(0), 0);
    execute_builtin_statement("BORDER", make_int(0), 2, make_int(2), make_int(4));
    (&v42_COURSE)->value = make_int(0);
    execute_builtin_statement("CLS", make_int(0), 0);
    line_170: ;
    execute_input(make_int(0), 1, make_string("Load Your Own High Scores ? (y/n) "));
    (&v21_AN_)->value = read_input_value(0, TYPE_STRING);
    if (is_true(bitwise_or_value(compare_equal((&v21_AN_)->value, make_string("Y")), compare_equal((&v21_AN_)->value, make_string("y")))))
    {
        r45_dataload();
    }
    execute_builtin_statement("CLS", make_int(0), 0);
    execute_builtin_statement("PRINT", make_int(0), 0);
    execute_builtin_statement("PRINT", make_int(0), 1, make_string("SPACE=PLAY : ANY KEY=TOPSCORES"));
    (&v78_POINTER_)->value = invoke_builtin_function("INKEY$", 1, negate_value(make_int(1)));
    if (is_true(compare_not_equal((&v78_POINTER_)->value, make_string(" "))))
    {
        r95_top();
    }
    (&v78_POINTER_)->value = make_string(" ");
    execute_builtin_statement("CLS", make_int(0), 0);
    line_230: ;
    execute_input(make_int(0), 1, make_string("Number of course? (1-100)  "));
    (&v42_COURSE)->value = read_input_value(0, TYPE_INT);
    if (is_true(bitwise_or_value(compare_greater_than((&v42_COURSE)->value, make_int(100)), compare_less_than((&v42_COURSE)->value, make_int(0)))))
    {
        goto line_230;
    }
    execute_builtin_statement("RANDOMISE", make_null(), 1, (&v42_COURSE)->value);
    execute_builtin_statement("PAPER", make_int(0), 1, make_int(0));
    execute_builtin_statement("CLS", make_null(), 0);
    (&v87_SC1)->value = make_int(0);
    {
        int loop_0_step = as_int(make_int(1));
        int loop_0_end = as_int((&v59_HOLES)->value);
        for (int loop_0_index = as_int(make_int(1)); (loop_0_step >= 0) ? (loop_0_index <= loop_0_end) : (loop_0_index >= loop_0_end); loop_0_index += loop_0_step)
        {
            (&v57_HOLE)->value = make_int(loop_0_index);
            (&v92_SHOTS)->value = make_int(0);
            (&v52_FINISHED)->value = make_int(0);
            execute_builtin_statement("PAPER", make_null(), 3, make_int(7), make_int(4), make_int(1));
            execute_builtin_statement("CLS", make_null(), 0);
            execute_builtin_statement("CLS", make_int(0), 0);
            r98_trees(make_value_arg(make_int(75)));
            r50_fairway();
            r98_trees(make_value_arg(make_int(45)));
            if (is_true(compare_equal(invoke_builtin_function("RND", 1, slice_range_value(make_int(0), make_int(1))), make_int(1))))
            {
                r99_water();
            }
            else
            {
                r66_lake();
            }
            r54_green();
            (&v67_LENGTH)->value = add_value(subtract_value(make_int(800), multiply_value(make_int(20), (&v35_C)->value)), invoke_builtin_function("RND", 1, slice_range_value(make_int(0), make_int(20))));
            if (is_true(compare_less_than_or_equal((&v67_LENGTH)->value, make_int(270))))
            {
                (&v75_PAR)->value = make_int(3);
            }
            if (is_true(bitwise_and_value(compare_greater_than((&v67_LENGTH)->value, make_int(270)), compare_less_than((&v67_LENGTH)->value, make_int(395)))))
            {
                (&v75_PAR)->value = make_int(4);
            }
            if (is_true(compare_greater_than((&v67_LENGTH)->value, make_int(395))))
            {
                (&v75_PAR)->value = make_int(5);
            }
            execute_builtin_statement("CSIZE", make_null(), 2, make_int(0), make_int(0));
            execute_builtin_statement("INK", make_null(), 1, make_int(0));
            execute_builtin_statement("PRINT", make_null(), 2, make_string("  Hole "), (&v57_HOLE)->value);
            execute_builtin_statement("PRINT", make_null(), 2, make_string("     Length "), (&v67_LENGTH)->value);
            execute_builtin_statement("PRINT", make_null(), 2, make_string("     Par "), (&v75_PAR)->value);
            execute_builtin_statement("AT", make_null(), 2, make_int(19), make_int(0));
            execute_builtin_statement("PRINT", make_null(), 4, make_string("score "), (&v87_SC1)->value, make_string("     Course "), (&v42_COURSE)->value);
            while (1)
            {
                r79_pow();
                execute_builtin_statement("PAUSE", make_null(), 1, make_int(50));
                r22_ang();
                (&v92_SHOTS)->value = add_value((&v92_SHOTS)->value, make_int(1));
                r90_shoot();
                r94_test();
                if (is_true((&v52_FINISHED)->value))
                {
                    goto loop_1_exit;
                }
                loop_1_next: ;
            }
            loop_1_exit: ;
            execute_builtin_statement("PAPER", make_null(), 1, make_int(1));
            execute_builtin_statement("INK", make_null(), 1, make_int(7));
            execute_builtin_statement("CLS", make_int(0), 0);
            execute_builtin_statement("PRINT", make_int(0), 1, make_string("   It's in, well played"));
            execute_builtin_statement("PAUSE", make_null(), 1, make_int(40));
            execute_builtin_statement("CLS", make_null(), 0);
            set_array_value(&v91_SHOOTS, (&v92_SHOTS)->value, 1, (&v57_HOLE)->value);
            set_array_value(&v76_PARS, (&v75_PAR)->value, 1, (&v57_HOLE)->value);
            if (is_true(compare_equal((&v92_SHOTS)->value, make_int(1))))
            {
                r62_hole_in_one();
            }
            if (is_true(compare_less_than_or_equal((&v92_SHOTS)->value, (&v75_PAR)->value)))
            {
                execute_builtin_statement("CLS", make_int(0), 0);
                (&v84_R)->value = subtract_value((&v75_PAR)->value, (&v92_SHOTS)->value);
                if (is_true(bitwise_and_value(compare_greater_than_or_equal((&v84_R)->value, (&v84_R)->value), compare_less_than_or_equal((&v84_R)->value, make_int(0)))))
                {
                    execute_builtin_statement("PRINT", make_int(0), 1, make_string("A Par, Not Bad"));
                }
                else
                {
                    if (is_true(bitwise_and_value(compare_greater_than_or_equal((&v84_R)->value, (&v84_R)->value), compare_less_than_or_equal((&v84_R)->value, make_int(1)))))
                    {
                        execute_builtin_statement("PRINT", make_int(0), 1, make_string("Well played, A birdie"));
                    }
                    else
                    {
                        if (is_true(bitwise_and_value(compare_greater_than_or_equal((&v84_R)->value, (&v84_R)->value), compare_less_than_or_equal((&v84_R)->value, make_int(2)))))
                        {
                            execute_builtin_statement("PRINT", make_int(0), 1, make_string("Fantastic shot, An Eagle"));
                        }
                        else
                        {
                            if (is_true(bitwise_and_value(compare_greater_than_or_equal((&v84_R)->value, (&v84_R)->value), compare_less_than_or_equal((&v84_R)->value, make_int(3)))))
                            {
                                execute_builtin_statement("PRINT", make_int(0), 1, make_string("An Albatross..incredible!"));
                            }
                        }
                    }
                }
                set_array_value(&v91_SHOOTS, (&v92_SHOTS)->value, 1, (&v57_HOLE)->value);
                set_array_value(&v76_PARS, (&v75_PAR)->value, 1, (&v57_HOLE)->value);
                r89_scorecard();
            }
            else
            {
                execute_builtin_statement("CLS", make_int(0), 0);
                execute_builtin_statement("PRINT", make_int(0), 2, subtract_value((&v92_SHOTS)->value, (&v75_PAR)->value), make_string("over par on this hole."));
                set_array_value(&v91_SHOOTS, (&v92_SHOTS)->value, 1, (&v57_HOLE)->value);
                set_array_value(&v76_PARS, (&v75_PAR)->value, 1, (&v57_HOLE)->value);
                r89_scorecard();
            }
            (&v87_SC1)->value = subtract_value(add_value((&v87_SC1)->value, (&v92_SHOTS)->value), (&v75_PAR)->value);
            execute_builtin_statement("PAUSE", make_null(), 1, make_int(200));
            loop_0_next: ;
        }
        loop_0_exit: ;
    }
    execute_builtin_statement("PRINT", make_int(0), 1, make_string("Overall you were "));
    if (is_true((&v87_SC1)->value))
    {
        execute_builtin_statement("PRINT", make_null(), 2, invoke_builtin_function("ABS", 1, (&v87_SC1)->value), make_string(" "));
    }
    if (is_true(compare_equal((&v87_SC1)->value, make_int(0))))
    {
        execute_builtin_statement("PRINT", make_null(), 1, make_string("on par"));
    }
    if (is_true(compare_greater_than((&v87_SC1)->value, make_int(0))))
    {
        execute_builtin_statement("PRINT", make_null(), 1, make_string("over par"));
    }
    if (is_true(compare_less_than((&v87_SC1)->value, make_int(0))))
    {
        execute_builtin_statement("PRINT", make_null(), 1, make_string("under par"));
    }
    if (is_true(compare_less_than((&v87_SC1)->value, get_array_value(&v88_SCORE, 1, (&v42_COURSE)->value))))
    {
        r95_top();
        r46_datasave();
    }
    execute_builtin_statement("CLS", make_int(0), 0);
    execute_builtin_statement("CLS", make_null(), 0);
    r89_scorecard();
    line_780: ;
    execute_builtin_statement("CLS", make_int(0), 0);
    execute_builtin_statement("PRINT", make_int(0), 1, make_string("Hit spacebar to play again"));
    if (is_true(compare_equal(invoke_builtin_function("INKEY$", 1, negate_value(make_int(1))), make_string(" "))))
    {
        goto line_170;
    }
    goto line_780;
    execute_builtin_statement("POKE_W", make_null(), 2, (&v20_ADD)->value, (&v40_COLOR)->value);
    execute_builtin_statement("POKE_W", make_null(), 2, add_value((&v20_ADD)->value, make_int(128)), (&v41_COLOR1)->value);
    execute_builtin_statement("PAUSE", make_null(), 1, make_int(10));
    execute_builtin_statement("POKE_W", make_null(), 2, (&v20_ADD)->value, (&v72_NEWCOL)->value);
    execute_builtin_statement("POKE_W", make_null(), 2, add_value((&v20_ADD)->value, make_int(128)), (&v73_NEWCOL1)->value);
    execute_builtin_statement("PAUSE", make_null(), 1, make_int(9));
    /* REMark END DEFine */
    execute_builtin_statement("POKE_W", make_null(), 2, (&v20_ADD)->value, (&v40_COLOR)->value);
    execute_builtin_statement("POKE_W", make_null(), 2, add_value((&v20_ADD)->value, make_int(1)), (&v41_COLOR1)->value);
    (&v58_HOLED)->value = make_int(1);
    /* REMark END DEFine */
    execute_builtin_statement("PAPER", make_null(), 1, make_int(0));
    execute_builtin_statement("CLS", make_null(), 0);
    execute_builtin_statement("CSIZE", make_null(), 2, make_int(3), make_int(1));
    execute_builtin_statement("INK", make_null(), 1, make_int(7));
    execute_builtin_statement("CLS", make_int(0), 0);
    execute_builtin_statement("CLS", make_int(0), 0);
    execute_builtin_statement("PAPER", make_null(), 1, make_int(1));
    execute_builtin_statement("INK", make_null(), 1, make_int(7));
    execute_builtin_statement("CSIZE", make_null(), 2, make_int(0), make_int(0));
    execute_builtin_statement("CLS", make_null(), 0);
sb_exit:
    return 0;
}
