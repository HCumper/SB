#include "sbruntime_c.h"

/* DATA and INPUT runtime state */
DataValue sb_data[] = {  };
int sb_data_count = 0;
RestorePoint sb_restore_points[] = {  };
int sb_restore_point_count = 0;
int sb_data_pointer = 0;
char sb_input_buffer[4096];
char* sb_input_parts[256];
int sb_input_part_count = 0;

/* Global storage */
static Cell v0_A;

int main(void);

/* Program entry point */
int main(void)
{

    sb_runtime_init();
    v0_A = make_cell(make_null());
    register_global("A", &v0_A);

    (&v0_A)->value = make_int(1);
    execute_builtin_statement("PRINT", make_null(), 1, add_value((&v0_A)->value, make_int(2)));
    goto sb_exit;

sb_exit:
    return 0;
}
