#ifndef SBRUNTIME_C_H
#define SBRUNTIME_C_H

#include <ctype.h>
#include <setjmp.h>
#include <stdarg.h>
#include <time.h>
#include "cport.h"

typedef struct ArrayEntry ArrayEntry;
typedef struct Cell Cell;
typedef struct ParamBinding ParamBinding;
typedef struct DynamicBinding DynamicBinding;
typedef enum ValueType { TYPE_NULL, TYPE_INT, TYPE_FLOAT, TYPE_STRING, TYPE_ARRAY } ValueType;
typedef struct Value { ValueType type; int int_value; double float_value; const char* string_value; ArrayEntry* array_value; } Value;
struct Cell { Value value; };
struct ArrayEntry { char* key; Cell cell; ArrayEntry* next; };
struct ParamBinding { int is_ref; Cell* ref_cell; Value value; };
struct DynamicBinding { const char* name; Cell* cell; };
typedef Value DataValue;
typedef struct RestorePoint { int line; int slot; } RestorePoint;

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

extern DataValue sb_data[];
extern int sb_data_count;
extern RestorePoint sb_restore_points[];
extern int sb_restore_point_count;
extern int sb_data_pointer;
extern char sb_input_buffer[4096];
extern char* sb_input_parts[256];
extern int sb_input_part_count;

void sb_runtime_init(void);
void sb_push_error_frame(jmp_buf* target);
void sb_pop_error_frame(void);
void sb_record_error_context(int line, int retry_line, int continue_line);
void sb_raise_error(int number, const char* name, const char* description);
void sb_fail_last_error(void);
int sb_last_error_number(void);
int sb_last_error_line(void);
const char* sb_last_error_name(void);
const char* sb_last_error_description(void);
Cell make_cell(Value value);
Value make_null(void);
Value make_int(int value);
Value make_float(double value);
Value make_string(const char* value);
Value make_array(void);
void runtime_not_supported(const char* message);
Value unsupported_dynamic_read(const char* name);
Value unsupported_dynamic_write(const char* name);
ParamBinding make_value_arg(Value value);
ParamBinding make_ref_arg(Cell* cell);
Cell* bind_parameter(const ParamBinding* binding, Cell* local_cell);
void push_dynamic_frame(DynamicBinding* bindings, int binding_count);
void pop_dynamic_frame(void);
void register_global(const char* name, Cell* cell);
Cell* lookup_dynamic_cell(const char* name);
int as_int(Value value);
double as_double(Value value);
const char* as_string(Value value);
int is_true(Value value);
Value negate_value(Value value);
Value bitwise_not_value(Value value);
Value concat_value(Value left, Value right);
Value add_value(Value left, Value right);
Value subtract_value(Value left, Value right);
Value multiply_value(Value left, Value right);
Value divide_value(Value left, Value right);
Value power_value(Value left, Value right);
Value integer_divide_value(Value left, Value right);
Value modulo_value(Value left, Value right);
Value bitwise_and_value(Value left, Value right);
Value bitwise_or_value(Value left, Value right);
Value bitwise_xor_value(Value left, Value right);
Value compare_equal(Value left, Value right);
Value compare_not_equal(Value left, Value right);
Value compare_less_than(Value left, Value right);
Value compare_less_than_or_equal(Value left, Value right);
Value compare_greater_than(Value left, Value right);
Value compare_greater_than_or_equal(Value left, Value right);
Value instr_value(Value left, Value right);
Value slice_range_value(Value left, Value right);
Value unsupported_unary(const char* name, Value value);
Value unsupported_binary(const char* name, Value left, Value right);
Cell* get_array_cell(Cell* array_cell, int count, ...);
Value get_array_value(Cell* array_cell, int count, ...);
void set_array_value(Cell* array_cell, Value value, int count, ...);
Value get_string_char_value(Cell* source, int one_based_index);
void set_string_char_value(Cell* target, int one_based_index, Value replacement);
Value invoke_builtin_function(const char* name, int arg_count, ...);
void execute_builtin_statement(const char* name, Value channel, int arg_count, ...);
void execute_input(Value channel, int prompt_count, ...);
Value read_input_value(int index, ValueType target_type);
Value read_data_value(ValueType target_type);
void restore_to_line(int line);

#endif
