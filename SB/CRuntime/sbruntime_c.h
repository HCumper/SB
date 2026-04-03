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

void sb_runtime_init _P_(( void ));
void sb_push_error_frame _P_(( jmp_buf* target ));
void sb_pop_error_frame _P_(( void ));
void sb_record_error_context _P_(( int line, int retry_line, int continue_line ));
void sb_raise_error _P_(( int number, const char* name, const char* description ));
void sb_fail_last_error _P_(( void ));
int sb_last_error_number _P_(( void ));
int sb_last_error_line _P_(( void ));
const char* sb_last_error_name _P_(( void ));
const char* sb_last_error_description _P_(( void ));
Cell make_cell _P_(( Value value ));
Value make_null _P_(( void ));
Value make_int _P_(( int value ));
Value make_float _P_(( double value ));
Value make_string _P_(( const char* value ));
Value make_array _P_(( void ));
void runtime_not_supported _P_(( const char* message ));
Value unsupported_dynamic_read _P_(( const char* name ));
Value unsupported_dynamic_write _P_(( const char* name ));
ParamBinding make_value_arg _P_(( Value value ));
ParamBinding make_ref_arg _P_(( Cell* cell ));
Cell* bind_parameter _P_(( const ParamBinding* binding, Cell* local_cell ));
void push_dynamic_frame _P_(( DynamicBinding* bindings, int binding_count ));
void pop_dynamic_frame _P_(( void ));
void register_global _P_(( const char* name, Cell* cell ));
Cell* lookup_dynamic_cell _P_(( const char* name ));
int as_int _P_(( Value value ));
double as_double _P_(( Value value ));
const char* as_string _P_(( Value value ));
int is_true _P_(( Value value ));
Value negate_value _P_(( Value value ));
Value bitwise_not_value _P_(( Value value ));
Value concat_value _P_(( Value left, Value right ));
Value add_value _P_(( Value left, Value right ));
Value subtract_value _P_(( Value left, Value right ));
Value multiply_value _P_(( Value left, Value right ));
Value divide_value _P_(( Value left, Value right ));
Value power_value _P_(( Value left, Value right ));
Value integer_divide_value _P_(( Value left, Value right ));
Value modulo_value _P_(( Value left, Value right ));
Value bitwise_and_value _P_(( Value left, Value right ));
Value bitwise_or_value _P_(( Value left, Value right ));
Value bitwise_xor_value _P_(( Value left, Value right ));
Value compare_equal _P_(( Value left, Value right ));
Value compare_not_equal _P_(( Value left, Value right ));
Value compare_less_than _P_(( Value left, Value right ));
Value compare_less_than_or_equal _P_(( Value left, Value right ));
Value compare_greater_than _P_(( Value left, Value right ));
Value compare_greater_than_or_equal _P_(( Value left, Value right ));
Value instr_value _P_(( Value left, Value right ));
Value slice_range_value _P_(( Value left, Value right ));
Value unsupported_unary _P_(( const char* name, Value value ));
Value unsupported_binary _P_(( const char* name, Value left, Value right ));
Cell* get_array_cell _P_(( Cell* array_cell, int count, ... ));
Value get_array_value _P_(( Cell* array_cell, int count, ... ));
void set_array_value _P_(( Cell* array_cell, Value value, int count, ... ));
Value get_string_char_value _P_(( Cell* source, int one_based_index ));
void set_string_char_value _P_(( Cell* target, int one_based_index, Value replacement ));
Value invoke_builtin_function _P_(( const char* name, int arg_count, ... ));
void execute_builtin_statement _P_(( const char* name, Value channel, int arg_count, ... ));
void execute_input _P_(( Value channel, int prompt_count, ... ));
Value read_input_value _P_(( int index, ValueType target_type ));
Value read_data_value _P_(( ValueType target_type ));
void restore_to_line _P_(( int line ));

#endif
