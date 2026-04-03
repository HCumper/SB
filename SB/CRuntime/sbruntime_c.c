#include "sbruntime_c.h"

static int sb_runtime_initialized = 0;
static const long sb_ql_epoch_offset = 283996800L;
typedef struct DynamicFrame DynamicFrame;
typedef struct GlobalBinding GlobalBinding;
typedef struct ErrorFrame ErrorFrame;

struct DynamicFrame
{
    DynamicBinding* bindings;
    int binding_count;
    DynamicFrame* next;
};

struct GlobalBinding
{
    const char* name;
    Cell* cell;
    GlobalBinding* next;
};

struct ErrorFrame
{
    jmp_buf* target;
    ErrorFrame* next;
};

static DynamicFrame* sb_dynamic_frames = NULL;
static GlobalBinding* sb_global_bindings = NULL;
static ErrorFrame* sb_error_frames = NULL;
static int sb_error_number = 0;
static int sb_error_line = 0;
static int sb_error_retry_line = 0;
static int sb_error_continue_line = 0;
static const char* sb_error_name = "";
static const char* sb_error_description = "";

static char* duplicate_string(const char* source)
{
    size_t length = source ? strlen(source) + 1 : 1;
    char* copy = (char*)malloc(length);
    if (!copy) exit(1);
    if (source) memcpy(copy, source, length);
    else copy[0] = '\0';
    return copy;
}

static char* duplicate_substring(const char* source, size_t length)
{
    char* copy = (char*)malloc(length + 1);
    if (!copy) exit(1);
    memcpy(copy, source, length);
    copy[length] = '\0';
    return copy;
}

static int ci_compare(const char* left, const char* right)
{
    while (*left && *right)
    {
        int diff = toupper((unsigned char)*left) - toupper((unsigned char)*right);
        if (diff != 0) return diff;
        left++;
        right++;
    }
    return toupper((unsigned char)*left) - toupper((unsigned char)*right);
}

static int ci_starts_with(const char* value, const char* prefix)
{
    while (*prefix)
    {
        if (toupper((unsigned char)*value) != toupper((unsigned char)*prefix)) return 0;
        value++;
        prefix++;
    }
    return 1;
}

void sb_runtime_init(void)
{
    if (sb_runtime_initialized) return;
    CP_Initialise();
    srand((unsigned int)time(NULL));
    sb_runtime_initialized = 1;
}

void sb_push_error_frame(jmp_buf* target)
{
    ErrorFrame* frame = (ErrorFrame*)malloc(sizeof(ErrorFrame));
    if (!frame) exit(1);
    frame->target = target;
    frame->next = sb_error_frames;
    sb_error_frames = frame;
}

void sb_pop_error_frame(void)
{
    ErrorFrame* frame = sb_error_frames;
    if (!frame) return;
    sb_error_frames = frame->next;
    free(frame);
}

void sb_record_error_context(int line, int retry_line, int continue_line)
{
    sb_error_line = line;
    sb_error_retry_line = retry_line;
    sb_error_continue_line = continue_line;
}

void sb_raise_error(int number, const char* name, const char* description)
{
    sb_error_number = number;
    sb_error_name = name ? name : "";
    sb_error_description = description ? description : "";

    if (sb_error_frames && sb_error_frames->target)
    {
        longjmp(*sb_error_frames->target, 1);
    }

    fprintf(stderr, "%s\n", sb_error_description[0] ? sb_error_description : "Runtime error");
    exit(1);
}

void sb_fail_last_error(void)
{
    sb_raise_error(sb_error_number != 0 ? sb_error_number : -19, sb_error_name[0] ? sb_error_name : "ERR_NI", sb_error_description[0] ? sb_error_description : "Runtime error");
}

int sb_last_error_number(void) { return sb_error_number; }
int sb_last_error_line(void) { return sb_error_line; }
const char* sb_last_error_name(void) { return sb_error_name; }
const char* sb_last_error_description(void) { return sb_error_description; }

Cell make_cell(Value value)
{
    Cell cell;
    cell.value = value;
    return cell;
}

Value make_null(void) { Value v = { TYPE_NULL, 0, 0.0, NULL, NULL }; return v; }
Value make_int(int value) { Value v = { TYPE_INT, value, (double)value, NULL, NULL }; return v; }
Value make_float(double value) { Value v = { TYPE_FLOAT, 0, value, NULL, NULL }; return v; }
Value make_string(const char* value) { Value v = { TYPE_STRING, 0, 0.0, value ? value : "", NULL }; return v; }
Value make_array(void) { Value v = { TYPE_ARRAY, 0, 0.0, NULL, NULL }; return v; }

void runtime_not_supported(const char* message)
{
    sb_raise_error(-19, "ERR_NI", message);
}

Value unsupported_dynamic_read(const char* name)
{
    char message[256];
    snprintf(message, sizeof(message), "Dynamic scoped read '%s' is not supported by the generated C backend yet.", name);
    runtime_not_supported(message);
    return make_null();
}

Value unsupported_dynamic_write(const char* name)
{
    char message[256];
    snprintf(message, sizeof(message), "Dynamic scoped write '%s' is not supported by the generated C backend yet.", name);
    runtime_not_supported(message);
    return make_null();
}

ParamBinding make_value_arg(Value value)
{
    ParamBinding binding;
    binding.is_ref = 0;
    binding.ref_cell = NULL;
    binding.value = value;
    return binding;
}

ParamBinding make_ref_arg(Cell* cell)
{
    ParamBinding binding;
    binding.is_ref = 1;
    binding.ref_cell = cell;
    binding.value = make_null();
    return binding;
}

Cell* bind_parameter(const ParamBinding* binding, Cell* local_cell)
{
    if (binding->is_ref && binding->ref_cell != NULL)
    {
        return binding->ref_cell;
    }

    local_cell->value = binding->value;
    return local_cell;
}

void push_dynamic_frame(DynamicBinding* bindings, int binding_count)
{
    DynamicFrame* frame = (DynamicFrame*)malloc(sizeof(DynamicFrame));
    if (!frame) exit(1);
    frame->bindings = bindings;
    frame->binding_count = binding_count;
    frame->next = sb_dynamic_frames;
    sb_dynamic_frames = frame;
}

void pop_dynamic_frame(void)
{
    DynamicFrame* frame = sb_dynamic_frames;
    if (!frame) return;
    sb_dynamic_frames = frame->next;
    free(frame);
}

void register_global(const char* name, Cell* cell)
{
    GlobalBinding* binding = sb_global_bindings;
    while (binding)
    {
        if (ci_compare(binding->name, name) == 0)
        {
            binding->cell = cell;
            return;
        }
        binding = binding->next;
    }

    binding = (GlobalBinding*)malloc(sizeof(GlobalBinding));
    if (!binding) exit(1);
    binding->name = name;
    binding->cell = cell;
    binding->next = sb_global_bindings;
    sb_global_bindings = binding;
}

Cell* lookup_dynamic_cell(const char* name)
{
    DynamicFrame* frame = sb_dynamic_frames;
    while (frame)
    {
        int index;
        for (index = 0; index < frame->binding_count; index++)
        {
            if (ci_compare(frame->bindings[index].name, name) == 0)
            {
                return frame->bindings[index].cell;
            }
        }
        frame = frame->next;
    }

    {
        GlobalBinding* binding = sb_global_bindings;
        while (binding)
        {
            if (ci_compare(binding->name, name) == 0)
            {
                return binding->cell;
            }
            binding = binding->next;
        }
    }

    {
        char message[256];
        snprintf(message, sizeof(message), "Dynamic storage '%s' does not exist.", name);
        runtime_not_supported(message);
    }

    return NULL;
}

int as_int(Value value)
{
    switch (value.type)
    {
        case TYPE_INT: return value.int_value;
        case TYPE_FLOAT: return (int)llround(value.float_value);
        case TYPE_STRING: return value.string_value ? atoi(value.string_value) : 0;
        default: return 0;
    }
}

double as_double(Value value)
{
    switch (value.type)
    {
        case TYPE_INT: return (double)value.int_value;
        case TYPE_FLOAT: return value.float_value;
        case TYPE_STRING: return value.string_value ? atof(value.string_value) : 0.0;
        default: return 0.0;
    }
}

const char* as_string(Value value)
{
    static char buffers[16][256];
    static int next_buffer = 0;
    char* buffer = buffers[next_buffer++ % 16];
    switch (value.type)
    {
        case TYPE_STRING: return value.string_value ? value.string_value : "";
        case TYPE_INT: snprintf(buffer, 256, "%d", value.int_value); return buffer;
        case TYPE_FLOAT: snprintf(buffer, 256, "%.17g", value.float_value); return buffer;
        case TYPE_NULL: return "";
        default: return "<array>";
    }
}

int is_true(Value value)
{
    if (value.type == TYPE_STRING) return value.string_value && value.string_value[0] != '\0';
    if (value.type == TYPE_ARRAY) return 1;
    return fabs(as_double(value)) > 0.0000001;
}

Value negate_value(Value value) { return make_float(-as_double(value)); }
Value bitwise_not_value(Value value) { return make_int(~as_int(value)); }

Value concat_value(Value left, Value right)
{
    const char* left_text = as_string(left);
    const char* right_text = as_string(right);
    size_t left_length = strlen(left_text);
    size_t right_length = strlen(right_text);
    char* buffer = (char*)malloc(left_length + right_length + 1);
    if (!buffer) exit(1);
    memcpy(buffer, left_text, left_length);
    memcpy(buffer + left_length, right_text, right_length + 1);
    return make_string(buffer);
}

Value add_value(Value left, Value right) { return (left.type == TYPE_STRING || right.type == TYPE_STRING) ? concat_value(left, right) : make_float(as_double(left) + as_double(right)); }
Value subtract_value(Value left, Value right) { return make_float(as_double(left) - as_double(right)); }
Value multiply_value(Value left, Value right) { return make_float(as_double(left) * as_double(right)); }
Value divide_value(Value left, Value right) { return make_float(as_double(left) / as_double(right)); }
Value power_value(Value left, Value right) { return make_float(pow(as_double(left), as_double(right))); }
Value integer_divide_value(Value left, Value right) { return make_int(as_int(left) / as_int(right)); }
Value modulo_value(Value left, Value right) { return make_int(as_int(left) % as_int(right)); }
Value bitwise_and_value(Value left, Value right) { return make_int(as_int(left) & as_int(right)); }
Value bitwise_or_value(Value left, Value right) { return make_int(as_int(left) | as_int(right)); }
Value bitwise_xor_value(Value left, Value right) { return make_int(as_int(left) ^ as_int(right)); }
Value compare_equal(Value left, Value right) { return make_int((left.type == TYPE_STRING || right.type == TYPE_STRING) ? strcmp(as_string(left), as_string(right)) == 0 : fabs(as_double(left) - as_double(right)) < 0.0000001); }
Value compare_not_equal(Value left, Value right) { return make_int(as_int(compare_equal(left, right)) == 0); }
Value compare_less_than(Value left, Value right) { return make_int((left.type == TYPE_STRING || right.type == TYPE_STRING) ? strcmp(as_string(left), as_string(right)) < 0 : as_double(left) < as_double(right)); }
Value compare_less_than_or_equal(Value left, Value right) { return make_int((left.type == TYPE_STRING || right.type == TYPE_STRING) ? strcmp(as_string(left), as_string(right)) <= 0 : as_double(left) <= as_double(right)); }
Value compare_greater_than(Value left, Value right) { return make_int((left.type == TYPE_STRING || right.type == TYPE_STRING) ? strcmp(as_string(left), as_string(right)) > 0 : as_double(left) > as_double(right)); }
Value compare_greater_than_or_equal(Value left, Value right) { return make_int((left.type == TYPE_STRING || right.type == TYPE_STRING) ? strcmp(as_string(left), as_string(right)) >= 0 : as_double(left) >= as_double(right)); }
Value instr_value(Value left, Value right) { const char* source = as_string(left); const char* found = strstr(source, as_string(right)); return make_int(found ? (int)(found - source) + 1 : 0); }
Value slice_range_value(Value left, Value right) { (void)left; return right; }
Value unsupported_unary(const char* name, Value value) { runtime_not_supported(name); return value; }
Value unsupported_binary(const char* name, Value left, Value right) { runtime_not_supported(name); return left; }

Value get_string_char_value(Cell* source, int one_based_index)
{
    static char buffers[16][2];
    static int next_buffer = 0;
    char* buffer = buffers[next_buffer++ % 16];
    const char* text = as_string(source->value);
    size_t length = strlen(text);

    if (one_based_index < 1 || (size_t)one_based_index > length)
    {
        buffer[0] = '\0';
        buffer[1] = '\0';
        return make_string(buffer);
    }

    buffer[0] = text[one_based_index - 1];
    buffer[1] = '\0';
    return make_string(buffer);
}

void set_string_char_value(Cell* target, int one_based_index, Value replacement)
{
    const char* existing = as_string(target->value);
    const char* replacement_text = as_string(replacement);
    size_t existing_length = strlen(existing);

    if (one_based_index < 1)
    {
        return;
    }

    size_t zero_based_index = (size_t)(one_based_index - 1);
    size_t new_length = existing_length > zero_based_index ? existing_length : (zero_based_index + 1);
    char* buffer = (char*)malloc(new_length + 1);
    if (!buffer) exit(1);

    memset(buffer, ' ', new_length);
    if (existing_length > 0)
    {
        memcpy(buffer, existing, existing_length);
    }

    buffer[zero_based_index] = replacement_text[0] ? replacement_text[0] : ' ';
    buffer[new_length] = '\0';

    target->value.type = TYPE_STRING;
    target->value.int_value = 0;
    target->value.float_value = 0.0;
    target->value.string_value = buffer;
    target->value.array_value = NULL;
}

static char* build_array_key(int count, va_list args)
{
    static char buffer[256];
    int i;
    buffer[0] = '\0';
    for (i = 0; i < count; i++)
    {
        Value value = va_arg(args, Value);
        char part[32];
        snprintf(part, sizeof(part), i == 0 ? "%d" : ",%d", as_int(value));
        strncat(buffer, part, sizeof(buffer) - strlen(buffer) - 1);
    }
    return buffer;
}

Cell* get_array_cell(Cell* array_cell, int count, ...)
{
    va_list args;
    char* key;
    va_start(args, count);
    key = build_array_key(count, args);
    va_end(args);
    if (array_cell->value.type != TYPE_ARRAY) array_cell->value = make_array();
    {
        ArrayEntry* entry;
        for (entry = array_cell->value.array_value; entry; entry = entry->next)
            if (strcmp(entry->key, key) == 0) return &entry->cell;
    }
    {
        ArrayEntry* entry = (ArrayEntry*)calloc(1, sizeof(ArrayEntry));
        if (!entry) exit(1);
        entry->key = duplicate_string(key);
        entry->cell = make_cell(make_null());
        entry->next = array_cell->value.array_value;
        array_cell->value.array_value = entry;
        return &entry->cell;
    }
}

Value get_array_value(Cell* array_cell, int count, ...)
{
    va_list args;
    va_start(args, count);
    {
        char* key = build_array_key(count, args);
        ArrayEntry* entry;
        if (array_cell->value.type != TYPE_ARRAY) array_cell->value = make_array();
        va_end(args);
        for (entry = array_cell->value.array_value; entry; entry = entry->next)
        {
            if (strcmp(entry->key, key) == 0) return entry->cell.value;
        }
    }
    return make_null();
}

void set_array_value(Cell* array_cell, Value value, int count, ...)
{
    va_list args;
    char* key;
    ArrayEntry* entry;
    va_start(args, count);
    key = build_array_key(count, args);
    va_end(args);
    if (array_cell->value.type != TYPE_ARRAY) array_cell->value = make_array();
    for (entry = array_cell->value.array_value; entry; entry = entry->next)
    {
        if (strcmp(entry->key, key) == 0) { entry->cell.value = value; return; }
    }
    entry = (ArrayEntry*)calloc(1, sizeof(ArrayEntry));
    if (!entry) exit(1);
    entry->key = duplicate_string(key);
    entry->cell = make_cell(value);
    entry->next = array_cell->value.array_value;
    array_cell->value.array_value = entry;
}

static long current_ql_seconds(void)
{
    return (long)time(NULL) + sb_ql_epoch_offset;
}

static struct tm ql_seconds_to_tm(long seconds)
{
    time_t unix_time = (time_t)(seconds - sb_ql_epoch_offset);
    struct tm result;
#if defined(_WIN32)
    gmtime_s(&result, &unix_time);
#else
    gmtime_r(&unix_time, &result);
#endif
    return result;
}

static Value make_formatted_time(long seconds, const char* format)
{
    static char buffer[64];
    struct tm time_parts = ql_seconds_to_tm(seconds);
    strftime(buffer, sizeof(buffer), format, &time_parts);
    return make_string(buffer);
}

static Value peek_memory(int address, int width)
{
    sb_runtime_init();
    switch (width)
    {
        case 1:
            return make_int((int)((unsigned char)SB_Peek(address)));
        case 2:
            return make_int((int)((unsigned short)SB_Peek_W(address)));
        case 4:
            return make_int((int)SB_Peek_L(address));
        default:
            runtime_not_supported("Unsupported memory width.");
            return make_null();
    }
}

static void poke_memory(int address, int value, int width)
{
    sb_runtime_init();
    switch (width)
    {
        case 1:
            SB_Poke(address, value);
            break;
        case 2:
            SB_Poke_W(address, value);
            break;
        case 4:
            SB_Poke_L(address, value);
            break;
        default:
            runtime_not_supported("Unsupported memory width.");
            break;
    }
}

static int normalize_channel_id(Value channel, int default_channel)
{
    return channel.type == TYPE_NULL ? default_channel : as_int(channel);
}

static FILE* resolve_writer(Value channel)
{
    sb_runtime_init();
    int channel_id = normalize_channel_id(channel, 1);
    return FNO(channel_id) ? FNO(channel_id) : stdout;
}

static FILE* resolve_reader(Value channel)
{
    sb_runtime_init();
    int channel_id = normalize_channel_id(channel, 0);
    return FNO(channel_id) ? FNO(channel_id) : stdin;
}

static void close_channel_id(int channel_id)
{
    sb_runtime_init();
    _SB_Close(&sb_channo[channel_id]);
}

static void open_channel_id(int channel_id, const char* path, const char* mode)
{
    sb_runtime_init();
    if (_SB_Open(&sb_channo[channel_id], (char*)path, (char*)mode) != 0)
        runtime_not_supported("Unable to open channel path.");
}

static void write_to_channel(Value channel, const char* text)
{
    FILE* writer = resolve_writer(channel);
    fputs(text, writer);
    fflush(writer);
}

static void write_line_to_channel(Value channel, const char* text)
{
    FILE* writer = resolve_writer(channel);
    fputs(text, writer);
    fputc('\n', writer);
    fflush(writer);
}

static char* trim_left_in_place(char* text)
{
    while (*text == ' ' || *text == '\t') text++;
    return text;
}

static int read_line_from_channel(Value channel, char* buffer, size_t size)
{
    FILE* reader = resolve_reader(channel);
    if (!fgets(buffer, (int)size, reader))
    {
        buffer[0] = '\0';
        return 0;
    }
    return 1;
}

static int channel_eof(int channel_id)
{
    sb_runtime_init();
    return FNO(channel_id) ? feof(FNO(channel_id)) : 1;
}

static void runtime_pause_ms(int milliseconds)
{
    if (milliseconds <= 0) return;
    clock_t end_time = clock() + (clock_t)((double)milliseconds * CLOCKS_PER_SEC / 1000.0);
    while (clock() < end_time) { }
}

static Value string_left(Value source_value, int length)
{
    const char* source = as_string(source_value);
    size_t source_length = strlen(source);
    size_t clamped = (size_t)((length < 0) ? 0 : length);
    if (clamped > source_length) clamped = source_length;
    return make_string(duplicate_substring(source, clamped));
}

static Value string_right(Value source_value, int length)
{
    const char* source = as_string(source_value);
    size_t source_length = strlen(source);
    size_t clamped = (size_t)((length < 0) ? 0 : length);
    if (clamped > source_length) clamped = source_length;
    return make_string(duplicate_substring(source + (source_length - clamped), clamped));
}

static Value string_mid(Value source_value, int start, int count, int has_count)
{
    const char* source = as_string(source_value);
    size_t source_length = strlen(source);
    size_t zero_based = (size_t)((start <= 1) ? 0 : start - 1);
    if (zero_based > source_length) zero_based = source_length;
    size_t available = source_length - zero_based;
    size_t length = has_count ? (size_t)((count < 0) ? 0 : count) : available;
    if (length > available) length = available;
    return make_string(duplicate_substring(source + zero_based, length));
}

static Value fill_string(Value text_value, int count)
{
    const char* text = as_string(text_value);
    size_t part_length = strlen(text);
    size_t total_length;
    char* buffer;
    char* current;
    int i;
    if (count <= 0 || part_length == 0) return make_string("");
    total_length = part_length * (size_t)count;
    buffer = (char*)malloc(total_length + 1);
    if (!buffer) exit(1);
    current = buffer;
    for (i = 0; i < count; i++)
    {
        memcpy(current, text, part_length);
        current += part_length;
    }
    buffer[total_length] = '\0';
    return make_string(buffer);
}

static Value repl_string(Value source_value, Value replacement_value, int start, int count, int has_count)
{
    const char* source = as_string(source_value);
    const char* replacement = as_string(replacement_value);
    size_t source_length = strlen(source);
    size_t replacement_length = strlen(replacement);
    size_t zero_based = (size_t)((start <= 1) ? 0 : start - 1);
    if (zero_based > source_length) zero_based = source_length;
    size_t remove_length = has_count ? (size_t)((count < 0) ? 0 : count) : replacement_length;
    if (remove_length > source_length - zero_based) remove_length = source_length - zero_based;
    size_t total_length = zero_based + replacement_length + (source_length - zero_based - remove_length);
    char* buffer = (char*)malloc(total_length + 1);
    if (!buffer) exit(1);
    memcpy(buffer, source, zero_based);
    memcpy(buffer + zero_based, replacement, replacement_length);
    memcpy(buffer + zero_based + replacement_length, source + zero_based + remove_length, source_length - zero_based - remove_length);
    buffer[total_length] = '\0';
    return make_string(buffer);
}

Value invoke_builtin_function(const char* name, int arg_count, ...)
{
    va_list args;
    sb_runtime_init();
    va_start(args, arg_count);

    if (ci_compare(name, "ABS") == 0) { Value v = va_arg(args, Value); va_end(args); return make_float(fabs(as_double(v))); }
    if (ci_compare(name, "ACOS") == 0) { Value v = va_arg(args, Value); va_end(args); return make_float(acos(as_double(v))); }
    if (ci_compare(name, "ACOT") == 0) { Value v = va_arg(args, Value); double n = as_double(v); va_end(args); return make_float(n == 0.0 ? (M_PI / 2.0) : atan(1.0 / n)); }
    if (ci_compare(name, "ADATE") == 0) { Value v = va_arg(args, Value); va_end(args); return make_int((int)(current_ql_seconds() + as_int(v))); }
    if (ci_compare(name, "ASC") == 0 || ci_compare(name, "CODE") == 0) { Value v = va_arg(args, Value); const char* text = as_string(v); va_end(args); return make_int(text[0] ? (unsigned char)text[0] : 0); }
    if (ci_compare(name, "ASIN") == 0) { Value v = va_arg(args, Value); va_end(args); return make_float(asin(as_double(v))); }
    if (ci_compare(name, "ATAN") == 0) { Value v = va_arg(args, Value); va_end(args); return make_float(atan(as_double(v))); }
    if (ci_compare(name, "CHR$") == 0) { Value v = va_arg(args, Value); static char text[2]; text[0] = (char)(as_int(v) & 255); text[1] = '\0'; va_end(args); return make_string(text); }
    if (ci_compare(name, "COS") == 0) { Value v = va_arg(args, Value); va_end(args); return make_float(cos(as_double(v))); }
    if (ci_compare(name, "COT") == 0) { Value v = va_arg(args, Value); va_end(args); return make_float(1.0 / tan(as_double(v))); }
    if (ci_compare(name, "DATE") == 0) { va_end(args); return make_int((int)SB_Date()); }
    if (ci_compare(name, "DATE$") == 0) { long seconds = (arg_count > 0) ? as_int(va_arg(args, Value)) : current_ql_seconds(); va_end(args); return make_formatted_time(seconds, "%Y %b %d %H:%M:%S"); }
    if (ci_compare(name, "DAY$") == 0) { long seconds = (arg_count > 0) ? as_int(va_arg(args, Value)) : current_ql_seconds(); va_end(args); return make_formatted_time(seconds, "%A"); }
    if (ci_compare(name, "DEG") == 0) { Value v = va_arg(args, Value); va_end(args); return make_float(as_double(v) * 180.0 / M_PI); }
    if (ci_compare(name, "EOF") == 0) { Value v = va_arg(args, Value); va_end(args); return make_int(channel_eof(as_int(v)) ? 1 : 0); }
    if (ci_compare(name, "ERLIN") == 0) { va_end(args); return make_int(sb_last_error_line()); }
    if (ci_compare(name, "ERNUM") == 0) { va_end(args); return make_int(sb_last_error_number()); }
    if (ci_compare(name, "EXP") == 0) { Value v = va_arg(args, Value); va_end(args); return make_float(exp(as_double(v))); }
    if (ci_compare(name, "ERR_NI") == 0) { va_end(args); return make_int(ci_compare(sb_last_error_name(), "ERR_NI") == 0); }
    if (ci_compare(name, "ERR_BP") == 0) { va_end(args); return make_int(ci_compare(sb_last_error_name(), "ERR_BP") == 0); }
    if (ci_compare(name, "ERR_FE") == 0) { va_end(args); return make_int(ci_compare(sb_last_error_name(), "ERR_FE") == 0); }
    if (ci_compare(name, "ERR_BL") == 0) { va_end(args); return make_int(ci_compare(sb_last_error_name(), "ERR_BL") == 0); }
    if (ci_compare(name, "FILL$") == 0) { Value text = va_arg(args, Value); Value count = va_arg(args, Value); va_end(args); return fill_string(text, as_int(count)); }
    if (ci_compare(name, "GETENV$") == 0) { Value v = va_arg(args, Value); const char* raw = getenv(as_string(v)); va_end(args); return make_string(raw ? raw : ""); }
    if (ci_compare(name, "INKEY") == 0) { va_end(args); return make_int(0); }
    if (ci_compare(name, "INKEY$") == 0) { va_end(args); return make_string(""); }
    if (ci_compare(name, "INT") == 0) { Value v = va_arg(args, Value); va_end(args); return make_int((int)floor(as_double(v))); }
    if (ci_compare(name, "KEYROW") == 0) { va_end(args); return make_int(0); }
    if (ci_compare(name, "LEN") == 0) { Value v = va_arg(args, Value); va_end(args); return make_int((int)strlen(as_string(v))); }
    if (ci_compare(name, "LEFT$") == 0) { Value text = va_arg(args, Value); Value count = va_arg(args, Value); va_end(args); return string_left(text, as_int(count)); }
    if (ci_compare(name, "LN") == 0) { Value v = va_arg(args, Value); va_end(args); return make_float(log(as_double(v))); }
    if (ci_compare(name, "LOG") == 0 || ci_compare(name, "LOG10") == 0) { Value v = va_arg(args, Value); va_end(args); return make_float(log10(as_double(v))); }
    if (ci_compare(name, "MID$") == 0)
    {
        Value text = va_arg(args, Value);
        Value start = va_arg(args, Value);
        Value count = arg_count > 2 ? va_arg(args, Value) : make_int(0);
        va_end(args);
        return string_mid(text, as_int(start), as_int(count), arg_count > 2);
    }
    if (ci_compare(name, "PEEK") == 0) { Value v = va_arg(args, Value); va_end(args); return peek_memory(as_int(v), 1); }
    if (ci_compare(name, "PEEK_W") == 0) { Value v = va_arg(args, Value); va_end(args); return peek_memory(as_int(v), 2); }
    if (ci_compare(name, "PEEK_L") == 0) { Value v = va_arg(args, Value); va_end(args); return peek_memory(as_int(v), 4); }
    if (ci_compare(name, "PI") == 0) { va_end(args); return make_float(M_PI); }
    if (ci_compare(name, "RAD") == 0) { Value v = va_arg(args, Value); va_end(args); return make_float(as_double(v) * M_PI / 180.0); }
    if (ci_compare(name, "RIGHT$") == 0) { Value text = va_arg(args, Value); Value count = va_arg(args, Value); va_end(args); return string_right(text, as_int(count)); }
    if (ci_compare(name, "RND") == 0)
    {
        if (arg_count == 0) { va_end(args); return make_float(SB_Rnd_f()); }
        Value v = va_arg(args, Value);
        va_end(args);
        {
            int upper = as_int(v);
            if (upper < 1) upper = 1;
            return make_int(SB_Rnd_i(1, upper));
        }
    }
    if (ci_compare(name, "REPL$") == 0)
    {
        Value source = va_arg(args, Value);
        Value replacement = va_arg(args, Value);
        Value start = va_arg(args, Value);
        Value count = arg_count > 3 ? va_arg(args, Value) : make_int(0);
        va_end(args);
        return repl_string(source, replacement, as_int(start), as_int(count), arg_count > 3);
    }
    if (ci_compare(name, "ROUND") == 0) { Value v = va_arg(args, Value); va_end(args); return make_int((int)llround(as_double(v))); }
    if (ci_compare(name, "SGN") == 0) { Value v = va_arg(args, Value); double n = as_double(v); va_end(args); return make_int(n > 0.0 ? 1 : (n < 0.0 ? -1 : 0)); }
    if (ci_compare(name, "SIN") == 0) { Value v = va_arg(args, Value); va_end(args); return make_float(sin(as_double(v))); }
    if (ci_compare(name, "SQRT") == 0) { Value v = va_arg(args, Value); va_end(args); return make_float(sqrt(as_double(v))); }
    if (ci_compare(name, "STR$") == 0) { Value v = va_arg(args, Value); va_end(args); return make_string(as_string(v)); }
    if (ci_compare(name, "TAN") == 0) { Value v = va_arg(args, Value); va_end(args); return make_float(tan(as_double(v))); }
    if (ci_compare(name, "TIME") == 0)
    {
        time_t now = time(NULL);
        struct tm current_time;
#if defined(_WIN32)
        localtime_s(&current_time, &now);
#else
        localtime_r(&now, &current_time);
#endif
        int seconds = current_time.tm_hour * 3600 + current_time.tm_min * 60 + current_time.tm_sec;
        if (arg_count > 0) seconds += as_int(va_arg(args, Value));
        va_end(args);
        seconds %= 86400;
        if (seconds < 0) seconds += 86400;
        return make_int(seconds);
    }
    if (ci_compare(name, "VAL") == 0) { Value v = va_arg(args, Value); va_end(args); return make_float(atof(as_string(v))); }

    va_end(args);
    {
        char message[256];
        snprintf(message, sizeof(message), "Built-in function '%s' is not supported by the generated C backend yet.", name);
        runtime_not_supported(message);
    }
    return make_null();
}

void execute_builtin_statement(const char* name, Value channel, int arg_count, ...)
{
    va_list args;
    sb_runtime_init();
    va_start(args, arg_count);

    if (ci_compare(name, "PRINT") == 0)
    {
        FILE* writer = resolve_writer(channel);
        int i;
        for (i = 0; i < arg_count; i++)
        {
            Value value = va_arg(args, Value);
            fputs(as_string(value), writer);
            if (i + 1 < arg_count) fputc(' ', writer);
        }
        fputc('\n', writer);
        fflush(writer);
        va_end(args);
        return;
    }

    if (ci_compare(name, "REFERENCE") == 0) { va_end(args); return; }
    if (ci_compare(name, "RANDOMISE") == 0)
    {
        double seed = arg_count == 0 ? (double)time(NULL) : (double)as_int(va_arg(args, Value));
        _SB_Randomise(seed);
        va_end(args);
        return;
    }
    if (ci_compare(name, "MODE") == 0) { SB_Mode(arg_count == 0 ? 4 : as_int(va_arg(args, Value))); va_end(args); return; }
    if (ci_compare(name, "PAUSE") == 0) { runtime_pause_ms(arg_count == 0 ? 0 : as_int(va_arg(args, Value))); va_end(args); return; }
    if (ci_compare(name, "BEEP") == 0) { fputc('\a', stdout); fflush(stdout); va_end(args); return; }
    if (ci_compare(name, "POKE") == 0) { Value address = va_arg(args, Value); Value value = va_arg(args, Value); va_end(args); poke_memory(as_int(address), as_int(value), 1); return; }
    if (ci_compare(name, "POKE_W") == 0) { Value address = va_arg(args, Value); Value value = va_arg(args, Value); va_end(args); poke_memory(as_int(address), as_int(value), 2); return; }
    if (ci_compare(name, "POKE_L") == 0) { Value address = va_arg(args, Value); Value value = va_arg(args, Value); va_end(args); poke_memory(as_int(address), as_int(value), 4); return; }
    if (ci_compare(name, "OPEN") == 0 || ci_compare(name, "OPEN_NEW") == 0)
    {
        Value path = va_arg(args, Value);
        int channel_id = normalize_channel_id(channel, 3);
        open_channel_id(channel_id, as_string(path), ci_compare(name, "OPEN_NEW") == 0 ? "n+" : "r+");
        va_end(args);
        return;
    }
    if (ci_compare(name, "OPEN_IN") == 0)
    {
        Value path = va_arg(args, Value);
        int channel_id = normalize_channel_id(channel, 3);
        open_channel_id(channel_id, as_string(path), "r");
        va_end(args);
        return;
    }
    if (ci_compare(name, "APPEND") == 0)
    {
        Value path = va_arg(args, Value);
        int channel_id = normalize_channel_id(channel, 3);
        open_channel_id(channel_id, as_string(path), "a");
        va_end(args);
        return;
    }
    if (ci_compare(name, "CLOSE") == 0) { close_channel_id(normalize_channel_id(channel, 3)); va_end(args); return; }
    if (ci_compare(name, "DIR") == 0)
    {
        FILE* writer = resolve_writer(channel);
        char* target = arg_count > 0 ? (char*)as_string(va_arg(args, Value)) : "*";
        SB_Dir(writer, target);
        va_end(args);
        return;
    }
    if (ci_compare(name, "CLS") == 0) { SB_Cls(resolve_writer(channel), 0); va_end(args); return; }
    if (ci_compare(name, "REPORT") == 0)
    {
        FILE* writer = resolve_writer(channel);
        if (arg_count == 0) fputs(sb_last_error_description(), writer);
        else fputs(as_string(va_arg(args, Value)), writer);
        fputc('\n', writer);
        fflush(writer);
        va_end(args);
        return;
    }
    if (ci_compare(name, "WINDOW") == 0)
    {
        FILE* writer = resolve_writer(channel);
        int w = arg_count > 0 ? as_int(va_arg(args, Value)) : 0;
        int h = arg_count > 1 ? as_int(va_arg(args, Value)) : 0;
        int x = arg_count > 2 ? as_int(va_arg(args, Value)) : 0;
        int y = arg_count > 3 ? as_int(va_arg(args, Value)) : 0;
        _SB_Window(writer, (short)w, (short)h, (short)x, (short)y);
        va_end(args);
        return;
    }
    if (ci_compare(name, "AT") == 0)
    {
        FILE* writer = resolve_writer(channel);
        int line = arg_count > 0 ? as_int(va_arg(args, Value)) : 0;
        int column = arg_count > 1 ? as_int(va_arg(args, Value)) : 0;
        _SB_At(writer, (short)line, (short)column);
        va_end(args);
        return;
    }
    if (ci_compare(name, "INK") == 0) { _SB_Ink(resolve_writer(channel), (colour_t)as_int(va_arg(args, Value))); va_end(args); return; }
    if (ci_compare(name, "PAPER") == 0) { _SB_Paper(resolve_writer(channel), (colour_t)as_int(va_arg(args, Value))); va_end(args); return; }
    if (ci_compare(name, "BORDER") == 0)
    {
        FILE* writer = resolve_writer(channel);
        int width = arg_count > 0 ? as_int(va_arg(args, Value)) : 1;
        int color = arg_count > 1 ? as_int(va_arg(args, Value)) : 0;
        _SB_Border(writer, (short)width, (colour_t)color);
        va_end(args);
        return;
    }
    if (ci_compare(name, "LINE") == 0)
    {
        FILE* writer = resolve_writer(channel);
        int x1 = as_int(va_arg(args, Value));
        int y1 = as_int(va_arg(args, Value));
        int x2 = as_int(va_arg(args, Value));
        int y2 = as_int(va_arg(args, Value));
        SB_Line(writer, x1, y1, x2, y2);
        va_end(args);
        return;
    }
    if (ci_compare(name, "CIRCLE") == 0)
    {
        FILE* writer = resolve_writer(channel);
        int x = as_int(va_arg(args, Value));
        int y = as_int(va_arg(args, Value));
        int radius = as_int(va_arg(args, Value));
        SB_Circle(writer, x, y, radius);
        va_end(args);
        return;
    }
    if (ci_compare(name, "BLOCK") == 0)
    {
        FILE* writer = resolve_writer(channel);
        int w = as_int(va_arg(args, Value));
        int h = as_int(va_arg(args, Value));
        int x = as_int(va_arg(args, Value));
        int y = as_int(va_arg(args, Value));
        int color = arg_count > 4 ? as_int(va_arg(args, Value)) : 0;
        _SB_Block(writer, (short)w, (short)h, (short)x, (short)y, (colour_t)color);
        va_end(args);
        return;
    }
    if (ci_compare(name, "OVER") == 0) { SB_Over(resolve_writer(channel), as_int(va_arg(args, Value))); va_end(args); return; }
    if (ci_compare(name, "UNDER") == 0) { SB_Under(resolve_writer(channel), as_int(va_arg(args, Value))); va_end(args); return; }
    if (ci_compare(name, "FLASH") == 0) { SB_Flash(resolve_writer(channel), as_int(va_arg(args, Value))); va_end(args); return; }
    if (ci_compare(name, "WINDOW") == 0 || ci_compare(name, "AT") == 0 || ci_compare(name, "CURSOR") == 0
        || ci_compare(name, "CSIZE") == 0 || ci_compare(name, "CHAR_USE") == 0 || ci_compare(name, "S_FONT") == 0
        || ci_compare(name, "INK") == 0 || ci_compare(name, "PAPER") == 0 || ci_compare(name, "BORDER") == 0
        || ci_compare(name, "CLEAR") == 0 || ci_compare(name, "SCROLL") == 0 || ci_compare(name, "WIDTH") == 0
        || ci_compare(name, "PAN") == 0 || ci_compare(name, "RECOL") == 0 || ci_compare(name, "PALETTE") == 0
        || ci_compare(name, "PLOT") == 0 || ci_compare(name, "POINT") == 0 || ci_compare(name, "POINT_R") == 0
        || ci_compare(name, "DRAW") == 0 || ci_compare(name, "DLINE") == 0 || ci_compare(name, "LINE") == 0
        || ci_compare(name, "LINE_R") == 0 || ci_compare(name, "CIRCLE") == 0 || ci_compare(name, "CIRCLE_R") == 0
        || ci_compare(name, "ELLIPSE") == 0 || ci_compare(name, "ELLIPSE_R") == 0 || ci_compare(name, "ARC") == 0
        || ci_compare(name, "ARC_R") == 0 || ci_compare(name, "BLOCK") == 0 || ci_compare(name, "FILL") == 0
        || ci_compare(name, "SCALE") == 0 || ci_compare(name, "OVER") == 0 || ci_compare(name, "UNDER") == 0
        || ci_compare(name, "FLASH") == 0 || ci_compare(name, "PENDOWN") == 0 || ci_compare(name, "PENUP") == 0
        || ci_compare(name, "TURN") == 0 || ci_compare(name, "TURNTO") == 0 || ci_starts_with(name, "TURBO"))
    {
        va_end(args);
        return;
    }

    va_end(args);
    runtime_not_supported("Built-in statement is not supported by the generated C backend yet.");
}

void execute_input(Value channel, int prompt_count, ...)
{
    va_list args;
    int i;
    char* token;
    sb_runtime_init();
    va_start(args, prompt_count);
    for (i = 0; i < prompt_count; i++)
    {
        Value prompt = va_arg(args, Value);
        write_to_channel(channel, as_string(prompt));
        if (i + 1 < prompt_count) write_to_channel(channel, " ");
    }
    va_end(args);
    if (!read_line_from_channel(channel, sb_input_buffer, sizeof(sb_input_buffer))) sb_input_buffer[0] = '\0';
    sb_input_part_count = 0;
    token = strtok(sb_input_buffer, ",\r\n");
    while (token && sb_input_part_count < 256)
    {
        sb_input_parts[sb_input_part_count++] = trim_left_in_place(token);
        token = strtok(NULL, ",\r\n");
    }
}

Value read_input_value(int index, ValueType target_type)
{
    const char* raw = index < sb_input_part_count ? sb_input_parts[index] : "";
    switch (target_type)
    {
        case TYPE_STRING: return make_string(raw);
        case TYPE_FLOAT: return make_float(atof(raw));
        default: return make_int(atoi(raw));
    }
}

Value read_data_value(ValueType target_type)
{
    Value value;
    if (sb_data_pointer >= sb_data_count) runtime_not_supported("READ moved past the end of DATA.");
    value = sb_data[sb_data_pointer++];
    switch (target_type)
    {
        case TYPE_STRING: return make_string(as_string(value));
        case TYPE_FLOAT: return make_float(as_double(value));
        default: return make_int(as_int(value));
    }
}

void restore_to_line(int line)
{
    int i;
    for (i = 0; i < sb_restore_point_count; i++)
    {
        if (sb_restore_points[i].line == line) { sb_data_pointer = sb_restore_points[i].slot; return; }
    }
    runtime_not_supported("RESTORE target does not exist in generated C backend.");
}

