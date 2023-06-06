#include <janet.h>
#include <string.h>

struct effect {
  Janet name;
  Janet operations;
};

static int effect_gcmark(void* data, size_t len) {
  (void) len;
  struct effect* eff = (struct effect*) data;
  janet_mark(eff->name);
  janet_mark(eff->operations);
  return 0;
}

static int effect_get(void* data, Janet key, Janet* out) {
  struct effect* eff = (struct effect*) data;

  Janet result = janet_get(eff->operations, key);
  if (janet_checktype(result, JANET_NIL)) {
    return 0;
  }

  *out = result;
  return 1;
}

static void effect_marshal(void* p, JanetMarshalContext* ctx) {
  struct effect* eff = (struct effect*) p;
  janet_marshal_abstract(ctx, p);
  janet_marshal_janet(ctx, eff->name);
  janet_marshal_janet(ctx, eff->operations);
}

static void* effect_unmarshal(JanetMarshalContext* ctx) {
  struct effect* eff =
      (struct effect*) janet_unmarshal_abstract(ctx, sizeof(struct effect));
  eff->name = janet_unmarshal_janet(ctx);
  eff->operations = janet_unmarshal_janet(ctx);
  return eff;
}

static void effect_tostring(void* p, JanetBuffer* buffer) {
  struct effect* eff = (struct effect*) p;
  janet_formatb(buffer, "%V", eff->name);
}

Janet effect_next(void* p, Janet key) {
  struct effect* eff = (struct effect*) p;
  return janet_next(eff->operations, key);
}

size_t effect_length(void* p, size_t len) {
  struct effect* eff = (struct effect*) p;
  return janet_length(eff->operations);
}

static const JanetAbstractType effect_at = {
    .name = "effect",
    .gcmark = effect_gcmark,
    .get = effect_get,
    .marshal = effect_marshal,
    .unmarshal = effect_unmarshal,
    .tostring = effect_tostring,
    .next = effect_next,
    .length = effect_length,
};

struct operation {
  Janet effect;
  Janet name;
  Janet invoke;
  int32_t min_arity;
  int32_t max_arity;
};

static int operation_gcmark(void* data, size_t len) {
  (void) len;
  struct operation* op = (struct operation*) data;
  janet_mark(op->effect);
  janet_mark(op->name);
  janet_mark(op->invoke);
  return 0;
}

static void operation_marshal(void* p, JanetMarshalContext* ctx) {
  struct operation* op = (struct operation*) p;
  janet_marshal_abstract(ctx, p);
  janet_marshal_janet(ctx, op->effect);
  janet_marshal_janet(ctx, op->name);
  janet_marshal_int(ctx, op->min_arity);
  janet_marshal_int(ctx, op->max_arity);
}

static void* operation_unmarshal(JanetMarshalContext* ctx) {
  struct operation* op = (struct operation*) janet_unmarshal_abstract(
      ctx, sizeof(struct operation));
  op->effect = janet_unmarshal_janet(ctx);
  op->name = janet_unmarshal_janet(ctx);
  op->min_arity = janet_unmarshal_int(ctx);
  op->max_arity = janet_unmarshal_int(ctx);
  return op;
}

static void operation_tostring(void* p, JanetBuffer* buffer) {
  struct operation* op = (struct operation*) p;
  struct effect* eff = (struct effect*) janet_unwrap_abstract(op->effect);

  if (!janet_equals(op->name, eff->name)) {
    janet_formatb(buffer, "%V/", eff->name);
  }

  janet_formatb(buffer, "%V", op->name);
}

static Janet operation_callable(void* p) {
  struct operation* op = (struct operation*) p;
  return op->invoke;
}

static const JanetAbstractType operation_at = {
    .name = "operation",
    .gcmark = operation_gcmark,
    .marshal = operation_marshal,
    .unmarshal = operation_unmarshal,
    .tostring = operation_tostring,
    .callable = operation_callable,
};

static Janet cfun_effect(int32_t argc, Janet* argv) {
  janet_fixarity(argc, 3);

  if (!janet_checktype(argv[0], JANET_KEYWORD)) {
    janet_panicf("expected keyword for name, got %v", argv[0]);
  }

  struct effect* eff = janet_abstract(&effect_at, sizeof(struct effect));
  Janet effobj = janet_wrap_abstract(eff);

  eff->name = argv[0];

  int32_t len = janet_length(argv[1]);
  JanetTable* operations = janet_table(len);
  for (int i = 0; i < len; i++) {
    Janet entry = janet_in(argv[1], janet_wrap_integer(i));
    if (!janet_checktype(entry, JANET_TUPLE) || janet_length(entry) != 3) {
      janet_panicf(
          "operation spec must be a sequence of tuples of operation names "
          "(keywords) and arities (eg, [[name min-arity max-arity]]). got: %V",
          entry);
    }

    Janet name = janet_in(entry, janet_wrap_integer(0));
    Janet min_arity = janet_in(entry, janet_wrap_integer(1));
    Janet max_arity = janet_in(entry, janet_wrap_integer(2));

    if (!janet_checktype(name, JANET_KEYWORD)) {
      janet_panicf("operation name must be a keyword.  got :%V", name);
    }
    if (!janet_checktype(min_arity, JANET_NUMBER)) {
      janet_panicf("min-arity name must be a number.  got :%V", min_arity);
    }
    if (!janet_checktype(max_arity, JANET_NUMBER)) {
      janet_panicf("max-arity name must be a number.  got :%V", max_arity);
    }

    // TODO: Support JANET_CFUNCTION
    if (!janet_checktype(argv[2], JANET_FUNCTION)) {
      janet_panicf("expected fuction for invoke, got %v", argv[2]);
    }

    struct operation* op =
        janet_abstract(&operation_at, sizeof(struct operation));

    op->effect = janet_wrap_abstract(eff);
    op->name = name;
    op->min_arity = janet_unwrap_integer(min_arity);
    op->max_arity = janet_unwrap_integer(max_arity);
    op->invoke = argv[2];

    janet_table_put(operations, name, janet_wrap_abstract(op));
  }

  eff->operations = janet_wrap_struct(janet_table_to_struct(operations));

  return effobj;
}

static Janet cfun_effect_name(int32_t argc, Janet* argv) {
  janet_fixarity(argc, 1);

  struct effect* eff = janet_getabstract(argv, 0, &effect_at);
  return eff->name;
}

static Janet cfun_op_effect(int32_t argc, Janet* argv) {
  janet_fixarity(argc, 1);

  struct operation* op = janet_getabstract(argv, 0, &operation_at);
  return op->effect;
}

static Janet cfun_op_name(int32_t argc, Janet* argv) {
  janet_fixarity(argc, 1);

  struct operation* op = janet_getabstract(argv, 0, &operation_at);
  return op->name;
}

static Janet cfun_op_check_arity(int32_t argc, Janet* argv) {
  janet_fixarity(argc, 2);

  struct operation* op = janet_getabstract(argv, 0, &operation_at);
  int32_t check_argc = janet_getinteger(argv, 1);

  if (op->min_arity != -1 && op->min_arity == op->max_arity) {
    janet_fixarity(check_argc, op->min_arity);
  } else {
    janet_arity(check_argc, op->min_arity, op->max_arity);
  }

  return janet_wrap_nil();
}

static const JanetReg cfuns[] = {
    {"effect", cfun_effect, "Create an effect."},
    {"effect/name", cfun_effect_name, "Get the name of an effect."},

    {"op/effect", cfun_op_effect, "Get the effect an operation is part of."},
    {"op/name", cfun_op_name, "Get the name of an operation."},
    {"op/check-arity", cfun_op_check_arity, ""},
    {NULL, NULL, NULL},
};

void abstract_register_type(JanetTable* env) {
  (void) env;
  janet_register_abstract_type(&effect_at);
  janet_register_abstract_type(&operation_at);
}

void abstract_register_functions(JanetTable* env) {
  janet_cfuns(env, "effuse/internal", cfuns);
}

JANET_MODULE_ENTRY(JanetTable* env) {
  abstract_register_type(env);
  abstract_register_functions(env);
}
