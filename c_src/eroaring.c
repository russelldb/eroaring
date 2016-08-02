/* order matters */
#include <stddef.h>
static void * calloc(size_t cnt, size_t sz);
static void   free(void * p);
static void * malloc(size_t sz);
static void * realloc(void * p, size_t sz);
#include <string.h>

#include "erl_nif.h"

static void * calloc(size_t cnt, size_t sz)
{
    /* assume 64-bit alignment */
    size_t asz = (cnt * ((sz + 7) & ~7));
    return memset(enif_alloc(asz), 0, asz);
}
static void   free(void * p)
{
    enif_free(p);
}
static void * malloc(size_t sz)
{
    return enif_alloc(sz);
}
static void * realloc(void * p, size_t sz)
{
    return enif_realloc(p, sz);
}

#include "../roaring/roaring.c"

typedef struct {
  roaring_bitmap_t * bits;
} eroaring_t;

/*
 * Create a new eroaring_t containing 'bits', which may be NULL.
 * Returns NULL on error.
 */
static eroaring_t * resource_ctor(roaring_bitmap_t * bits);

/*
 * Deletes the contents of ((eroaring_t *) obj)->bits.
 * DOES NOT delete the resource object itself, that's handled by the VM.
 * You probably don't ever want to call this function directly.
 */
static void resource_dtor(ErlNifEnv * env, void * obj);

/*
 * Transfers ownership of 'res' to the VM.
 * Returns an Erlang term that can be passed out of the NIF.
 * Use get_term_resource(env, res) to get a C pointer to the eroaring_t.
 */
static ERL_NIF_TERM resource_to_term(ErlNifEnv * env, eroaring_t * res);

/*
 * Get the C pointer to the eroaring_t represented by 'term'.
 * 'term' must have originally been created by resource_to_term(env, res) and
 * may have been passed between the VM and NIF any number of times.
 * Returns NULL if 'term' was not created by resource_to_term(env, res).
 */
static eroaring_t * get_term_resource(ErlNifEnv * env, ERL_NIF_TERM term);

/*
 * Atoms.
 */
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_ENOMEM;
static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_FALSE;



// The actual C implementation of the Erlang functions.
//
// Docs: http://erlang.org/doc/man/erl_nif.html#ErlNifFunc

/*
 * eroaring:new() -> eroaring:bits()
 */
static ERL_NIF_TERM
eroaring_new(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    roaring_bitmap_t *  bits;
    eroaring_t *        res;

    if ((bits = roaring_bitmap_create()) == NULL)
        return  enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);

    if ((res = resource_ctor(bits)) == NULL)
        return  enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);

    return  resource_to_term(env, res);
}

/*
 * eroaring:run_optimize(eroaring:bits()) -> eroaring:bits()
 * Returns an optimized COPY of the input resource.
 */
static ERL_NIF_TERM
eroaring_run_optimize(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    roaring_bitmap_t *  bits;
    eroaring_t *        res_in;
    eroaring_t *        res_out;

    if (argc != 1 || (res_in = get_term_resource(env, *argv)) == NULL)
        return  enif_make_badarg(env);

    /* @TED? Why do we copy? what happens to the old one? */
    if ((bits = roaring_bitmap_copy(res_in->bits)) == NULL)
        return  enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);

    /* does anyone care about the result? */
    roaring_bitmap_run_optimize(bits);

    if ((res_out = resource_ctor(bits)) == NULL)
        return  enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);

    return  resource_to_term(env, res_out);
}

/*
 * eroaring:cardinality(eroaring:bits()) -> long
 * Returns the cardinality of the bitset
 */
static ERL_NIF_TERM
eroaring_cardinality(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
  eroaring_t *        res_in;

    if (argc != 1 || (res_in = get_term_resource(env, *argv)) == NULL)
        return  enif_make_badarg(env);

    return  enif_make_long(env, roaring_bitmap_get_cardinality(res_in->bits));
}

/*
 * eroaring:add(int, eroaring:bits()) -> eroaring:bits()
 * Returns the input resource with int added
 */
static ERL_NIF_TERM
eroaring_add(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    roaring_bitmap_t *  bits;
    eroaring_t *        res_in;
    eroaring_t *        res_out;
    long int            to_add;

    if (argc != 2 || (res_in = get_term_resource(env, *argv)) == NULL ||
        !enif_get_long(env, argv[1], &to_add))
        return  enif_make_badarg(env);

    /* Why do we copy? what happens to the old one? */
    if ((bits = roaring_bitmap_copy(res_in->bits)) == NULL)
        return  enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);

    /* does anyone care about the result? */
    roaring_bitmap_add(bits, to_add);

    if ((res_out = resource_ctor(bits)) == NULL)
        return  enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);

    return  resource_to_term(env, res_out);
}

/*
 * eroaring:add(int, eroaring:bits()) -> eroaring:bits()
 * Returns the input resource with int added
 */
static ERL_NIF_TERM
eroaring_add_all(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
  roaring_bitmap_t *  bits;
  eroaring_t *        res_in;
  eroaring_t *        res_out;
  ERL_NIF_TERM        to_add;
  ERL_NIF_TERM head, tail;
  long bit;

  if (argc != 2 || (res_in = get_term_resource(env, *argv)) == NULL ||
      !enif_is_list(env, argv[1])) {
    return  enif_make_badarg(env);
  } else {
    to_add = argv[1];
  }

  /* Why do we copy? what happens to the old one? */
  if ((bits = roaring_bitmap_copy(res_in->bits)) == NULL)
    return  enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);

  /* @TODO There must be a better way than iterating the list like
     this? */
  while(enif_get_list_cell(env, to_add, &head, &tail)) {
    if(!enif_get_long(env, head, &bit)) {
    return enif_make_badarg(env);
  }
  /* does anyone care about the result? */
  roaring_bitmap_add(bits, bit);
  to_add = tail;
}

  if ((res_out = resource_ctor(bits)) == NULL)
    return  enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);

  return  resource_to_term(env, res_out);
}


/*
 * eroaring:serialize(eroaring:bits()) -> binary()
 * Returns an serialized representation of the input resource.
 */
static ERL_NIF_TERM
eroaring_serialize(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    roaring_bitmap_t *  bits;
    eroaring_t *        res_in;
    ErlNifBinary        output_binary;

    if (argc != 1 || (res_in = get_term_resource(env, *argv)) == NULL)
        return  enif_make_badarg(env);

    if ((bits = res_in->bits) == NULL)
        return  enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);

    uint32_t expectedsize = roaring_bitmap_portable_size_in_bytes(bits);
    enif_alloc_binary(expectedsize, &output_binary);

    roaring_bitmap_portable_serialize(bits, output_binary.data);

    return enif_make_binary(env, &output_binary);
}

/*
 * eroaring:deserialize(binary()) -> eroaring:bits()
 * Returns a resource de-serialized from a binary.
 */
static ERL_NIF_TERM
eroaring_deserialize(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
  roaring_bitmap_t *  bits;
  eroaring_t *        res_out;
  ErlNifBinary        input_binary;

  if (argc != 1 || !enif_inspect_iolist_as_binary(env, argv[0], &input_binary)) {
    return enif_make_badarg(env);
  }

  bits = roaring_bitmap_portable_deserialize(input_binary.data);


  if ((res_out = resource_ctor(bits)) == NULL)
    return  enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);

  return  resource_to_term(env, res_out);
}

/*
 * eroaring:contains(int, eroaring:bits()) -> boolean() Returns
 * boolean true if bits contains int, false otherwise
 */
static ERL_NIF_TERM
eroaring_contains(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
  roaring_bitmap_t *  bits;
  eroaring_t *        res_in;
  long int            contains;

  if (argc != 2 || (res_in = get_term_resource(env, *argv)) == NULL ||
      !enif_get_long(env, argv[1], &contains))
    return  enif_make_badarg(env);

  if ((bits = res_in->bits) == NULL)
    return  enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);

  if( roaring_bitmap_contains(bits, contains)) {
    return ATOM_TRUE;
  } else {
    return ATOM_FALSE;
  }

}

/*
 * eroaring:remove(int, eroaring:bits()) -> eroaring:bits()
 * Returns the input resource with int removed
 */
static ERL_NIF_TERM
eroaring_remove(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    roaring_bitmap_t *  bits;
    eroaring_t *        res_in;
    eroaring_t *        res_out;
    long int            to_rem;

    if (argc != 2 || (res_in = get_term_resource(env, *argv)) == NULL ||
        !enif_get_long(env, argv[1], &to_rem))
        return  enif_make_badarg(env);

    /* Why do we copy? what happens to the old one? */
    if ((bits = roaring_bitmap_copy(res_in->bits)) == NULL)
        return  enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);

    /* does anyone care about the result? */
    roaring_bitmap_remove(bits, to_rem);

    if ((res_out = resource_ctor(bits)) == NULL)
        return  enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);

    return  resource_to_term(env, res_out);
}

/* Library and resource initialization, never called directly */

static ErlNifFunc nif_funcs[] = {
    {"new", 0, eroaring_new},
    {"run_optimize", 1, eroaring_run_optimize},
    {"add", 2, eroaring_add},
    {"add_all", 2, eroaring_add_all},
    {"cardinality", 1, eroaring_cardinality},
    {"serialize", 1, eroaring_serialize},
    {"deserialize", 1, eroaring_deserialize},
    {"contains", 2, eroaring_contains},
    {"remove", 2, eroaring_remove}
};

static int init_resource_type(ErlNifEnv * env);

static void init_atom(
    ErlNifEnv * env, const char * name, ERL_NIF_TERM * dest)
{
    if (! enif_make_existing_atom(env, name, dest, ERL_NIF_LATIN1))
        *dest = enif_make_atom(env, name);
}

static int load(
    ErlNifEnv * env, void ** priv, ERL_NIF_TERM load_info)
{
    int rc = init_resource_type(env);
    if (rc == 0)
    {
        /* initialize atoms and whatnot */
        init_atom(env, "error", &ATOM_ERROR);
        init_atom(env, "enomem", &ATOM_ENOMEM);
        init_atom(env, "true", &ATOM_TRUE);
        init_atom(env, "false", &ATOM_FALSE);
    }
    return  rc;
}

static int upgrade(
    ErlNifEnv * env, void ** priv, void ** old_priv, ERL_NIF_TERM load_info)
{
    /* take ownership of the resource type */
    return  init_resource_type(env);
}

static void unload(ErlNifEnv * env, void * priv)
{
}

// Initialize this NIF library.
//
// Args: (MODULE, ErlNifFunc funcs[], load, reload, upgrade, unload)
// Docs: http://erlang.org/doc/man/erl_nif.html#ERL_NIF_INIT

ERL_NIF_INIT(eroaring, nif_funcs, load, NULL, upgrade, unload);

/* resource functions */

static ErlNifResourceType * EROARING_RESOURCE_TYPE = NULL;

static eroaring_t * resource_ctor(roaring_bitmap_t * bits)
{
    eroaring_t * res = enif_alloc_resource(
        EROARING_RESOURCE_TYPE, sizeof(eroaring_t));
    res->bits = bits;
    return  res;
}

static void resource_dtor(ErlNifEnv * env, void * obj)
{
    eroaring_t * res = ((eroaring_t *) obj);
    if (res->bits != NULL)
    {
        roaring_bitmap_free(res->bits);
        res->bits = NULL;
    }
}

static ERL_NIF_TERM resource_to_term(ErlNifEnv * env, eroaring_t * res)
{
    ERL_NIF_TERM term = enif_make_resource(env, res);
    enif_release_resource(res);
    return  term;
}

static eroaring_t * get_term_resource(ErlNifEnv * env, ERL_NIF_TERM term)
{
    void * obj;
    if (enif_get_resource(env, term, EROARING_RESOURCE_TYPE, &obj))
        return  ((eroaring_t *) obj);
    else
        return  NULL;
}

static int init_resource_type(ErlNifEnv * env)
{
    ErlNifResourceFlags flags = (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    EROARING_RESOURCE_TYPE = enif_open_resource_type(
        env, NULL, "eroaring_t", resource_dtor, flags, &flags);
    return  (EROARING_RESOURCE_TYPE == NULL) ? -1 : 0;
}

