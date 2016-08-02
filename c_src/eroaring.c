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
static ERL_NIF_TERM ATOM_INTERNAL;
static ERL_NIF_TERM ATOM_BADMATCH;


// The actual C implementation of the Erlang functions.
//
// Docs: http://erlang.org/doc/man/erl_nif.html#ErlNifFunc

/*
 * eroaring:new() -> eroaring:bits()
 * eroaring:new(Size :: pos_integer()) -> eroaring:bits()
 * eroaring:new(From :: non_neg_integer(), To :: pos_integer())
 *  -> eroaring:bits()
 */
static ERL_NIF_TERM
eroaring_new(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    roaring_bitmap_t *  bits;
    eroaring_t *        res;

    switch (argc)
    {
        case 0 :
            bits = roaring_bitmap_create();
            break;
        case 1 :
        {
            unsigned    size;
            if (! enif_get_uint(env, argv[0], &size))
                return  enif_make_badarg(env);
            bits = roaring_bitmap_create_with_capacity(size);
            break;
        }
        case 2 :
        {
            unsigned    lo, hi;
            if (! enif_get_uint(env, argv[0], &lo)
            ||  ! enif_get_uint(env, argv[1], &hi)
            ||  lo >= hi)
                return  enif_make_badarg(env);
            if (++hi == 0)  /* check for wraparound, just in case */
                --hi;
            bits = roaring_bitmap_from_range(lo, hi, 1);
            break;
        }
        default :
            return  enif_make_badarg(env);
    }
    if (bits == NULL)
        return  enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);

    if ((res = resource_ctor(bits)) == NULL)
    {
        /* resource allocation failed, need to clean up the 'bits' */
        roaring_bitmap_free(bits);
        return  enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
    }

    return  resource_to_term(env, res);
}

/*
 * eroaring:run_optimize(eroaring:bits()) -> eroaring:bits()
 * Modifies the bitmap IN PLACE!
 */
static ERL_NIF_TERM
eroaring_run_optimize(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    eroaring_t *    res;

    if (argc != 1 || (res = get_term_resource(env, *argv)) == NULL)
        return  enif_make_badarg(env);

    /*
     * Returns true/false indicating whether 'bits' contains any optimizable
     * runs, but do we care?
     */
    roaring_bitmap_run_optimize(res->bits);

    return  *argv;
}

/*
 * eroaring:cardinality(eroaring:bits()) -> non_neg_integer()
 * Returns the cardinality of the bitset
 */
static ERL_NIF_TERM
eroaring_cardinality(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    eroaring_t *    res;
    ErlNifUInt64    card;

    if (argc != 1 || (res = get_term_resource(env, *argv)) == NULL)
        return  enif_make_badarg(env);

    card = roaring_bitmap_get_cardinality(res->bits);
    return  enif_make_uint64(env, card);
}

/*
 * eroaring:add(eroaring:bits(), non_neg_integer() | [non_neg_integer()])
 *  -> eroaring:bits()
 * Returns the input resource with the specified bit(s) added
 */
static ERL_NIF_TERM
eroaring_add(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    eroaring_t *    res;
    unsigned        num;

    if (argc != 2 || (res = get_term_resource(env, *argv)) == NULL)
        return  enif_make_badarg(env);

    if (enif_get_list_length(env, argv[1], &num))
    {
        /*
         * Check all of the values before adding any of them to the bitset.
         * If the list is huge, iterate over it twice so we don't need to
         * allocate heap memory to duplicate it.
         */
        unsigned        vals[4096/sizeof(unsigned)];  /* give it 4K of stack */
        ERL_NIF_TERM    head, tail;

        if (num > (sizeof(vals)/sizeof(*vals)))
        {
            for (head = argv[1];
                    enif_get_list_cell(env, head, &head, &tail); head = tail)
                if (! enif_get_uint(env, head, &num))
                    return  enif_make_badarg(env);
            /* we know they're all suitable bits now, just add them */
            for (head = argv[1];
                    enif_get_list_cell(env, head, &head, &tail); head = tail)
            {
                enif_get_uint(env, head, &num);
                roaring_bitmap_add(res->bits, num);
            }
        }
        else
        {
            unsigned    x = 0;
            for (head = argv[1];
                    enif_get_list_cell(env, head, &head, &tail); head = tail)
                if (! enif_get_uint(env, head, (vals + x++)))
                    return  enif_make_badarg(env);
            /* paranoid maybe */
            if (x != num)
                return  enif_make_tuple2(env, ATOM_ERROR,
                            enif_make_tuple2(env, ATOM_INTERNAL, ATOM_BADMATCH));
            for (x = 0; x < num; ++x)
                roaring_bitmap_add(res->bits, vals[x]);
        }
    }
    else if (enif_get_uint(env, argv[1], &num))
        roaring_bitmap_add(res->bits, num);
    else
        return  enif_make_badarg(env);

    return  *argv;
}

/*
 * eroaring:serialize(eroaring:bits()) -> binary()
 * Returns an serialized representation of the input resource.
 */
static ERL_NIF_TERM
eroaring_serialize(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    eroaring_t *    res;
    size_t          size;
    ErlNifBinary    bin;

    if (argc != 1 || (res = get_term_resource(env, *argv)) == NULL)
        return  enif_make_badarg(env);

    size = roaring_bitmap_portable_size_in_bytes(res->bits);
    if (! enif_alloc_binary(size, &bin))
        return  enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);

    if (roaring_bitmap_portable_serialize(res->bits, (char *) bin.data) != size)
        return  enif_make_tuple2(env, ATOM_ERROR,
                    enif_make_tuple2(env, ATOM_INTERNAL, ATOM_BADMATCH));

    return  enif_make_binary(env, &bin);
}

/*
 * eroaring:deserialize(binary()) -> eroaring:bits()
 * Returns a resource de-serialized from a binary.
 */
static ERL_NIF_TERM
eroaring_deserialize(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    roaring_bitmap_t *  bits;
    eroaring_t *        res;
    ErlNifBinary        bin;

    if (argc != 1 || !enif_inspect_binary(env, *argv, &bin))
        return  enif_make_badarg(env);

    bits = roaring_bitmap_portable_deserialize((const char *) bin.data);
    if (bits == NULL)
        return  enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);

    if ((res = resource_ctor(bits)) == NULL)
    {
        roaring_bitmap_free(bits);
        return  enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
    }

    return  resource_to_term(env, res);
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
    {"new", 1, eroaring_new},
    {"new", 2, eroaring_new},
    {"run_optimize", 1, eroaring_run_optimize},
    {"add", 2, eroaring_add},
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
        init_atom(env, "internal", &ATOM_INTERNAL);
        init_atom(env, "badmatch", &ATOM_BADMATCH);
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

