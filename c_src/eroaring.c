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

    if ((bits = roaring_bitmap_copy(res_in->bits)) == NULL)
        return  enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);

    /* does anyone care about the result? */
    roaring_bitmap_run_optimize(bits);

    if ((res_out = resource_ctor(bits)) == NULL)
        return  enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);

    return  resource_to_term(env, res_out);
}


/* Library and resource initialization, never called directly */

static ErlNifFunc nif_funcs[] = {
    {"new", 1, eroaring_new},
    {"run_optimize", 1, eroaring_run_optimize}
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

