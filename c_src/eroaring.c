#include "erl_nif.h"
#include "roaring.c"

typedef struct {
  roaring_bitmap_t *r;
} roaring;

ErlNifResourceType* ROARING;

// There are four functions that may be called during the lifetime
// of a NIF. load, reload, upgrade, and unload. Any of these functions
// can be left unspecified by passing NULL to the ERL_NIF_INIT macro.
//
// NIFs are awesome.

// Return value of 0 indicates success.
// Docs: http://erlang.org/doc/man/erl_nif.html#load

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  const char* mod = "eroaring";
  const char* name = "eroaring";
  int flags = ERL_NIF_RT_CREATE;

  // instantiate the BITARRAY resource type
  ROARING = enif_open_resource_type(
                                     env,
                                     mod,
                                     name,
                                     free_roaring,
                                     flags,
                                     NULL);
  if (ROARING == NULL) return -1;

    return 0;
}

// Called when changing versions of the C code for a module's NIF
// implementation if I read the docs correctly.
//
// Return value of 0 indicates success.
// Docs: http://erlang.org/doc/man/erl_nif.html#upgrade

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info)
{
    return 0;
}

// Called when the library is unloaded. Not called after a reload
// executes.
//
// No return value
// Docs: http://erlang.org/doc/man/erl_nif.html#load

static void
unload(ErlNifEnv* env, void* priv)
{
    return;
}

// The actual C implementation of an Erlang function.
//
// Docs: http://erlang.org/doc/man/erl_nif.html#ErlNifFunc

static ERL_NIF_TERM
new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    roaring_bitmap_t *r1 = roaring_bitmap_create();
    return enif_make_badarg(env);
}

static ErlNifFunc nif_funcs[] = {
    {"new", 1, new}
};

// Initialize this NIF library.
//
// Args: (MODULE, ErlNifFunc funcs[], load, reload, upgrade, unload)
// Docs: http://erlang.org/doc/man/erl_nif.html#ERL_NIF_INIT

ERL_NIF_INIT(eroaring, nif_funcs, &load, NULL, &upgrade, &unload);

// Or if you don't need reload, upgrade, or unload.
// ERL_NIF_INIT(eroaring, nif_funcs, &load, NULL, NULL, NULL);

