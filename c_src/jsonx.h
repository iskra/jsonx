// Copyright 2013 Yuriy Iskra <iskra.yw@gmail.com>

#include "erl_nif.h"

typedef struct{
  ERL_NIF_TERM am_true;
  ERL_NIF_TERM am_false;
  ERL_NIF_TERM am_null;

  ERL_NIF_TERM am_error;
  ERL_NIF_TERM am_erange;
  ERL_NIF_TERM am_estr;
  ERL_NIF_TERM am_esyntax;
  ERL_NIF_TERM am_etrailing;

  ERL_NIF_TERM am_json;
  ERL_NIF_TERM am_struct;
  ERL_NIF_TERM am_proplist;
  ERL_NIF_TERM am_eep18;
  ERL_NIF_TERM am_no_match;

  ErlNifResourceType* records_RSTYPE;
}PrivData;


ERL_NIF_TERM
decode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
encode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

