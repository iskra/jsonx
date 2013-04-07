// Copyright 2013 Yuriy Iskra <iskra.yw@gmail.com>

#include <string.h>
#include "jsonx.h"

static void
enc_rt_dtor(ErlNifEnv* env, void* obj){
  EncEntry *entry = (EncEntry*)obj;
  enif_release_binary(&entry->bin);
  entry->bin.data = NULL;
  entry = NULL;
}

static int
load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info){

  PrivData *pdata = enif_alloc(sizeof(PrivData));
  if(pdata == NULL) return 1;

  pdata->encoder_RSTYPE = enif_open_resource_type(env, NULL,
					   "encoder_RSTYPE",
					   enc_rt_dtor,
					   ERL_NIF_RT_CREATE, NULL);
  if (pdata->encoder_RSTYPE == NULL) return -1;

  pdata->decoder_RSTYPE = enif_open_resource_type(env, NULL,
					   "decoder_RSTYPE",
					   NULL,
					   ERL_NIF_RT_CREATE, NULL);
  if (pdata->decoder_RSTYPE == NULL) return -1;

  if(!enif_make_existing_atom(env, "true",     &(pdata->am_true),     ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "false",    &(pdata->am_false),    ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "null",     &(pdata->am_null),     ERL_NIF_LATIN1)) return 1;

  if(!enif_make_existing_atom(env, "error",          &(pdata->am_error),   ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "big_num",        &(pdata->am_erange),  ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "invalid_string", &(pdata->am_estr),    ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "invalid_json",   &(pdata->am_esyntax), ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "trailing_data",  &(pdata->am_etrailing),ERL_NIF_LATIN1)) return 1;

  if(!enif_make_existing_atom(env, "json",     &(pdata->am_json),     ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "struct",   &(pdata->am_struct),   ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "proplist", &(pdata->am_proplist), ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "eep18",    &(pdata->am_eep18),    ERL_NIF_LATIN1)) return 1;
  if(!enif_make_existing_atom(env, "no_match", &(pdata->am_no_match), ERL_NIF_LATIN1)) return 1;

  *priv_data = (void*)pdata;

  return 0;
}

static int
reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info){
  return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info){
  return 0;
}
static void
unload(ErlNifEnv* env, void* priv_data){
  return;
}

ERL_NIF_TERM
make_encoder_resource_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  unsigned rs_len, fs_len, bin_sz;
  enif_get_uint(env, argv[0], &rs_len);
  enif_get_uint(env, argv[1], &fs_len);
  enif_get_uint(env, argv[4], &bin_sz);
  PrivData* priv = (PrivData*)enif_priv_data(env);
  unsigned resource_sz = enc_resource_size(rs_len, fs_len);
  EncEntry *entry_rs = (EncEntry*)enif_alloc_resource(priv->encoder_RSTYPE, resource_sz); 
  //memset(entry_rs, 0, resource_sz);
  entry_rs->records_cnt = rs_len;
  entry_rs->fields_cnt = fs_len;
  if(!enif_alloc_binary(bin_sz + 1, &entry_rs->bin))
    goto error;
  //memset(entry_rs->bin.data, 0, bin_sz + 1);
  ErlNifBinary ebin;
  enif_inspect_binary(env, argv[5], &ebin);
  memcpy(entry_rs->bin.data, ebin.data , ebin.size);

  ERL_NIF_TERM list, head, tail;
  list = argv[2];
  int i = 0;
  while(enif_get_list_cell(env, list, &head, &tail)){
    const ERL_NIF_TERM *tuple;
    int arity;
    unsigned ip;
    enif_get_tuple(env, head, &arity, &tuple);
    EncRecord *records = enc_records_base(entry_rs);
    records[i].tag = tuple[0];
    enif_get_uint(env, tuple[1], &ip);
    records[i].fds_offset = ip;
    enif_get_uint(env, tuple[2], &ip);
    records[i].arity = ip;
    i++;
    list = tail;
  }
  list = argv[3];
  i = 0;
  while(enif_get_list_cell(env, list, &head, &tail)){
    const ERL_NIF_TERM *tuple;
    int arity;
    unsigned ip;
    enif_get_tuple(env, head, &arity, &tuple);
    EncField *fields = enc_fields_base(entry_rs);
    enif_get_uint(env, tuple[0], &ip);
    fields[i].offset = ip;
    enif_get_uint(env, tuple[1], &ip);
    fields[i].size = ip;
    i++;
    list = tail;
  }
  ERL_NIF_TERM ret = enif_make_resource(env, (void *)entry_rs);
  enif_release_resource(entry_rs);
  return ret;
 error:
  enif_release_resource(entry_rs);
  return enif_make_badarg(env);
}



static ERL_NIF_TERM
dec_resource_to_term(ErlNifEnv* env, DecEntry *dec_entry){
  ERL_NIF_TERM *ukeys = ukeys_base(dec_entry);
  unsigned     *keys  = keys_base(dec_entry, dec_entry->ukeys_cnt);
  DecRecord    *records = records_base(dec_entry, dec_entry->ukeys_cnt, dec_entry->keys_cnt);
  int i;

  ERL_NIF_TERM keys_term = enif_make_list(env,0);
  for(i = dec_entry->keys_cnt - 1; i >= 0 ; i--){
    keys_term = enif_make_list_cell(env, enif_make_uint(env, keys[i]), keys_term);
  }

  ERL_NIF_TERM records_term = enif_make_list(env,0);
  for(i = dec_entry->records_cnt - 1; i >= 0 ; i--){
    ERL_NIF_TERM field = enif_make_tuple3(env,
					  records[i].tag,
					  enif_make_uint(env, records[i].keys_off),
					  enif_make_uint(env, records[i].arity)
					  );
    records_term = enif_make_list_cell(env, field, records_term);
  }

  ERL_NIF_TERM  bit_masks_term = enif_make_list(env,0);
  long unsigned *bit_mask = bit_mask_base(dec_entry, dec_entry->ukeys_cnt, dec_entry->keys_cnt, dec_entry->records_cnt);
  size_t        bit_mask_len = BITS_TO_WORDS(dec_entry->ukeys_cnt);
  int k;
  for(i = dec_entry->records_cnt - 1; i >= 0 ; i--){
    ERL_NIF_TERM  mask_term = enif_make_list(env, 0);
    for(k = bit_mask_len - 1; k >= 0; k--){
      mask_term = enif_make_list_cell(env, enif_make_uint(env, bit_mask[i * bit_mask_len + k]), mask_term);
    }
    bit_masks_term = enif_make_list_cell(env, mask_term, bit_masks_term);
  }
  return enif_make_tuple7(env
			  ,enif_make_uint(env, dec_entry->records_cnt)
			  ,enif_make_uint(env, dec_entry->ukeys_cnt)
			  ,enif_make_uint(env, dec_entry->keys_cnt)
			  ,enif_make_list_from_array(env, ukeys, dec_entry->ukeys_cnt)
			  ,keys_term
			  ,records_term
			  ,bit_masks_term
			  );
}

ERL_NIF_TERM
make_decoder_resource_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  unsigned records_cnt, ukeys_cnt, keys_cnt;

  enif_get_uint(env, argv[0], &records_cnt);
  enif_get_uint(env, argv[1], &ukeys_cnt);
  enif_get_uint(env, argv[2], &keys_cnt);
  PrivData* priv = (PrivData*)enif_priv_data(env);
  unsigned resource_sz = dec_resource_size(records_cnt, ukeys_cnt, keys_cnt);
  DecEntry *entry_rs = (DecEntry*)enif_alloc_resource(priv->decoder_RSTYPE, resource_sz);

  entry_rs->records_cnt = records_cnt;
  entry_rs->ukeys_cnt = ukeys_cnt;
  entry_rs->keys_cnt = keys_cnt;

  ERL_NIF_TERM list, head, tail;
  int i;
  ERL_NIF_TERM* ukeys = ukeys_base(entry_rs);
  list = argv[3]; //UKeys
  for(i = 0; i < ukeys_cnt; i++){
    if(!enif_get_list_cell(env, list, &head, &tail))
      goto error;
    ukeys[i] = head;
    list = tail;
  }

  unsigned* keys = keys_base(entry_rs, ukeys_cnt);
  list = argv[4]; //KeyNums
  for(i = 0; i < keys_cnt; i++){
    if(!enif_get_list_cell(env, list, &head, &tail))
      goto error;
    if(!enif_get_uint(env, head, &keys[i]))
      goto error;
    list = tail;
  }
 
  DecRecord* records = records_base(entry_rs, ukeys_cnt, keys_cnt);
  long unsigned *bit_mask = bit_mask_base(entry_rs, ukeys_cnt, keys_cnt, records_cnt);
  size_t bit_mask_len = BITS_TO_WORDS(entry_rs->ukeys_cnt);
  memset((void *)bit_mask, 0, bit_mask_array_size(ukeys_cnt, records_cnt));
  const ERL_NIF_TERM *tuple;
  int arity, k;
  list = argv[5]; //Records
  for(i = 0; i < records_cnt; i++){
    if(!enif_get_list_cell(env, list, &head, &tail))
      goto error;
    if(!(enif_get_tuple(env, head, &arity, &tuple) && (arity == 3)))
      goto error;
    if(!enif_is_atom(env, tuple[0]))
      goto error;
    records[i].tag = tuple[0];
    if(!enif_get_uint(env, tuple[1], &records[i].keys_off))
      goto error;
    if(!enif_get_uint(env, tuple[2], &records[i].arity))
      goto error;
    list = tail;

    for(k = 0; k < records[i].arity; k++){
      int p = records[i].keys_off + k; //position in keys
      fprintf(stderr, "i: %d, k: %d, p: %d, keys[p]: %d\r\n", i, k, p, keys[p]);
      set_bit(keys[p], bit_mask + (i * bit_mask_len));
    }
  }

  ERL_NIF_TERM ret = enif_make_resource(env, (void *)entry_rs);
  enif_release_resource(entry_rs);
  return enif_make_tuple8(env, enif_make_atom(env, "decode_resource"), dec_resource_to_term(env, entry_rs),
 			  argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
  //return ret;
 error:
  enif_release_resource(entry_rs);
  return enif_make_badarg(env);
}

static ErlNifFunc
nif_funcs[] = {
  {"encode",     1, encode_nif},
  {"encode_res", 2, encode_nif}, // with resource
  {"decode_opt", 2, decode_nif}, // with options
  {"decode_res", 3, decode_nif}, // with options and resource
  {"make_encoder_resource", 6, make_encoder_resource_nif},
  {"make_decoder_resource", 6, make_decoder_resource_nif}
};

ERL_NIF_INIT(jsonx, nif_funcs, load, reload, upgrade, unload);

