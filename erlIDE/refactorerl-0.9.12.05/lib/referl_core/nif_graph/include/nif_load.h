/** -*- coding: latin-1 -*-
 * 
 * The  contents of this  file are  subject to  the Erlang  Public License,
 * Version  1.1, (the  "License");  you may  not  use this  file except  in
 * compliance  with the License.  You should  have received  a copy  of the
 * Erlang  Public License  along  with this  software.  If not,  it can  be
 * retrieved at http://plc.inf.elte.hu/erlang/
 *
 * Software  distributed under  the License  is distributed  on an  "AS IS"
 * basis, WITHOUT  WARRANTY OF ANY  KIND, either expressed or  implied. See
 * the License  for the specific language governing  rights and limitations
 * under the License.
 *
 * The Original Code is RefactorErl.
 *
 * The Initial Developer of the  Original Code is Eötvös Loránd University.
 * Portions created  by Eötvös Loránd University are  Copyright 2008-2009,
 * Eötvös Loránd University. All Rights Reserved.
 *
 * @author Peter Felker <felker.peter88@gmail.com>
 * 
 */

#ifndef __nif_load_h_
#define __nif_load_h_

extern "C"
{
    #include "erl_nif.h"

    int reinit_globals(ErlNifEnv* env, ERL_NIF_TERM load_info);
    int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
    int reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
    int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
    void unload(ErlNifEnv* env, void* priv_data);
    ERL_NIF_TERM nif_get_datastore(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
}

#endif
