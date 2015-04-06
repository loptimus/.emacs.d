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
 * The Initial Developer of the  Original Code is E�tv�s Lor�nd University.
 * Portions created  by E�tv�s Lor�nd University are  Copyright 2008-2009,
 * E�tv�s Lor�nd University. All Rights Reserved.
 *
 * @author Peter Felker <felker.peter88@gmail.com>
 * 
 */

#include <iostream>
#include <string>
#include <vector>

#include "directory.h"
#include "conversions.h"
#include "graph.h"
#include "types.h"
#include "persistence.h"
#include "nif_load.h"
#include "globals.h"

using namespace std;
extern graph* globals::ngraph;


extern "C"
{
    int reinit_globals(ErlNifEnv* env, ERL_NIF_TERM load_info) {
        ERL_NIF_TERM head;
        ERL_NIF_TERM tail;

        enif_get_list_cell(env, load_info, &head, &tail);
        erl_ptr_t graph_ptr = term2globalptr(env, head);

        load_info = tail;
        enif_get_list_cell(env, load_info, &head, &tail);
        erl_ptr_t persistence_ptr = term2globalptr(env, head);
        
        // get mnesia:system_info(directory) parameter
        load_info = tail;
        enif_get_list_cell(env, load_info, &head, &tail);
        string graphs_dir_root = erl_str2str(env, head);
        
        directory d(graphs_dir_root);
        if(!d.exist(GRAPHS_DIR_NAME)) {
            d.make_dir(GRAPHS_DIR_NAME);
        }
        
        char dir_separator  = directory::separator();
        globals::graphs_dir = graphs_dir_root + dir_separator +
                              GRAPHS_DIR_NAME + dir_separator;

        if (graph_ptr == 0) {
            if(persistence_ptr != 0) {
                delete globals::backup_system;
            }

            // The constructor of the persistence class
            // initializes globals::ngraph.
            try {
                globals::backup_system = new persistence();
            } catch(...) {
                return -1;
            }
        } else {
             globals::ngraph = reinterpret_cast<graph*>(graph_ptr);
             globals::backup_system =
                                reinterpret_cast<persistence*>(persistence_ptr);
        }

        return 0;
    }

    int load(ErlNifEnv* env, void**, ERL_NIF_TERM load_info) {
        return reinit_globals(env, load_info);
    }

    int reload(ErlNifEnv* env, void**, ERL_NIF_TERM load_info) {
        return reinit_globals(env, load_info);
    }

    int upgrade(ErlNifEnv* env, void**, void**, ERL_NIF_TERM load_info) {
        return reinit_globals(env, load_info);
    }

    void unload(ErlNifEnv*, void*) {
        if(globals::backup_system != NULL) {
            delete globals::backup_system;
            globals::backup_system = NULL;
        }
		
        if(globals::ngraph != NULL) {
            delete globals::ngraph;
            globals::ngraph = NULL;
        }
    }

    ERL_NIF_TERM nif_get_datastore(ErlNifEnv* env, int, const ERL_NIF_TERM[]) {
        return enif_make_list2(env, globalptr2term(env, globals::ngraph),
                                    globalptr2term(env, globals::backup_system));
    }

}
