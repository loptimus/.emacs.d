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

#ifndef __types_h_
#define __types_h_

extern "C" {
    #include <erl_nif.h>
}

#include <iostream>
#include <vector>
#include <map>
#include <list>
#include <set>
#include <queue>

#ifdef __OLD_SOLARIS__
#include <inttypes.h>
#else
#include <stdint.h>
#endif

#include "macros.h"

#include <dirent.h>
typedef struct dirent*                            dir_elem_t;

class backup;
class gnode;

typedef unsigned long                             erl_ptr_t;
typedef int8_t                                    op_code_t;
typedef uint8_t                                   atom_t;
typedef uint8_t                                   data_t;
typedef uint32_t                                  data_size_t;
typedef uint32_t                                  gnode_id_t;
#ifndef __OLD_SOLARIS__
typedef int32_t                                   index_t;
#else
#define index_t int32_t
#endif
typedef uint32_t                                  backup_num_t;
typedef uint32_t                                  size_type; // size_t already reserved

typedef std::vector<backup>                       backups_t;
typedef std::vector<std::string>                  strings_t;
typedef std::pair<std::string, gnode_id_t>        simple_link_t;
typedef std::vector<gnode_id_t>                   gnode_ids_t;
typedef std::vector<simple_link_t>                simple_links_t;
typedef std::pair<atom_t, gnode_ids_t>            gnode_link_t;
typedef std::map<atom_t, gnode_ids_t>             gnode_links_t;
typedef std::map<std::string, atom_t>             erl_atom2atom_t;
typedef strings_t                                 atom2erl_atom_t;
typedef std::vector<gnode*>                       gnodes_t;
typedef std::priority_queue<gnode_id_t,
                    std::vector<gnode_id_t>,
                    std::greater<gnode_id_t> >    free_indexes_t;
typedef gnode_ids_t                               prot_nodes_t;
typedef std::pair<gnode_id_t, gnode_links_t>      prot_link_t;
typedef std::map<gnode_id_t, gnode_links_t>       prot_links_t;
typedef std::list<gnode_id_t>                     garbage_container_t;

#endif
