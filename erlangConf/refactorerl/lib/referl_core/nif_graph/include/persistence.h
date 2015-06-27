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

#ifndef __persistence_h_
#define __persistence_h_

#include <string>

#include "backup.h"
#include "types.h"

/** Implements the backup-system.
 *  In the backup-system a backup looks like this: backup.A, where A is
 *  an unsigned integer value, and that is called the checkpoint number.
 *  For instance, the first backup method call will create backup.1,
 *  and the second will create backup.2, etc.
 */
class persistence
{
public:

    persistence();

    
    //-------------------------------------------------------------------------
    // Backup operations

    /** Creates a checkpoint from the actual graph.
     *
     *  @return Absolute path of the created checkpoint/file.
     */
    std::string create_backup();

    /** Creates a backup from the actual graph.
     *
     *  @param commit_log The commit log of the checkpoint.
     *  @return Absolute path of the created checkpoint/file.
     */
    std::string create_backup(std::string commit_log);

    /** Saves the actual graph.
     *
     *  @param _state The state object, that contains the path where
     *                the saved graph will be.
     */
    void save(const state& _state);

    /** Restores the given state.
     *
     *  @param _state A state object, that contains
     *                the path of the graph to be restored.
     */
    void restore(const state& _state);

    /** Deletes all backups. */
    void delete_all_backups();


    //-------------------------------------------------------------------------
    // Other operations

    /** Swaps the current graph to another.
     *
     *  @param grap_name The name of the graph to be loaded.
     */
    void swap_graph(const std::string& graph_name);


    //-------------------------------------------------------------------------
    // Exceptions

    /** Thrown when a file does not ends with the proper magic symbol. */
    DEFINE_EXCEPTION(graph_load_fail, "Error occured while loading the graph")

private:

    //-------------------------------------------------------------------------
    // Properties

    /** The max checkpoint number of the backups.
     *  It's always determined, when a graph has been loaded. */
    backup_num_t max_checkpoint_num;


    //-------------------------------------------------------------------------
    // Constants

    static std::string  LAST_USED_BACKUP_FILE();


    //-------------------------------------------------------------------------
    // Read/Write operations

    /** Writes out the given state's name to a text file,
     *  if the state's name is not an empty string.
     *  This function always used to write out the last used state or
     *  backup file to the hard drive, so we can load that state or backup
     *  later.
     */
    void write_last_backup_features(const state& _state);

    /** Reads the last used state or backup name, that has been written
     *  before with the write_last_backup_features function.
     *
     *  @param _state The state object into which the function reads
     *                the last used state or backup name.
     */
    void read_last_backup_features(state& _state);

    /** Determines the max checkpoint number of the backups,
     *  in the current graph folder.
     */
    backup_num_t determine_max_checkpoint_num();

    //-------------------------------------------------------------------------
    // Other private operations

    /** Initializes the private variables. */
    inline void init_vars();

    /** The save and restore methods use this function. */
    inline void finishing_up(const state& _state);

    /** Loads the last used graph.
     *  If last used graph does not exist, then
     *  creates a graph named "refactorerl", and loads it.
     */
    void load_last_used_graph();
    
};

#endif
